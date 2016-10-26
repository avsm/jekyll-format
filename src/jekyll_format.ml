(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

module E = struct
  let yaml_no_start = "Unable to find YAML front matter start ---"
  let yaml_no_end = "Unable to find YAML front matter terminating ---"
  let yaml_field_parse s = Fmt.strf "Unable to parse YAML field: %s" s
end

type fields = (String.Sub.t * String.Sub.t) list
type body = String.Sub.t list
type t = fields * body

let of_string t =
  let open R.Infix in
  let lines = String.Sub.(cuts ~sep:(v "\n") (v t)) in
  let is_yaml_delimiter s = String.Sub.to_string s = "---" in
  let rec get_yaml acc = function
  | [] -> R.error_msg E.yaml_no_end
  | hd::tl when String.Sub.length hd = 0 -> R.error_msg E.yaml_no_end
  | hd::tl when is_yaml_delimiter hd -> R.ok (List.rev acc, tl)
  | hd::tl ->
     String.Sub.(cut ~sep:(v ":") hd) |> function
     | None -> R.error_msg (E.yaml_field_parse (String.Sub.to_string hd))
     | Some (k,v) -> get_yaml ((k,String.Sub.trim v) :: acc) tl in
  match lines with
  | [] -> R.error_msg E.yaml_no_start
  | hd::_ when not (is_yaml_delimiter hd) -> R.error_msg E.yaml_no_start
  | hd::tl -> get_yaml [] tl

exception Parse_failure of string
let of_string_exn t =
  match of_string t with
  | Ok r -> r
  | Error (`Msg m) -> raise (Parse_failure m)

let fields = fst
let body = snd
let body_to_string b = String.Sub.(concat ~sep:(v "\n") b |> to_string)

let find key (f:fields) =
  try
    List.find (fun (k,_) -> String.equal (String.Sub.to_string k) key) f |>
    snd |> String.Sub.to_string |> fun x -> Some x
  with Not_found -> None

let keys f =
  List.map (fun (k,v) -> String.Sub.to_string k) f

let pp_body ppf body =
  let open Fmt in
  let pp_list = list ~sep:Format.pp_force_newline String.Sub.pp in
  pf ppf "%a" pp_list body

let pp_fields ppf fields =
  let open Fmt in
  let pp_colon = unit ":" in
  let pp_field = pair ~sep:pp_colon String.Sub.pp String.Sub.pp in
  let pp_fields = list ~sep:Format.pp_force_newline pp_field in
  pf ppf "%a" pp_fields fields

let pp ppf t =
  let open Fmt in
  pf ppf "\n---\n";
  pf ppf "%a" (pair ~sep:(unit "\n---\n") pp_fields pp_body) t

let parse_date_exn ?(and_time=true) s =
  let ymd,hms,tz =
    match and_time with
    |true ->
      (match String.cuts ~sep:" " s with
      |[ymd] -> (Some ymd), None, None
      |[ymd;hms] -> (Some ymd), (Some hms), None
      |ymd::hms::tz::_ -> (Some ymd), (Some hms), (Some tz)
      |[] -> None, None, None)
    |false -> Some s, None, None
  in
  let dfail s = raise (Parse_failure s) in
  match ymd with
  | None -> dfail "No valid year/month/date found"
  | Some ymd ->
      let to_int l s =
        try int_of_string s
        with _ -> dfail (l ^ " component is not a valid integer") in
      let date =
        match String.cuts ~sep:"-" ymd with
        |[y;m;d] -> (to_int "year" y), (to_int "month" m), (to_int "date" d)
        |[y;m] -> dfail "No date component found"
        |[y] -> dfail "No month or date component found"
        |[] -> dfail "Empty date component"
        |_ -> dfail "Date component must be in form YYYY-MM-DD" in
      let time =
        match hms with
        | None -> 0,0,0
        | Some hms ->
            match String.cuts ~sep:":" hms with
            |[h;m;s] -> (to_int "hour" h), (to_int "minute" m), (to_int "seconds" s)
            |[h;m] -> (to_int "hour" h), (to_int "minute" m), 0
            |[h] -> (to_int "hour" h), 0, 0
            |[] -> dfail "Empty time component"
            |_ -> dfail "Time component must be in form HH:MM:SS" in
      let tz = (* can be [+-]HH[:]MM *)
        match tz with
        | None -> 0
        | Some tz -> begin
           String.filter (function |'0'..'9'|'+'|'-' -> true |_->false) tz |> fun tz ->
           let off,hh,mn =
             match String.length tz with
             | 5 ->
                let hh = String.with_range ~first:1 ~len:2 tz |> to_int "timezone hour" in
                let mn = String.with_range ~first:3 ~len:2 tz |> to_int "timezone min" in
                let off = match String.get_head tz with
                 | '+' -> 1 | '-' -> (-1) | _ -> dfail "Invalid timezone direction" in
                off,hh,mn
             | 4 ->
                let hh = String.with_range ~first:0 ~len:2 tz |> to_int "timezone hour" in
                let mn = String.with_range ~first:2 ~len:2 tz |> to_int "timezone min" in
                let off = 1 in
                off,hh,mn
             | _ -> dfail ("Unable to parse timezone " ^ tz)
           in
           ((hh * 3600) + (mn * 1800)) * off
        end
      in
      Ptime.of_date_time (date, (time,tz)) |> function
      | None -> dfail "Invalid date/time"
      | Some p -> p

let parse_date ?(and_time=true) s =
  try R.ok (parse_date_exn ~and_time s) with Parse_failure m -> R.error_msg m

let parse_filename s =
  let open R.Infix in
  let dashc = String.concat ~sep:"-" in
  match String.cuts ~sep:"-" s with
  | y::m::d::title -> begin
     dashc title |> function
     | "" -> R.error_msg "Empty title not allowed"
     | title ->
        Fpath.v title |>
        Fpath.split_ext |> fun (title, ext) ->
        String.drop ~max:1 ext |> fun ext ->
        Fpath.to_string title |> fun title ->
        parse_date ~and_time:false (dashc [y;m;d]) >>| fun time ->
       (time, title, ext)
  end
  | _ -> R.error_msg "Unable to find a date component in filename"
 
let parse_filename_exn s =
  match parse_filename s with
  | Ok r -> r
  | Error (`Msg m) -> raise (Parse_failure m)

let title ?fname f =
  match find "title" f with
  | Some t -> Some t
  | None ->
      match fname with
      | None -> None
      | Some fname ->
          match parse_filename fname with
          | Error _ -> None
          | Ok (_,title,_) -> Some title

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
