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
