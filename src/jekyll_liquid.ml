(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
module JF = Jekyll_format

module Tag_parser = struct
  type lines = String.Sub.t list
  type highlight = {
    lang: string option;
    body: lines;
    linenos: bool;
  }

  let pp_highlight ppf {lang; body; linenos} =
    let open Fmt in
    pf ppf "lang: %a linenos: %a@,body:@,%a"
      (option string) lang (list ~sep:(unit "\n") String.Sub.pp) body bool linenos

  let mk_highlight ?lang ?(body=[]) ?(linenos=false) () =
    { lang; body; linenos }

  let extract_tag ?(start=0) ~start_tag ~stop_tag s =
    match String.find_sub ~start ~sub:start_tag s with
    |None -> None
    |Some start_idx ->
      let start_data_idx = String.length start_tag + start_idx in
      match String.find_sub ~start:start_data_idx ~sub:stop_tag s with
      |None -> None
      |Some end_data_idx -> begin
         let end_idx = String.length stop_tag + end_data_idx in
         String.with_index_range ~first:start_data_idx ~last:(end_data_idx-1) s |> fun tag ->
         String.trim tag |> function
         | "" -> None
         | tag_data -> Some (start_idx, tag_data, end_idx)
    end

  let extract_tags ?(start=0) ~start_tag ~stop_tag s =
    let rec find_one acc start =
      if start >= String.length s then List.rev acc else
      match extract_tag ~start ~start_tag ~stop_tag s with
      |None -> List.rev acc
      |Some (start_idx,tag_data,end_idx) ->
        let acc = (start_idx, tag_data, end_idx) :: acc in
        find_one acc end_idx
    in find_one [] start

  let extract_liquid_tag ?(start=0) s =
    extract_tag ~start ~start_tag:"{%" ~stop_tag:"%}" s

  let extract_liquid_tags ?(start=0) s =
    extract_tags ~start ~start_tag:"{%" ~stop_tag:"%}" s

  let highlight tag_data =
    String.cuts ~empty:false ~sep:" " tag_data |> function
    | ["highlight"] -> Some (mk_highlight ())
    | ["highlight";lang] -> Some (mk_highlight ~lang ())
    | ["highlight";lang;"linenos"] -> Some (mk_highlight ~lang ~linenos:true ())
    | _ -> None

  let endhighlight tag_data =
    tag_data = "endhighlight"

end

let highlight_exn ?(start=0) fn body =
  Tag_parser.extract_liquid_tags body |> fun tags ->
  String.Sub.v ~start body |> fun body ->
  let rec parse_tag_pairs acc body curpos = function
    | (start1,tag1,end1)::(start2,tag2,end2)::tl -> begin
        match Tag_parser.highlight tag1 with
        |Some h -> begin
          match Tag_parser.endhighlight tag2 with
          |true ->
             let new_body =
               String.Sub.with_index_range ~first:end1 ~last:(start2-1) body |>
               String.Sub.trim |>
               String.Sub.to_string |> fn |> String.Sub.v in
             String.Sub.with_index_range ~first:curpos ~last:(start1-1) body |> fun start_sub ->
             String.Sub.with_index_range ~first:end2 body |> fun last_sub ->
             let acc = new_body::start_sub::acc in
             parse_tag_pairs acc body end2 tl 
          |false -> raise (Failure "highlight found without endhighlight tag")
        end
       |None -> ignore_tag acc body (start1,tag1,end1) ((start2,tag2,end2)::tl)
   end
   | [t1] -> ignore_tag acc body t1 []
   | [] ->
       let rest = String.Sub.with_index_range ~first:curpos body in
       rest :: acc
  and ignore_tag acc body (start1,tag1,end1) rest =
     String.Sub.with_index_range ~last:(end1-1) body |> fun bhd ->
     String.Sub.with_index_range ~first:end1 body |> fun thd ->
     let acc = bhd :: acc in
     parse_tag_pairs acc thd end1 rest
  in
  parse_tag_pairs [] body 0 tags |> fun acc ->
  List.iter (fun x -> Printf.printf "%d\n%!" (String.Sub.length x)) acc;
  String.Sub.concat (List.rev acc) |>
  String.Sub.to_string

let highlight_markdown_code s =
  "```\n"^s^"\n```"

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

