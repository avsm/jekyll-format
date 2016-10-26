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

  let extract_tag ~start ~stop s =
    let t = String.Sub.to_string s in
    match String.find_sub ~sub:start t with
    | None -> None
    | Some idx ->
        let first = String.length start + idx in
        match String.find_sub ~rev:true ~sub:stop t with
        | None -> None
        | Some idx ->
            let last = idx - (String.length stop) in
            if last <= first then None else
            String.Sub.with_index_range ~first ~last s |> fun t ->
            Some (String.Sub.trim t)

  let highlight t =
    extract_tag ~start:"{%" ~stop:"%}" t |> function
    | None -> None
    | Some t ->
       String.Sub.(cuts ~empty:false ~sep:(v " ") t) |>
       List.map String.Sub.to_string |> function
       | ["highlight"] -> Some (mk_highlight ())
       | ["highlight";lang] -> Some (mk_highlight ~lang ())
       | ["highlight";lang;"linenos"] -> Some (mk_highlight ~lang ~linenos:true ())
       | _ -> None

  let endhighlight t =
    extract_tag ~start:"{%" ~stop:"%}" t |> function
    | None -> false
    | Some sub -> String.Sub.to_string sub = "endhighlight"

end

type line =
| Text of String.Sub.t
| Highlight of Tag_parser.highlight
 
let highlight_exn body =
  let rec find_start acc lines =
    match lines with
    |[] -> List.rev acc
    |line::tl ->
       match Tag_parser.highlight line with
       | None -> find_start (Text line :: acc) tl
       | Some h ->
          let rec find_end acc lines =
            match lines with
            |[] -> raise (JF.Parse_failure "Unable to find {% endhighlight %} tag")
            |line::tl ->
              match Tag_parser.endhighlight line with
              | false -> find_end (line::acc) tl
              | true ->
                  let body = List.rev acc in
                  let ent = Highlight (Tag_parser.mk_highlight ~body ()) in
                  ent, tl
          in
          let ent, tl = find_end [] tl in
          find_start (ent :: acc) tl
  in
  find_start [] body |>
  List.map (function
    | Text l -> [l]
    | Highlight {Tag_parser.lang;body} ->
       let delim = String.Sub.v "```" in
       delim :: body @ [delim]
  ) |>
  List.flatten

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

