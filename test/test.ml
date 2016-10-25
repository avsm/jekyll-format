(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Alcotest
(* This can be contributed back to Alcotest *)
let rresult_error =
  let module M = struct
    type t = Rresult.R.msg
    let pp = Rresult.R.pp_msg
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

open Rresult

module JF = Jekyll_format

let success_posts = [
  "simple.md", (Ok ());
  "no_start_yaml.md", (R.error_msg JF.E.yaml_no_start);
  "no_end_yaml.md", (R.error_msg JF.E.yaml_no_end);
  "field_parse.md", (R.error_msg (JF.E.yaml_field_parse "alice"))
]

let test_post ~expect ~base_dir ~post () =
  let test_parse =
    let open Rresult.R.Infix in
    Bos.OS.File.read (Fpath.(base_dir / post)) >>=
    Jekyll_format.of_string >>= fun t ->
    Fmt.pr "%a" JF.pp t;
    R.ok ()
  in
  check (result unit rresult_error) post expect test_parse

let test_set ~base_dir () =
  List.map (fun (post, expect) ->
    post, `Quick, (test_post ~expect ~base_dir ~post)
  ) success_posts

let () =
  let base_dir = Fpath.v "test/_posts" in
  Alcotest.run "post parsing" [
    "markdown", test_set ~base_dir ()
  ]

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
