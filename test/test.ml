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

let astring_sub =
  let module M = struct
    type t = Astring.String.sub
    let pp = Astring.String.Sub.pp
    let equal = Astring.String.Sub.equal
  end in
  (module M: TESTABLE with type t = M.t)

let highlight_testable =
   let module M = struct
    type t = Jekyll_liquid.Tag_parser.highlight
    let pp = Jekyll_liquid.Tag_parser.pp_highlight
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

open Rresult
open Astring

module JF = Jekyll_format
module JL = Jekyll_liquid

let posts = [
  "basic", "simple.md", (Ok ());
  "basic", "no_start_yaml.md", (R.error_msg JF.E.yaml_no_start);
  "basic", "no_end_yaml.md", (R.error_msg JF.E.yaml_no_end);
  "basic", "field_parse.md", (R.error_msg (JF.E.yaml_field_parse "alice"));
  "anil.recoil.org", "2015-02-18-icfp15-call-for-sponsorships.md", (Ok ());
  "anil.recoil.org", "2015-04-02-ocamllabs-2014-review.md", (Ok ());
]

let parse_post ~base_dir ~post () =
  let open Rresult.R.Infix in
  Bos.OS.File.read (Fpath.(base_dir / post)) >>=
  Jekyll_format.of_string

let parse_post_exn ~base_dir ~post () =
  match parse_post ~base_dir ~post () with
  | Ok r -> r
  | Error (`Msg m) -> raise Alcotest.Test_error

let test_post ~expect ~base_dir ~post () =
  let test_parse =
    let open Rresult.R.Infix in
    parse_post ~base_dir ~post () >>= fun t ->
    Fmt.pr "%a" JF.pp t;
    R.ok () in
  check (result unit rresult_error) post expect test_parse

let test_find ~base_dir () =
  let open Rresult.R.Infix in
  parse_post_exn ~base_dir ~post:"simple.md" () |>
  JF.fields |> fun f ->
  check (option string) "find success" (Some "111") (JF.find "alice" f);
  check (option string) "find fail" None (JF.find "xalice" f);
  check (option string) "find fail case sensitive" None (JF.find "Alice" f);
  check (list string) "find keys" ["alice";"bob";"charlie"] (JF.keys f)

let test_body ~base_dir () =
  let open Rresult.R.Infix in
  parse_post_exn ~base_dir ~post:"simple.md" () |>
  JF.body |> fun b ->
  check string "body" "\nbody\ngoes\nhere\n" (JF.body_to_string b)

let test_parsing ~base_dir () =
  List.map (fun (subdir, post, expect) ->
    let base_dir = Fpath.(base_dir / subdir) in
    post, `Quick, (test_post ~expect ~base_dir ~post)
  ) posts

let test_meta ~base_dir () =
  let base_dir = Fpath.v "test/_posts/basic" in
  ["find", `Quick, test_find ~base_dir;
   "body", `Quick, test_body ~base_dir]

let test_tag_extraction () =
  let tags = [
    "{% highlight %}", (Some "highlight");
    "{% highlight foo %}", (Some "highlight foo");
    "%} {%", None;
    "{%%}", None;
    "{% %}", None;
    "{% f %}", (Some "f");
  ] in
  List.iter (fun (a,b) ->
    check (option string) "tag extract" b
    (JL.Tag_parser.extract_tag ~start:"{%" ~stop:"%}" (String.Sub.v a)
     |> function None -> None | Some x -> Some (String.Sub.to_string x))
  ) tags

let test_tag_highlight () =
  let module T = JL.Tag_parser in
  let tags = [
    "{% highlight %}", (Some {T.body=[]; lang=None; linenos=false});
    "{% highlight ocaml %}", Some {T.body=[]; lang=(Some "ocaml"); linenos=false};
    "{% highlight ocaml linenos %}", Some {T.body=[]; lang=(Some "ocaml"); linenos=true};
  ] in
  List.iter (fun (a,b) ->
    check (option highlight_testable) a b (T.highlight (String.Sub.v a))
  ) tags

let test_tag_endhighlight () =
  let module T = JL.Tag_parser in
  check bool "endhighlight ok" true (T.endhighlight (String.Sub.v "{% endhighlight %}"));
  check bool "endhighlight fail" false (T.endhighlight (String.Sub.v "{% xendhighlight %}"))

let test_delimit_highlight () =
  let module T = JL.Tag_parser in
  let s = String.Sub.(cuts ~sep:(v "\n") (v "{% highlight ocaml %}\nfoo\nbar\n{% endhighlight %}")) in
  let h = JL.highlight_exn s |> JF.body_to_string in
  check string "delimit highlight" "```\nfoo\nbar\n```" h

let test_tag_parsing () =
  ["tag", `Quick, test_tag_extraction;
   "tag highlight", `Quick, test_tag_highlight;
   "tag endhighlight", `Quick, test_tag_endhighlight;
   "tag delimit highlight", `Quick, test_delimit_highlight
  ]

let () =
  let base_dir = Fpath.v "test/_posts" in
  Alcotest.run "post parsing" [
    "parsing", test_parsing ~base_dir ();
    "meta", test_meta ~base_dir ();
    "tags", test_tag_parsing ()
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
