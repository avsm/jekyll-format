(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Alcotest
(* This can be contributed back to Alcotest *)
let rresult_msg =
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
  check (result unit rresult_msg) post expect test_parse

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
let test_tag_extraction () =
  let tags = [
    "{% highlight %}", (Some (0,"highlight",15));
    "{%  highlight %}", (Some (0,"highlight",16));
    "  {% highlight %}", (Some (2,"highlight",17));
    "{% highlight foo %}", (Some (0,"highlight foo",19));
    "  {% bar %}  ", (Some (2, "bar", 11));
    "  {% bar %} %}", (Some (2,"bar", 11));
    "{% f  %}", (Some (0,"f",8));
    "%} {%", None;
    "{%%}", None;
    "{% %}", None;
  ] in
  let testfn =
    Alcotest.testable
    (fun ppf -> function
       |None -> Fmt.pf ppf "%s" "None"
       |Some (a,b,c) -> Fmt.pf ppf "Some@.(%d,%S,%d)" a b c)
    (=)
  in
  List.iter (fun (a,b) ->
    check testfn a b
    (JL.Tag_parser.extract_tag ~start_tag:"{%" ~stop_tag:"%}" a)
  ) tags

let test_tag_highlight () =
  let module T = JL.Tag_parser in
  let fn a = T.extract_liquid_tag a |>
    function None -> None | Some (_,a,_) -> T.highlight a in
  let tags = [
    "{% highlight %}", (Some {T.body=[]; lang=None; linenos=false});
    "{% highlight ocaml %}", Some {T.body=[]; lang=(Some "ocaml"); linenos=false};
    "{% highlight ocaml linenos %}", Some {T.body=[]; lang=(Some "ocaml"); linenos=true};
  ] in
  List.iter (fun (a,b) ->
    check (option highlight_testable) a b (fn a)
  ) tags

let test_tag_endhighlight () =
  let module T = JL.Tag_parser in
  let fn a = T.extract_liquid_tag a |>
    function None -> false | Some (_,a,_) -> T.endhighlight a in
  check bool "endhighlight ok" true (fn "{% endhighlight %}");
  check bool "endhighlight fail" false (fn "{% xendhighlight %}")

let test_delimit_highlight () =
  let module T = JL.Tag_parser in
  let fn = JL.highlight_markdown_code in
  "{% highlight ocaml %}\nfoo\nbar\n{% endhighlight %}" |>
  JL.highlight_exn fn |>
  check string "delimit highlight" "```\nfoo\nbar\n```";
  "  {% highlight %}\nfoo\nbar\n{% endhighlight %}\nhello\n{% highlight %}\nbar\n{% endhighlight %}\nafter\nword" |>
  JL.highlight_exn fn |>
  check string "delimit highlight multiple" "  ```\nfoo\nbar\n```\nhello\n```\nbar\n```\nafter\nword"

let option_exn = function | None -> raise Test_error | Some e -> e

let test_filename_date () =
  let datev d = option_exn (Ptime.of_date d) in
  let fs = [
    "2011-03-05-foo-bar.md", Ok (datev (2011,03,05), "foo-bar", "md");
    "foo-bar.md", Error (`Msg "Unable to find a date component in filename");
    "2011-foo-bar.md", Error (`Msg "Empty title not allowed");
    "", Error (`Msg "Unable to find a date component in filename");
    "2011-99-99-foo.md", Error (`Msg "Invalid date/time");
  ] in
  let check_fn = Fmt.(testable (fun ppf (a,b,c) -> pf ppf "@,%a@.%a@.%a@," Ptime.pp a string b string c)) (=) in
  List.iter (fun (f,b) ->
    check (result check_fn rresult_msg) ("filename_date: "^f) b (JF.parse_filename f)
  ) fs

let test_datetime_parse () =
  let ptime_check = testable Ptime.pp Ptime.equal in
  let date d = Ptime.of_date d |> option_exn in
  let datetime d t = Ptime.of_date_time (d,(t,0)) |> option_exn in
  let datetimetz d t tz = Ptime.of_date_time (d,(t,tz)) |> option_exn in
  [ "2016-03-04", date (2016,03,04);
    "2016-04-05", date (2016,04,05);
    "2016-04-05 01:02:33", datetime (2016,04,05) (01,02,33);
    "2016-04-05 01:02:59 0100", datetimetz (2016,04,05) (01,02,59) 3600;
    "2016-04-05 01:02:59 +0200", datetimetz (2016,04,05) (01,02,59) 7200;
    "2016-04-05 01:02:59 -02:00", datetimetz (2016,04,05) (01,02,59) (-7200);
    "2016-04-05 01:02:59 -02::::00", datetimetz (2016,04,05) (01,02,59) (-7200);
  ] |>
  List.iter (fun (f,e) -> check ptime_check ("datetime " ^ f) e (JF.parse_date_exn f))

let test_slug () =
  [ "foo and bar", "foo-and-bar";
    "foo1 and_bar","foo1-and-bar";
    "  foo and bar  ","--foo-and-bar--" ] |>
  List.iter (fun (f,e) -> check string f e (JF.slug_of_string f))

let test_parsing ~base_dir () =
  List.map (fun (subdir, post, expect) ->
    let base_dir = Fpath.(base_dir / subdir) in
    post, `Quick, (test_post ~expect ~base_dir ~post)
  ) posts

let test_meta ~base_dir () =
  let base_dir = Fpath.v "test/_posts/basic" in
  ["find", `Quick, test_find ~base_dir;
   "body", `Quick, test_body ~base_dir;
   "slug", `Quick, test_slug]

let test_tag_parsing () =
  ["tag", `Quick, test_tag_extraction;
   "tag highlight", `Quick, test_tag_highlight;
   "tag endhighlight", `Quick, test_tag_endhighlight;
   "tag delimit highlight", `Quick, test_delimit_highlight
  ]

let test_date_parsing () =
  ["date", `Quick, test_filename_date;
   "datetime", `Quick, test_datetime_parse;]

let () =
  let base_dir = Fpath.v "test/_posts" in
  Alcotest.run "post parsing" [
    "parsing", test_parsing ~base_dir ();
    "meta", test_meta ~base_dir ();
    "tags", test_tag_parsing ();
    "date", test_date_parsing ();
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
