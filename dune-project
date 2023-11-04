(lang dune 2.9)
(name jekyll-format)
(generate_opam_files true)
(license MIT)
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(authors "Anil Madhavapeddy" "Patrick Ferris")
(source (github avsm/jekyll-format))
(package
 (name jekyll-format)
 (depends
  (ocaml (>= 4.08.0))
  (yaml-sexp (>= 3.0.0))
  (yaml (>= 3.0.0))
  sexplib
  astring
  (fmt (>="0.8.7"))
  rresult
  ptime
  fpath
  (ezjsonm (>= 1.1.0))
  (alcotest :with-test)
  (bos :with-test))
 (synopsis "Jekyll post parsing library")
 (description
  "This library provides an OCaml interface to parsing\nposts in the Jekyll format."))
