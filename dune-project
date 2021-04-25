(lang dune 2.8)
(name jekyll-format)

(generate_opam_files true)
(license MIT)
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(authors "Anil Madhavapeddy" "Patrick Ferris")
(source (github avsm/jekyll-format))

(package
 (name jekyll-format)
 (depends
  (ocaml (>= 4.03.0))
  result
  astring
  omd
  fmt
  rresult
  ptime
  fpath
  yaml
  ezjsonm
  (alcotest :with-test)
  (bos :with-test))
 (synopsis "Jekyll post parsing library")
 (description "This library provides an OCaml interface to parsing
posts in the Jekyll format."))


