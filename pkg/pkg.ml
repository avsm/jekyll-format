#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "jekyll-format" @@ fun c ->
  Ok [ Pkg.mllib "src/jekyll_format.mllib";
       Pkg.test "test/test"; ]
