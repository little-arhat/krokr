#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  let build = Pkg.build () in
  Pkg.describe "krokr" ~build
  @@ fun _c ->
     Ok [Pkg.mllib ~api:["Krokr"] "src/krokr.mllib";
         (* Pkg.test "test/test"; *)
     ]
