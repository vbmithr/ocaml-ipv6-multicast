#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "ocb-stubblr.topkg"
open Topkg

let build = Pkg.build ~cmd:Ocb_stubblr_topkg.cmd ()

let () =
  Pkg.describe ~build "sockopt" @@ fun c ->
  Ok [ Pkg.mllib "src/sockopt.mllib" ;
       Pkg.clib "src/libsockopt.clib" ;
     ]
