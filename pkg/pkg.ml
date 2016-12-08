#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "ocb-stubblr.topkg"
open Topkg

let build = Pkg.build ~cmd:Ocb_stubblr_topkg.cmd ()

let () =
  Pkg.describe ~build "ipv6-multicast" @@ fun c ->
  Ok [ Pkg.mllib "src/ipv6_multicast.mllib" ;
       Pkg.clib "src/libipv6_multicast.clib" ;
     ]
