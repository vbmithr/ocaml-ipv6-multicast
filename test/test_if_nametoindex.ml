let () =
  if Array.length Sys.argv < 2
  then begin
    Printf.fprintf stderr "Usage: %s <iface>\n" Sys.argv.(0);
    exit 1
  end
  else
  Printf.printf "%s -> %d\n%!"
    Sys.argv.(1)
    (Ipv6_multicast.if_nametoindex Sys.argv.(1))
