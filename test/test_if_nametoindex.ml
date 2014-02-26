open Sockopt

let () =
  if Array.length Sys.argv < 2
  then
    (
      Printf.fprintf stderr "Usage: %s <iface>\n" Sys.argv.(0);
      exit 1
    )
  else
    Printf.printf "%s -> %d\n%!" Sys.argv.(1) (if_nametoindex Sys.argv.(1))
