open Ipv6_multicast

let bind ?iface ?(flowinfo=0) ch sa =
  Lwt_unix.check_descriptor ch;
  U.bind ?iface ~flowinfo (Lwt_unix.unix_file_descr ch) sa

let connect ?iface ?(flowinfo=0) ch sa =
  let open Lwt_unix in
  let fd = unix_file_descr ch in
  if Sys.win32 then
    (* [in_progress] tell wether connection has started but not
       terminated: *)
    let in_progress = ref false in
    wrap_syscall Write ch begin fun () ->
      if !in_progress then
        (* Nothing works without this test and i have no idea why... *)
        if writable ch then
          try
            U.connect ?iface ~flowinfo fd sa
          with
          | Unix.Unix_error (EISCONN, _, _) ->
              (* This is the windows way of telling that the connection
                 has completed. *)
              ()
        else
        raise Retry
      else
      try
        U.connect ?iface ~flowinfo fd sa
      with
      | Unix.Unix_error (EWOULDBLOCK, _, _) ->
          in_progress := true;
          raise Retry
    end
  else
  (* [in_progress] tell wether connection has started but not
     terminated: *)
  let in_progress = ref false in
  wrap_syscall Write ch begin fun () ->
    if !in_progress then
      (* If the connection is in progress, [getsockopt_error] tells
         wether it succceed: *)
      match Unix.getsockopt_error fd with
      | None ->
          (* The socket is connected *)
          ()
      | Some err ->
          (* An error happened: *)
          raise (Unix.Unix_error(err, "connect", ""))
    else
    try
      (* We should pass only one time here, unless the system call
         is interrupted by a signal: *)
      U.connect ?iface ~flowinfo fd sa
    with
    | Unix.Unix_error (EINPROGRESS, _, _) ->
        in_progress := true;
        raise Retry
  end

  let send ch buf pos len flags =
    if pos < 0 || len < 0 || pos > Bytes.length buf - len then
      Lwt.fail_invalid_arg "send"
    else
    let fd = Lwt_unix.unix_file_descr ch in
    Lwt_unix.(wrap_syscall Write ch (fun () -> U.send fd buf pos len flags))

  let send_substring ch buf pos len flags =
    if pos < 0 || len < 0 || pos > String.length buf - len then
      Lwt.fail_invalid_arg "send_substring"
    else
    let fd = Lwt_unix.unix_file_descr ch in
    Lwt_unix.(wrap_syscall Write ch (fun () ->
        U.send fd (Bytes.unsafe_of_string buf) pos len flags))

  let recv ch buf pos len flags =
    if pos < 0 || len < 0 || pos > Bytes.length buf - len then
      Lwt.fail_invalid_arg "recv"
    else
    let fd = Lwt_unix.unix_file_descr ch in
    Lwt_unix.(wrap_syscall Read ch (fun () -> U.send fd buf pos len flags))
