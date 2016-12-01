type ip_option =
  | IP_MULTICAST_IF
  | IP_MULTICAST_TTL
  | IP_MULTICAST_LOOP
  | IP_ADD_MEMBERSHIP
  | IP_DROP_MEMBERSHIP

type ipv6_option =
  | IPV6_JOIN_GROUP
  | IPV6_LEAVE_GROUP
  | IPV6_MULTICAST_HOPS
  | IPV6_MULTICAST_IF
  | IPV6_MULTICAST_LOOP
  | IPV6_UNICAST_HOPS
  | IPV6_V6ONLY

type level =
  | SOL_SOCKET
  | IPPROTO_IP
  | IPPROTO_IPV6
  | IPPROTO_ICMP
  | IPPROTO_RAW
  | IPPROTO_TCP
  | IPPROTO_UDP

type sa_family =
  | AF_INET
  | AF_INET6
  | AF_UNIX
  | AF_UNSPEC

type sendrecvflags =
  | MSG_CONFIRM
  | MSG_DONTROUTE
  | MSG_DONTWAIT
  | MSG_EOR
  | MSG_MORE
  | MSG_NOSIGNAL
  | MSG_OOB
  | MSG_CMSG_CLOEXEC
  | MSG_ERRQUEUE
  | MSG_PEEK
  | MSG_TRUNC
  | MSG_WAITALL

external swap16 : int -> int = "%bswap16";;

external int_of_level : level -> int = "c_int_of_level"
external int_of_ip_option : ip_option -> int = "c_int_of_ip_option"
external int_of_ipv6_option : ipv6_option -> int = "c_int_of_ipv6_option"
external int_of_sa_family : sa_family -> int = "c_int_of_sa_family"
external int_of_sendrecvflags : sendrecvflags -> int = "c_int_of_sendrecvflags"

let int_of_flags flags = List.fold_left (fun acc f -> acc lor int_of_sendrecvflags f) 0 flags

external if_nametoindex : string -> int = "if_nametoindex_stub"
external setsockopt_int : Unix.file_descr -> int -> int -> int -> unit = "setsockopt_int_stub"
external ip_add_membership : Unix.file_descr -> string -> string -> unit = "ip_add_membership"
external ip_drop_membership : Unix.file_descr -> string -> string -> unit = "ip_drop_membership"
external ipv6_add_membership : Unix.file_descr -> string -> string -> unit = "ipv6_add_membership"
external ipv6_drop_membership : Unix.file_descr -> string -> string -> unit = "ipv6_drop_membership"

external bind6 : Unix.file_descr -> string -> string -> int -> int -> unit = "bind6_stub"
external connect6 : Unix.file_descr -> string -> string -> int -> int -> unit = "connect6_stub"

external send : Unix.file_descr -> Bytes.t -> int -> int -> int -> int = "send_stub"
external recv : Unix.file_descr -> Bytes.t -> int -> int -> int -> int = "recv_stub"

module type IP = sig
  type addr
  val bind : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> addr -> int -> unit
  val connect : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> addr -> int -> unit
  val membership : ?iface:string -> Unix.file_descr -> addr -> [< `Join | `Leave ] -> unit
  val mcast_outgoing_iface : Unix.file_descr -> string -> unit
  val mcast_loop : Unix.file_descr -> bool -> unit
  val mcast_hops : Unix.file_descr -> int -> unit
end

module IPv4 = struct
  let bind sock v4addr port =
    Unix.(bind sock @@ ADDR_INET (Ipaddr_unix.V4.to_inet_addr v4addr, port))

  let connect sock v4addr port =
    Unix.(connect sock @@ ADDR_INET (Ipaddr_unix.V4.to_inet_addr v4addr, port))

  let membership ?(iface="") fd v4addr =
    let v4addr = Ipaddr.V4.to_bytes v4addr in function
      | `Join -> ip_add_membership fd iface v4addr
      | `Leave -> ip_drop_membership fd iface v4addr

  let mcast_outgoing_iface fd iface =
    setsockopt_int fd (int_of_level IPPROTO_IP)
      (int_of_ip_option IP_MULTICAST_IF) (if_nametoindex iface)

  let mcast_loop fd b =
    setsockopt_int fd (int_of_level IPPROTO_IP)
      (int_of_ip_option IP_MULTICAST_LOOP) (if b then 1 else 0)

  let mcast_hops fd n =
    setsockopt_int fd (int_of_level IPPROTO_IP)
      (int_of_ip_option IP_MULTICAST_TTL) n
end

module IPv6 = struct
  type addr

  let bind ?(iface="") ?(flowinfo=0) sock v6addr port =
    let v6addr = Ipaddr.V6.to_bytes v6addr in
    bind6 sock iface v6addr (swap16 port) flowinfo

  let connect ?(iface="") ?(flowinfo=0) sock v6addr port =
    let v6addr = Ipaddr.V6.to_bytes v6addr in
    connect6 sock iface v6addr (swap16 port) flowinfo

  let membership ?(iface="") fd v6addr =
    let v6addr = Ipaddr.V6.to_bytes v6addr in
    function
    | `Join -> ipv6_add_membership fd iface v6addr
    | `Leave -> ipv6_drop_membership fd iface v6addr

  let mcast_outgoing_iface fd iface =
    setsockopt_int fd (int_of_level IPPROTO_IPV6)
      (int_of_ipv6_option IPV6_MULTICAST_IF) (if_nametoindex iface)

  let mcast_loop fd b =
    setsockopt_int fd (int_of_level IPPROTO_IPV6)
      (int_of_ipv6_option IPV6_MULTICAST_LOOP) (if b then 1 else 0)

  let mcast_hops fd n =
    setsockopt_int fd (int_of_level IPPROTO_IP)
      (int_of_ipv6_option IPV6_MULTICAST_HOPS) n

  let ucast_hops fd n =
    setsockopt_int fd
      (int_of_level IPPROTO_IPV6)
      (int_of_ipv6_option IPV6_UNICAST_HOPS) n
end

module CU = Unix

module Unix = struct
  let bind ?iface ?(flowinfo=0) fd sa = match sa with
    | Unix.ADDR_UNIX a -> Unix.bind fd sa
    | Unix.ADDR_INET (h, p) ->
      match Ipaddr_unix.V6.of_inet_addr h with
      | None -> Unix.bind fd sa
      | Some v6addr -> IPv6.bind ?iface ~flowinfo fd v6addr p


  let connect ?iface ?(flowinfo=0) fd sa = match sa with
    | Unix.ADDR_UNIX a -> Unix.connect fd sa
    | Unix.ADDR_INET (h, p) ->
      match Ipaddr_unix.V6.of_inet_addr h with
      | None -> Unix.connect fd sa
      | Some v6addr -> IPv6.connect ?iface ~flowinfo fd v6addr p

  let send fd buf pos len flags =
    send fd buf pos len (int_of_flags flags)

  let send_substring fd buf pos len flags =
    send fd (Bytes.unsafe_of_string buf) pos len flags

  let recv fd buf pos len flags =
    if (pos < 0 || len < 0 || pos + len > Bytes.length buf)
    then invalid_arg "bounds";
    recv fd buf pos len (int_of_flags flags)

  let membership ?iface fd ipaddr direction =
    match Ipaddr_unix.of_inet_addr ipaddr with
    | Ipaddr.V4 v4addr -> IPv4.membership ?iface fd v4addr direction
    | Ipaddr.V6 v6addr -> IPv6.membership ?iface fd v6addr direction
end

module Lwt_unix = struct
  let bind ?iface ?(flowinfo=0) ch sa =
    Lwt_unix.check_descriptor ch;
    Unix.bind ?iface ~flowinfo (Lwt_unix.unix_file_descr ch) sa

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
              Unix.connect ?iface ~flowinfo fd sa
            with
            | CU.Unix_error (EISCONN, _, _) ->
              (* This is the windows way of telling that the connection
                 has completed. *)
              ()
          else
            raise Retry
        else
          try
            Unix.connect ?iface ~flowinfo fd sa
          with
          | CU.Unix_error (EWOULDBLOCK, _, _) ->
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
          match CU.getsockopt_error fd with
          | None ->
            (* The socket is connected *)
            ()
          | Some err ->
            (* An error happened: *)
            raise (CU.Unix_error(err, "connect", ""))
        else
          try
            (* We should pass only one time here, unless the system call
               is interrupted by a signal: *)
            Unix.connect ?iface ~flowinfo fd sa
          with
          | CU.Unix_error (EINPROGRESS, _, _) ->
            in_progress := true;
            raise Retry
      end

  let send ch buf pos len flags =
    if pos < 0 || len < 0 || pos > Bytes.length buf - len then
      invalid_arg "Sockopt.L.send";
    let fd = Lwt_unix.unix_file_descr ch in
    Lwt_unix.(wrap_syscall Write ch (fun () -> Unix.send fd buf pos len flags))

  let send_substring ch buf pos len flags =
    if pos < 0 || len < 0 || pos > String.length buf - len then
      invalid_arg "Sockopt.L.send_substring";
    let fd = Lwt_unix.unix_file_descr ch in
    Lwt_unix.(wrap_syscall Write ch (fun () -> Unix.send_substring fd buf pos len flags))

  let recv ch buf pos len flags =
    if pos < 0 || len < 0 || pos > Bytes.length buf - len then
      invalid_arg "Sockopt.L.recv";
    let fd = Lwt_unix.unix_file_descr ch in
    Lwt_unix.(wrap_syscall Read ch (fun () -> Unix.send_substring fd buf pos len flags))
end
