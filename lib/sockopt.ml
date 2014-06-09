open Ctypes
open Foreign

type level =
  | SOL_SOCKET
  | IPPROTO_IP
  | IPPROTO_IPV6
  | IPPROTO_ICMP
  | IPPROTO_RAW
  | IPPROTO_TCP
  | IPPROTO_UDP

type ipv6_option =
  | IPV6_JOIN_GROUP
  | IPV6_LEAVE_GROUP
  | IPV6_MULTICAST_HOPS
  | IPV6_MULTICAST_IF
  | IPV6_MULTICAST_LOOP
  | IPV6_UNICAST_HOPS
  | IPV6_V6ONLY

type sa_family =
  | AF_INET
  | AF_INET6
  | AF_UNIX
  | AF_UNSPEC

let if_nametoindex = foreign ~check_errno:true "if_nametoindex" (string @-> returning int)
external swap16 : int -> int = "%bswap16";;
let int_of_file_descr (fd:Unix.file_descr) : int = Obj.magic fd
external int_of_level : level -> int = "c_int_of_level"
external int_of_ipv6_option : ipv6_option -> int = "c_int_of_ipv6_option"
external int_of_sa_family : sa_family -> int = "c_int_of_sa_family"

module String = struct
  include String
  let explode s =
    let rec expl i l =
      if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) [];;

  let implode l =
    let result = String.create (List.length l) in
    let rec imp i = function
      | [] -> result
      | c :: l -> result.[i] <- c; imp (i + 1) l in
    imp 0 l
end

module In6_addr = struct
  type t
  let t : t structure typ = structure "in6_addr"
  let s6_addr = field t "s6_addr" (array 16 uint8_t)
  let () = seal t
  let make v6addr =
    let v6addr_bytes = Ipaddr.V6.to_bytes v6addr
                       |> String.explode
                       |> List.map (fun c -> Char.code c |> Unsigned.UInt8.of_int) in
    let s = make t in
    setf s s6_addr (Ctypes.CArray.of_list uint8_t v6addr_bytes);
    s
end

module Sockaddr = struct
  type t
  let t : t structure typ = structure "sockaddr"
  let sa_family = field t "sa_family" uint
  let sa_data = field t "sa_data" (array 14 uint8_t)
  let () = seal t
end

module Sockaddr_in6 = struct
  type t
  let t : t structure typ = structure "sockaddr_in6"
  let sin6_family = field t "sin6_family" uint16_t
  let sin6_port = field t "sin6_port" uint16_t
  let sin6_flowinfo = field t "sin6_flowinfo" uint32_t
  let sin6_addr = field t "sin6_addr" In6_addr.t
  let sin6_scope_id = field t "sin6_scope_id" uint32_t
  let () = seal t
  let make ?iface ?(flowinfo=0) v6addr port =
    let s = make t in
    setf s sin6_family (int_of_sa_family AF_INET6 |> Unsigned.UInt16.of_int);
    setf s sin6_port (port |> swap16 |> Unsigned.UInt16.of_int);
    setf s sin6_flowinfo (flowinfo |> Int32.of_int |> Unsigned.UInt32.of_int32);
    setf s sin6_addr (In6_addr.make v6addr);
    (match iface with
    | None -> setf s sin6_scope_id (Unsigned.UInt32.zero)
    | Some name -> setf s sin6_scope_id (if_nametoindex name |> Unsigned.UInt32.of_int));
    s
end

module Ipv6_mreq = struct
  type t
  let t : t structure typ = structure "ipv6_mreq"
  let ipv6mr_multiaddr = field t "ipv6mr_multiaddr" In6_addr.t
  let ipv6mr_interface = field t "ipv6mr_interface" int
  let () = seal t
  let make ?iface v6addr =
    let s = make t in
    (match iface with
     | None -> setf s ipv6mr_interface 0
     | Some name -> setf s ipv6mr_interface (if_nametoindex name));
    setf s ipv6mr_multiaddr (In6_addr.make v6addr);
    s
end

let setsockopt = foreign ~check_errno:true "setsockopt" (int @-> int @-> int @-> ptr void @-> int @-> returning int)
let getsockopt = foreign ~check_errno:true "getsockopt" (int @-> int @-> int @-> ptr void @-> ptr int @-> returning int)
let bind = foreign ~check_errno:true "bind" (int @-> ptr Sockaddr_in6.t @-> int @-> returning int)
let connect = foreign ~check_errno:true "connect" (int @-> ptr Sockaddr_in6.t @-> int @-> returning int)

let bind6 ?iface ?(flowinfo=0) sock v6addr port =
  let saddr_in6 = Sockaddr_in6.make ?iface ~flowinfo v6addr port in
  let ret = bind
      (int_of_file_descr sock)
      (addr saddr_in6)
      (sizeof Sockaddr_in6.t)
  in ignore (ret:int)

let connect6 ?iface ?(flowinfo=0) sock v6addr port =
  let saddr_in6 = Sockaddr_in6.make ?iface ~flowinfo v6addr port in
  let ret = connect
    (int_of_file_descr sock)
    (addr saddr_in6)
    (sizeof Sockaddr_in6.t)
  in ignore (ret:int)

module IPV6 = struct
  let membership ?iface fd v6addr direction =
    let s = Ipv6_mreq.make ?iface v6addr in
    let direction = match direction with
      | `Join -> IPV6_JOIN_GROUP
      | `Leave -> IPV6_LEAVE_GROUP in
    let ret = setsockopt
        (int_of_file_descr fd)
        (int_of_level IPPROTO_IPV6)
        (int_of_ipv6_option direction)
        (addr s |> to_voidp)
        (sizeof Ipv6_mreq.t)
    in ignore (ret:int)

  let mcast_outgoing_iface fd iface =
    let ret = setsockopt
      (int_of_file_descr fd)
      (int_of_level IPPROTO_IPV6)
      (int_of_ipv6_option IPV6_MULTICAST_IF)
      (if_nametoindex iface |> allocate int |> to_voidp)
      (sizeof int)
    in ignore (ret:int)

  let mcast_loop fd b =
    let ret = setsockopt
      (int_of_file_descr fd)
      (int_of_level IPPROTO_IPV6)
      (int_of_ipv6_option IPV6_MULTICAST_LOOP)
      (allocate uint Unsigned.UInt.(if b then one else zero) |> to_voidp)
      (sizeof uint)
    in ignore (ret:int)

  let mcast_hops fd n =
    let ret = setsockopt
      (int_of_file_descr fd)
      (int_of_level IPPROTO_IPV6)
      (int_of_ipv6_option IPV6_MULTICAST_HOPS)
      (allocate int n |> to_voidp)
      (sizeof int)
    in ignore (ret:int)

  let ucast_hops fd n =
    let ret = setsockopt
      (int_of_file_descr fd)
      (int_of_level IPPROTO_IPV6)
      (int_of_ipv6_option IPV6_UNICAST_HOPS)
      (allocate int n |> to_voidp)
      (sizeof int)
    in ignore (ret:int)
end
