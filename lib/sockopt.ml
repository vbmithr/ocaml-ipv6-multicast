open Ctypes
open Foreign

let if_nametoindex = foreign ~check_errno:true "if_nametoindex" (string @-> returning uint)
let setsockopt = foreign ~check_errno:true "setsockopt" (int @-> int @-> int @-> ptr void @-> int @-> returning int)
let getsockopt = foreign ~check_errno:true "getsockopt" (int @-> int @-> int @-> ptr void @-> ptr int @-> returning int)

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

type in6_addr
let in6_addr : in6_addr structure typ = structure "in6_addr"
let s6_addr = field in6_addr "s6_addr" (array 16 uint8_t)
let () = seal in6_addr
let make_in6_addr v6addr =
  let v6addr_bytes = Ipaddr.V6.to_bytes v6addr
                     |> String.explode
                     |> List.map (fun c -> Char.code c |> Unsigned.UInt8.of_int) in
  let s = make in6_addr in
  setf s s6_addr (Ctypes.Array.of_list uint8_t v6addr_bytes);
  s

type ipv6_mreq
let ipv6_mreq : ipv6_mreq structure typ = structure "ipv6_mreq"
let ipv6mr_multiaddr = field ipv6_mreq "ipv6mr_multiaddr" in6_addr
let ipv6mr_interface = field ipv6_mreq "ipv6mr_interface" uint
let () = seal ipv6_mreq
let make_ipv6_mreq ?iface v6addr =
  let s = make ipv6_mreq in
  (match iface with
   | None -> setf s ipv6mr_interface (Unsigned.UInt.of_int 0)
   | Some name -> setf s ipv6mr_interface (if_nametoindex name));
  setf s ipv6mr_multiaddr (make_in6_addr v6addr);
  s

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

let int_of_file_descr (fd:Unix.file_descr) : int = Obj.magic fd
external int_of_level : level -> int = "c_int_of_level"
external int_of_ipv6_option : ipv6_option -> int = "c_int_of_ipv6_option"

module IPV6 = struct
  let membership ?iface fd v6addr direction =
    let s = make_ipv6_mreq ?iface v6addr in
    let direction = match direction with
      | `Join -> IPV6_JOIN_GROUP
      | `Leave -> IPV6_LEAVE_GROUP in
    let ret = setsockopt
        (int_of_file_descr fd)
        (int_of_level IPPROTO_IPV6)
        (int_of_ipv6_option direction)
        (addr s |> to_voidp)
        (sizeof ipv6_mreq)
    in ignore (ret:int)

  let mcast_outgoing_iface fd iface =
    let ret = setsockopt
      (int_of_file_descr fd)
      (int_of_level IPPROTO_IPV6)
      (int_of_ipv6_option IPV6_MULTICAST_IF)
      (if_nametoindex iface |> allocate uint |> to_voidp)
      (sizeof int)
    in ignore (ret:int)
end
