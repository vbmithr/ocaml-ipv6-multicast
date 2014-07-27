open Unix

type ipver = V4 | V6

val if_nametoindex : string -> int
  (** [if_nametoindex iface] is the index of iface [iface]. *)

val bind : ?iface:string -> ?flowinfo:int -> file_descr -> sockaddr -> unit
val connect : ?iface:string -> ?flowinfo:int -> file_descr -> sockaddr -> unit

val bind6 : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Ipaddr.V6.t -> int -> unit
(** [bind6 ~iface ~flow fd v6addr port] binds [fd] to the socket
    address [v6addr:port]. The optional argument [iface] is required
    when binding a link-local multicast address, which is impossible
    to do with {!Unix.bind}. *)

val connect6 : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Ipaddr.V6.t -> int -> unit
(** [connect6 ~iface ~flow fd v6addr port] connects [fd] to the socket
    address [v6addr:port]. The optional argument [iface] is required
    when binding a link-local address, which is impossible to do with
    {!Unix.connect}. *)

(* Both IPv4 and IPv6 *)
val membership : ?iface:string -> Unix.file_descr -> Ipaddr.t -> [< `Join | `Leave ] -> unit
val mcast_outgoing_iface : Unix.file_descr -> ipver -> string -> unit
val mcast_loop : Unix.file_descr -> ipver -> bool -> unit
val mcast_hops : Unix.file_descr -> ipver -> int -> unit

(* IPv6 only *)
val ucast_hops : Unix.file_descr -> int -> unit

