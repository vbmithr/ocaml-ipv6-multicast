val bind6 : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Ipaddr.V6.t -> int -> unit
(** [bind6 ~iface ~flow fd v6addr port] binds [fd] to the socket
    address [v6addr:port]. The optional argument [iface] is required
    when binding a link-local multicast address, which is impossible
    to do with {!Unix.bind}. *)

module IPV6 : sig
  val membership : ?iface:string -> Unix.file_descr -> Ipaddr.V6.t -> [< `Join | `Leave ] -> unit
  val mcast_outgoing_iface : Unix.file_descr -> string -> unit
  val mcast_loop : Unix.file_descr -> bool -> unit
  val mcast_hops : Unix.file_descr -> int -> unit
  val ucast_hops : Unix.file_descr -> int -> unit
end
