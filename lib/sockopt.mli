val if_nametoindex : string -> int
(** [if_nametoindex iface] is the index of iface [iface]. *)

module IP : sig
  module V4 : sig
    val bind : Unix.file_descr -> Ipaddr.V4.t -> int -> unit
    val connect : Unix.file_descr -> Ipaddr.V4.t -> int -> unit
    val membership : ?iface:string -> Unix.file_descr -> Ipaddr.V4.t -> [< `Join | `Leave ] -> unit
    val mcast_outgoing_iface : Unix.file_descr -> string -> unit
    val mcast_loop : Unix.file_descr -> bool -> unit
    val mcast_hops : Unix.file_descr -> int -> unit
  end

  module V6 : sig
    val bind : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Ipaddr.V6.t -> int -> unit
    val connect : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Ipaddr.V6.t -> int -> unit
    val membership : ?iface:string -> Unix.file_descr -> Ipaddr.V6.t -> [< `Join | `Leave ] -> unit
    val mcast_outgoing_iface : Unix.file_descr -> string -> unit
    val mcast_loop : Unix.file_descr -> bool -> unit
    val mcast_hops : Unix.file_descr -> int -> unit
    val ucast_hops : Unix.file_descr -> int -> unit
  end
end

module U : sig
  val bind : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Unix.sockaddr -> unit
  val connect : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Unix.sockaddr -> unit
  val membership : ?iface:string -> Unix.file_descr -> Unix.inet_addr -> [< `Join | `Leave ] -> unit
end
