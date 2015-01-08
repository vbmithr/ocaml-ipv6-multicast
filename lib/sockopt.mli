val if_nametoindex : string -> int
(** [if_nametoindex iface] is the index of iface [iface]. *)

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

module IP : sig
  module V4 : sig
    val bind : Unix.file_descr -> Ipaddr.V4.t -> int -> unit
    val connect : Unix.file_descr -> Ipaddr.V4.t -> int -> unit
    val membership : ?iface_addr:Ipaddr.V4.t -> Unix.file_descr -> Ipaddr.V4.t -> [< `Join | `Leave ] -> unit
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
  val membership6 : ?iface:string -> Unix.file_descr -> Unix.inet_addr -> [< `Join | `Leave ] -> unit

  val send : Unix.file_descr -> Bytes.t -> int -> int -> sendrecvflags list -> int
  val send_substring : Unix.file_descr -> string -> int -> int -> sendrecvflags list -> int
  val recv : Unix.file_descr -> bytes -> int -> int -> sendrecvflags list -> int
end

module L : sig
  val bind : ?iface:string -> ?flowinfo:int -> Lwt_unix.file_descr -> Unix.sockaddr -> unit
  val connect : ?iface:string -> ?flowinfo:int -> Lwt_unix.file_descr -> Unix.sockaddr -> unit Lwt.t
  val send : Lwt_unix.file_descr -> Bytes.t -> int -> int -> sendrecvflags list -> int Lwt.t
  val send_substring : Lwt_unix.file_descr -> string -> int -> int -> sendrecvflags list -> int Lwt.t
  val recv : Lwt_unix.file_descr -> Bytes.t -> int -> int -> sendrecvflags list -> int Lwt.t
end
