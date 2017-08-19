open Ipv6_multicast

val bind : ?iface:string -> ?flowinfo:int -> Lwt_unix.file_descr -> Unix.sockaddr -> unit
val connect : ?iface:string -> ?flowinfo:int -> Lwt_unix.file_descr -> Unix.sockaddr -> unit Lwt.t

val send : Lwt_unix.file_descr -> Bytes.t -> int -> int -> sendrecvflags list -> int Lwt.t
val send_substring : Lwt_unix.file_descr -> string -> int -> int -> sendrecvflags list -> int Lwt.t
val recv : Lwt_unix.file_descr -> Bytes.t -> int -> int -> sendrecvflags list -> int Lwt.t


