(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

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

(** IPv6 multicast compatible with Ipaddr types *)
module I : sig
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

(** IPv6 multicast compatible with Unix types *)
module U : sig
  val bind : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Unix.sockaddr -> unit
  val connect : ?iface:string -> ?flowinfo:int -> Unix.file_descr -> Unix.sockaddr -> unit
  val membership : ?iface:string -> Unix.file_descr -> Unix.inet_addr -> [< `Join | `Leave ] -> unit

  val send : Unix.file_descr -> Bytes.t -> int -> int -> sendrecvflags list -> int
  val send_substring : Unix.file_descr -> string -> int -> int -> sendrecvflags list -> int
  val recv : Unix.file_descr -> bytes -> int -> int -> sendrecvflags list -> int
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
