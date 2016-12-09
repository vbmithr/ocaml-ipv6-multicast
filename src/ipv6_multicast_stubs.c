#include <string.h>
#include <errno.h>

#include <net/if.h>
#include <netinet/ip.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#ifdef __APPLE__
#define IPV6_ADD_MEMBERSHIP IPV6_JOIN_GROUP
#define IPV6_DROP_MEMBERSHIP IPV6_LEAVE_GROUP
#endif

int if_nametoindex_stub(value ifname)
{
  return Val_int(if_nametoindex(String_val(ifname)));
}

CAMLprim value send_stub(value sockfd, value buf, value pos, value len, value flags)
{
  CAMLparam5(sockfd, buf, pos, len, flags);
  int ret;
  enter_blocking_section();
  ret = send(Int_val(sockfd), String_val(buf)+Int_val(pos), Int_val(len), Int_val(flags));
  leave_blocking_section();
  if (ret == -1) uerror("send", Nothing);
  CAMLreturn(Val_int(ret));
}

CAMLprim value sendto4_stub(value sockfd, value buf, value flags, value addr, value port)
{
  CAMLparam5(sockfd, buf, flags, addr, port);
  int ret;
  struct sockaddr_in sa;
  sa.sin_family = AF_INET;
  sa.sin_port = Int_val(port);
  memcpy(&sa.sin_addr, String_val(addr), sizeof(struct in_addr));
  enter_blocking_section();
  ret = sendto(Int_val(sockfd), String_val(buf), caml_string_length(buf), Int_val(flags),
	       (const struct sockaddr *)&sa, sizeof(struct sockaddr_in));
  leave_blocking_section();
  if (ret == -1) uerror("sendto", Nothing);
  CAMLreturn(Val_int(ret));
}

CAMLprim value sendto6_stub(value sockfd, value buf, value flags, value addr, value port,
			     value flowinfo, value scope_id)
{
  CAMLparam5(sockfd, buf, flags, addr, port);
  CAMLxparam2(flowinfo, scope_id);
  int ret;
  struct sockaddr_in6 sa;
  sa.sin6_family = AF_INET6;
  sa.sin6_port = Int_val(port);
  sa.sin6_flowinfo = Int_val(flowinfo);
  memcpy(&sa.sin6_addr, String_val(addr), 16);
  sa.sin6_scope_id = Int_val(scope_id);
  enter_blocking_section();
  ret = sendto(Int_val(sockfd), String_val(buf), caml_string_length(buf), Int_val(flags),
	       (const struct sockaddr *)&sa, sizeof(struct sockaddr_in6));
  leave_blocking_section();
  if (ret == -1) uerror("sendto", Nothing);
  CAMLreturn(Val_int(ret));
}

CAMLprim value recv_stub(value sockfd, value buf, value pos, value len, value flags)
{
  CAMLparam5(sockfd, buf, pos, len, flags);
  int ret;
  enter_blocking_section();
  ret = recv(Int_val(sockfd), String_val(buf)+Int_val(pos), Int_val(len), Int_val(flags));
  leave_blocking_section();
  if (ret == -1) uerror("recv", Nothing);
  CAMLreturn(Val_int(ret));
}

/* TODO: Return the sockaddr to the client. */
CAMLprim value recvfrom_stub(value sockfd, value buf, value flags)
{
  CAMLparam3(sockfd, buf, flags);
  int ret;
  struct sockaddr_in6 sa;
  socklen_t sa_len;
  enter_blocking_section();
  ret = recvfrom(Int_val(sockfd), String_val(buf), caml_string_length(buf), Int_val(flags),
		 (struct sockaddr *)&sa, &sa_len);
  leave_blocking_section();
  if (ret == -1) uerror("recvfrom", Nothing);
  CAMLreturn(Val_int(ret));
}

CAMLprim value bind6_stub(value sockfd, value ifname, value addr, value port, value flowinfo)
{
  CAMLparam5(sockfd, ifname, addr, port, flowinfo);
  int ret;
  struct sockaddr_in6 sa;
  sa.sin6_family = AF_INET6;
  sa.sin6_port = Int_val(port);
  sa.sin6_flowinfo = Int_val(flowinfo);
  memcpy(&sa.sin6_addr, String_val(addr), sizeof(struct in6_addr));
  sa.sin6_scope_id = if_nametoindex(String_val(ifname));
  ret = bind(Int_val(sockfd), (const struct sockaddr *)&sa, sizeof(struct sockaddr_in6));
  if (ret == -1) uerror("bind", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value connect6_stub(value sockfd, value ifname, value addr, value port, value flowinfo)
{
  CAMLparam5(sockfd, ifname, addr, port, flowinfo);
  int ret;
  struct sockaddr_in6 sa;
  sa.sin6_family = AF_INET6;
  sa.sin6_port = Int_val(port);
  sa.sin6_flowinfo = Int_val(flowinfo);
  memcpy(&sa.sin6_addr, String_val(addr), sizeof(struct in6_addr));
  sa.sin6_scope_id = if_nametoindex(String_val(ifname));
  enter_blocking_section();
  ret = connect(Int_val(sockfd), (const struct sockaddr *)&sa, sizeof(struct sockaddr_in6));
  leave_blocking_section();
  if (ret == -1) uerror("connect", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value getsockopt_int_stub(value sockfd, value level, value optname)
{
  CAMLparam3(sockfd, level, optname);
  int ret;
  int c_value;
  int sizeof_int = sizeof(int);
  ret = getsockopt(Int_val(sockfd), Int_val(level), Int_val(optname),
		   &c_value, (socklen_t *)&sizeof_int);
  if (ret == -1) uerror("getsockopt_int", Nothing);
  CAMLreturn(Val_int(c_value));
}

CAMLprim value setsockopt_int_stub(value sockfd, value level, value optname, value optval)
{
  CAMLparam3(sockfd, level, optname);
  int ret;
  int c_optval = Int_val(optval);
  ret = setsockopt(Int_val(sockfd), Int_val(level), Int_val(optname),
		   &c_optval, (socklen_t)sizeof(int));
  if (ret == -1) uerror("setsockopt_int", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value ip_add_membership(value sockfd, value iface, value addr)
{
  CAMLparam3(sockfd, iface, addr);
  int ret;
  struct ip_mreqn mreq;
  memcpy(&mreq.imr_multiaddr, String_val(addr), sizeof(struct in_addr));
  memset(&mreq.imr_address, 0, sizeof(struct in_addr));
  mreq.imr_ifindex = if_nametoindex(String_val(iface));
  ret = setsockopt(Int_val(sockfd), IPPROTO_IP, IP_ADD_MEMBERSHIP,
		   &mreq, sizeof(struct ip_mreqn));
  if (ret == -1) uerror("setsockopt_int", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value ip_drop_membership(value sockfd, value iface, value addr)
{
  CAMLparam3(sockfd, iface, addr);
  int ret;
  struct ip_mreqn mreq;
  memcpy(&mreq.imr_multiaddr, String_val(addr), sizeof(struct in_addr));
  memset(&mreq.imr_address, 0, sizeof(struct in_addr));
  mreq.imr_ifindex = if_nametoindex(String_val(iface));
  ret = setsockopt(Int_val(sockfd), IPPROTO_IP, IP_DROP_MEMBERSHIP,
		   &mreq, sizeof(struct ip_mreqn));
  if (ret == -1) uerror("setsockopt_int", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value ipv6_add_membership(value sockfd, value iface, value addr)
{
  CAMLparam3(sockfd, iface, addr);
  int ret;
  struct ipv6_mreq mreq;
  memcpy(&mreq.ipv6mr_multiaddr, String_val(addr), sizeof(struct in6_addr));
  mreq.ipv6mr_interface = if_nametoindex(String_val(iface));
  ret = setsockopt(Int_val(sockfd), IPPROTO_IPV6, IPV6_ADD_MEMBERSHIP,
		   &mreq, sizeof(struct ipv6_mreq));
  if (ret == -1) uerror("setsockopt_int", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value ipv6_drop_membership(value sockfd, value iface, value addr)
{
  CAMLparam3(sockfd, iface, addr);
  int ret;
  struct ipv6_mreq mreq;
  memcpy(&mreq.ipv6mr_multiaddr, String_val(addr), sizeof(struct in6_addr));
  mreq.ipv6mr_interface = if_nametoindex(String_val(iface));
  ret = setsockopt(Int_val(sockfd), IPPROTO_IPV6, IPV6_DROP_MEMBERSHIP,
		   &mreq, sizeof(struct ipv6_mreq));
  if (ret == -1) uerror("setsockopt_int", Nothing);
  CAMLreturn(Val_unit);
}

int c_int_of_level(int level)
{
  switch(Int_val(level))
    {
    case 0: return Val_int(SOL_SOCKET);
    case 1: return Val_int(IPPROTO_IP);
    case 2: return Val_int(IPPROTO_IPV6);
    case 3: return Val_int(IPPROTO_ICMP);
    case 4: return Val_int(IPPROTO_RAW);
    case 5: return Val_int(IPPROTO_TCP);
    case 6: return Val_int(IPPROTO_UDP);
    default: return Val_int(-1);
    }
}

int c_int_of_ip_option(int option_name)
{
  switch(Int_val(option_name))
    {
    case 0: return Val_int(IP_MULTICAST_IF);
    case 1: return Val_int(IP_MULTICAST_TTL);
    case 2: return Val_int(IP_MULTICAST_LOOP);
    case 3: return Val_int(IP_ADD_MEMBERSHIP);
    case 4: return Val_int(IP_DROP_MEMBERSHIP);
    default: return Val_int(-1);
    }
}

int c_int_of_ipv6_option(int option_name)
{
  switch(Int_val(option_name))
    {
    case 0: return (Val_int(IPV6_JOIN_GROUP));
    case 1: return (Val_int(IPV6_LEAVE_GROUP));
    case 2: return (Val_int(IPV6_MULTICAST_HOPS));
    case 3: return (Val_int(IPV6_MULTICAST_IF));
    case 4: return (Val_int(IPV6_MULTICAST_LOOP));
    case 5: return (Val_int(IPV6_UNICAST_HOPS));
    case 6: return (Val_int(IPV6_V6ONLY));
    default: return Val_int(-1);
    }
}

int c_int_of_sa_family(int sa_family)
{
  switch(Int_val(sa_family))
    {
    case 0: return Val_int(AF_INET);
    case 1: return Val_int(AF_INET6);
    case 2: return Val_int(AF_UNIX);
    case 3: return Val_int(AF_UNSPEC);
    default: return Val_int(-1);
    }
}

int c_int_of_sendrecvflags(int flags)
{
  switch(Int_val(flags))
    {
#ifdef __linux__
    case 0: return Val_int(MSG_CONFIRM);
    case 4: return Val_int(MSG_MORE);
    case 5: return Val_int(MSG_NOSIGNAL);
    case 7: return Val_int(MSG_CMSG_CLOEXEC);
    case 8: return Val_int(MSG_ERRQUEUE);
#endif
    case 1: return Val_int(MSG_DONTROUTE);
    case 2: return Val_int(MSG_DONTWAIT);
    case 3: return Val_int(MSG_EOR);
    case 6: return Val_int(MSG_OOB);
    case 9: return Val_int(MSG_PEEK);
    case 10: return Val_int(MSG_TRUNC);
    case 11: return Val_int(MSG_WAITALL);
    default: caml_invalid_argument("unsupported flag");
    }
}
