#include <sys/socket.h>
#include <netinet/in.h>

#include <caml/mlvalues.h>

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
    default: return (Val_int(-1));
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
    default: return (Val_int(-1));
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
    default: return (Val_int(-1));
    }
}
