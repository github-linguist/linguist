#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

const char *msg = "hello socket world";

int main()
{
   int i, sock, len, slen;

   struct addrinfo hints, *addrs;
   memset(&hints, 0, sizeof(struct addrinfo));
   hints.ai_family = AF_UNSPEC;
   hints.ai_socktype = SOCK_STREAM;

   if (0 == getaddrinfo("localhost", "256", &hints, &addrs))
   {
       sock = socket(addrs->ai_family, addrs->ai_socktype, addrs->ai_protocol);
       if ( sock >= 0 )
       {
           if ( connect(sock, addrs->ai_addr, addrs->ai_addrlen) >= 0 )
           {
               const char *pm = msg;
               do
               {
                  len = strlen(pm);
                  slen = send(sock, pm, len, 0);
                  pm += slen;
               } while ((0 <= slen) && (slen < len));
           }
           close(sock);
       }
       freeaddrinfo(addrs);
   }
}
