#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>

int
main()
{
  int sd;

  printf("Creating a socket.\n");
  sd = socket(AF_INET, SOCK_DGRAM, 0);

  printf("sd: %x\n", sd);

  printf("Closing socket.\n");
  close(sd);

  return 0;
}
