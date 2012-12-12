#include <netinet/in.h>
#include <arpa/inet.h>

main()
{
  unsigned int addr;
  static char daffy[] = "128.95.2.11";
  struct in_addr inaddr;


  printf("\nDaffy:\n");
  addr = inet_addr(daffy);
  printf("Dot: %s, byte-ordered: %lu\n", daffy, addr);

  inaddr.s_addr = addr;
  
  printf("%d back to dot: %s\n", inaddr.s_addr, inet_ntoa( inaddr ));

}
