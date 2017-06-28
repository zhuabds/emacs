#include <stdio.h>

main()
{
  int lan = 0, pub = 0, booknum = 0, chkdigit = 0;

  printf("Enter ISBN:");
  scanf("%d-%d-%d-%d", &lan, &pub, &booknum, &chkdigit);

  printf("Language: %d\n", lan);
  printf("Publisher: %d\n", pub);
  printf("Book Number: %d\n", booknum);
  printf("Check digit: %d\n", chkdigit);

  return 0;
}
