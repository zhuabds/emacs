#include <stdio.h>

main()
{
  int yy = 00, mm = 0, dd = 0;

  printf("Enter a date (mm/dd/yy):");
  scanf("%d/%d/%d", &mm, &dd, &yy);
  printf("You entered the date %.2d%.2d%.2d\n", yy, mm, dd);

  return 0;
}
