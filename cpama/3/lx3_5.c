#include <stdio.h>

main()
{
  int itemnum = 0;
  float price = 0;
  int yy = 0, mm = 0, dd = 0;

  printf("Enter item number:");
  scanf("%d", &itemnum);

  printf("Enter unit price:");
  scanf("%f", &price);

  printf("Enter purchase date (mm/dd/yy):");
  scanf("%d/%d/%d", &mm, &dd, &yy);

  printf("Item\tUnit\tPurchase\n\tPrice\tDate\n%d\t$%6.2f\t%.2d/%.2d/%.2d\n", itemnum, price, mm, dd, yy);

  return 0;
}
