#include <stdio.h>

main()
{
  float num, denom;
  int price = 0, amount = 0;

  printf("请输入股票价格及数量:\n");
  scanf("%d %f/%f %d", &price, &num, &denom, &amount);

  printf("价值:%f\n", (price + num / denom) * amount);

  return 0;
}
