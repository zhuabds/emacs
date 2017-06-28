#include <stdio.h>

#define PI (4.0 / 3.0)
main()
{
  float r = 10;

  printf("请输入半径:");
  scanf("%f", &r);
  printf("体积:%f\n", PI * 3.14 * (r * r *r));

  return 0;
}
