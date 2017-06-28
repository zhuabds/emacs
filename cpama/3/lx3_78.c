#include <stdio.h>

main()
{
  int i = 0, j = 0;
  float x = 0, y = 0;

  scanf("%d%f%d", &i, &x, &j);
  printf("i:%d, x:%f, j:%d\n", i, x, j);

  x = 0.0;
  y = 0.0;
  i = 0;

  scanf("%f%d%f", &x, &i, &y);
  printf("x:%f, i:%d, y:%f\n", x, i, y);

  return 0;
}
