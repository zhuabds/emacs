#include <stdio.h>

main()
{
  int d, i1, i2, i3, i4, i5, j1, j2, j3, j4, j5,
    i_sum, j_sum, total;


  printf("Enter thr First (single) digit:");
  scanf("%1d", &d);
  printf("Enter first group of five digits:");
  scanf("%1d%1d%1d%d1%d1", &i1, &i2, &i3, &i4, &i5);
  printf("Enter second group of five digits:");
  scanf("%1d%1d%1d%1d%1d", &j1, &j2, &j3, &j4, &j5);

  i_sum = d + i2 + i4 + j1 + j3 + j5;
  j_sum = i1 + i3 + i5 + j2 + j4;

  total = 9 - ((i_sum * 3) + j_sum - 1) % 10;

  printf("Check digit: %d\n", total);

  return 0;
}
