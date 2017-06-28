#include <stdio.h>

#define CUBIC_IN_PER_LB 166

main()
{
  int height, width, length, volume, weight;

  printf("请输入高度:");
  scanf("%d", &height);
  printf("输入的的高度是:%d\n", height);

  printf("请输入宽度:");
  scanf("%d", &width);
  printf("输入的的宽度是:%d\n", width);

  printf("请输入长度:");
  scanf("%d", &length);
  printf("输入的的长度是:%d\n", length);

  volume = length * width * height;
  weight = (volume + CUBIC_IN_PER_LB - 1) / CUBIC_IN_PER_LB;

  printf("长(%d)X宽(%d)X高(%d):%d\n", length, width, height, volume);
  printf("空间重量:%d\n", weight);

  return 0;
}
