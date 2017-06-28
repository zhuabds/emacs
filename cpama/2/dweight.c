#include <stdio.h>

main(){
  int height, length, width, volume, weight;
  height = 8;
  length = 12;
  width = 10;

  volume = height * length * width;
  weight = (volume + 166) / 166;
  printf("L:%d W:%d H:%d\nVolume:%d\nWeight:%d\n",
         length, width, height, volume, weight);
}
