#include <stdio.h>
int main() {
  int a = 0;
  int b = 0;
  int c = 0;
  int d = 0;
  int e = 0;
  int f = 0;
  int g = 0;
  int h = 0;
  int COUNTER = 0;
  int divs[] = {2,    4,    5,    10,    20,    23,    25,   46,   47,
                50,   92,   94,   100,   115,   188,   230,  235,  460,
                470,  575,  940,  1081,  1150,  1175,  2162, 2300, 2350,
                4324, 4700, 5405, 10810, 21620, 27025, 54050};
  for (b = 108100, c = 108100 + 17000;; b += 17) {
    f = 1;
    for (int i = 0; i < 34; ++i) {
      d = divs[i];
      for (int j = 0; j < 34; ++j) {
        e = divs[j];
        // printf("mul\n");
        if (d * e == b)
          f = 0;
        // printf("%d %d %d %d %d %d %d %d\n", a, b, c, d, e, f, g, h);
        printf("%d %d %d %d\n", d, e, d * e, b);
      }
    }
    if (f == 0)
      ++h;
    if (b == c) {
      printf("%d loops (finished): h = %d\n", ++COUNTER, h);
      return 0;
    } else
      printf("%d loops (looping)\n", ++COUNTER);
  }
}
