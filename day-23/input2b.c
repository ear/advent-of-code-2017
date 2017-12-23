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
  for (b = 108100, c = 108100;; b += 17) {
    f = 1;
    for (d = 2; d != b;) {
      for (e = 2; e != b;) {
        // printf("mul\n");
        if (d * e == b)
          f = 0;
        ++e;
        // printf("%d %d %d %d %d %d %d %d\n", a, b, c, d, e, f, g, h);
        printf("%d %d %d %d\n", d, e, d * e, b);
      }
      ++d;
    }
    if (f == 0)
      ++h;
    if (b == c) {
      printf("%d loops (finished)\n", ++COUNTER);
      return 0;
    } else
      printf("%d loops (looping)\n", ++COUNTER);
  }
}
