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
  b = 81;
  c = b;
  int COUNTER = 0;
  while (1) {
    ++COUNTER;
    f = 1;
    d = 2;
    do {
      e = 2;
      do {
        g = (d * e) - b;
        printf("mul\n");
        if (g == 0)
          f = 0;
        ++e;
        g = e - b;
        printf("%d %d %d %d %d %d %d %d\n", a, b, c, d, e, f, g, h);
      } while (g != 0);
      ++d;
      g = d - b;
    } while (g != 0);
    if (f == 0)
      ++h;
    g = b - c;
    if (g == 0) {
      printf("%d loops\n", COUNTER);
      return 0;
    } else
      printf("%d %d %d %d %d %d %d %d\n", a, b, c, d, e, f, g, h);
    b += 17;
  }
}
