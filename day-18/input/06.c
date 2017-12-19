#include <stdio.h>
#include <stdlib.h>
int main () {
uint64_t i=31;
uint64_t a=1;
do {
    a = a*2;
    --i;
} while(i > 0);
printf("%llu\n", a);
printf("%llu %llu\n", 1<<31-1, 1<<32-1);
return 0;
}
