// The codegen will use this file to link "extern" C functions
#include <stdio.h>

void println_int64(long long x) {
    printf("%lld\n", x);
}
