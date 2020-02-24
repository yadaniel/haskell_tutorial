#include <stdio.h>
#include <stdint.h>

#include "foo.h"

uint32_t data[10] = {0,1,2,3,4,5,6,7,8,9};

uint32_t nextData(void) {
    static int idx = -1;
    idx += 1;
    idx %= 10;
    return data[idx];
}

void info(void) {
    printf("module foo\n");
}

int foo(int x1, int x2) {
    return x1 + x2;
}

double bar(double x1, double x2) {
    return x1 - x2;
}

