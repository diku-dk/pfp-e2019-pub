#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "timing.h"
#include "rle.h"

static inline int rle_c(int *output, int *input, int n) {
  int cur = input[0];
  int count = 1;
  int j = 0;
  for (int i = 1; i < n; i++) {
    int next = input[i];
    if (next != cur) {
      output[j++] = count;
      output[j++] = cur;
      cur = next;
      count = 1;
    } else {
      count++;
    }
  }
  output[j++] = count;
  output[j++] = cur;
  return j;
}

int main() {
  int n = 100000;
  int *input = calloc(n, sizeof(int));
  int c_m;
  int *c_output = calloc(n, sizeof(int));
  int ispc_m;
  int *ispc_output = calloc(n, sizeof(int));

  for (int i = 0; i < n;) {
    int x = rand()%2;
    int count = rand()%100;
    for (int j = 0; j < count && i < n; j++, i++) {
      input[i] = x;
    }
  }

  int runtime;

  TIMEIT(runtime) {
    c_m = rle_c(c_output, input, n);
  }
  printf("C:                 %8d microseconds\n", runtime);

  TIMEIT(runtime) {
    ispc_m = rle_ispc(ispc_output, input, n);
  }
  printf("ISPC:              %8d microseconds\n", runtime);

  if (c_m != ispc_m) {
    printf("C produced %d values, ISPC produced %d values\n", c_m, ispc_m);
    exit(1);
  }

  for (int i = 0; i < c_m; i++) {
    if (c_output[i] != ispc_output[i]) {
      fprintf(stderr, "Results differ at [%d]: %d != %d\n", i, c_output[i], ispc_output[i]);
      return 1;
    }
  }
}
