#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "timing.h"
#include "pack.h"

static inline int pack_c(int *output, int *input, int n) {
  int j = 1;
  int cur = input[0];
  output[0] = cur;
  for (int i = 1; i < n; i++) {
    int v = input[i];
    if (v != cur) {
      output[j++] = v;
    }
    cur = v;
  }
  return j;
}

int main() {
  int n = 100000;
  int *input = calloc(n, sizeof(int));
  int c_m;
  int *c_output = calloc(n, sizeof(int));
  int ispc_m;
  int *ispc_output = calloc(n, sizeof(int));

  for (int i = 0; i < n; i++) {
    input[i] = rand() % 2;
  }

  int runtime;

  TIMEIT(runtime) {
    c_m = pack_c(c_output, input, n);
  }
  printf("C:                 %8d microseconds\n", runtime);

  TIMEIT(runtime) {
    ispc_m = pack_ispc(ispc_output, input, n);
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
