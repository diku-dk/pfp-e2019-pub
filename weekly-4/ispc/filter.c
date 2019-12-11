#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "timing.h"
#include "filter.h"

static inline int filter_c(float *output, float *input, int n) {
  int j = 0;
  for (int i = 0; i < n; i++) {
    float v = input[i];
    if (v >= 0) {
      output[j++] = v;
    }
  }
  return j;
}

int main() {
  int n = 100000;
  float *input = calloc(n, sizeof(float));
  int c_m;
  float *c_output = calloc(n, sizeof(float));
  int ispc_m;
  float *ispc_output = calloc(n, sizeof(float));

  for (int i = 0; i < n; i++) {
    input[i] = (float)rand()/RAND_MAX * 2 - 1;
  }

  int runtime;

  TIMEIT(runtime) {
    c_m = filter_c(c_output, input, n);
  }
  printf("C:                 %8d microseconds\n", runtime);


  TIMEIT(runtime) {
    ispc_m = filter_ispc_slow(ispc_output, input, n);
  }
  printf("ISPC (slow):       %8d microseconds\n", runtime);

  TIMEIT(runtime) {
    ispc_m = filter_ispc(ispc_output, input, n);
  }
  printf("ISPC:              %8d microseconds\n", runtime);

  if (c_m != ispc_m) {
    printf("C produced %d values, ISPC produced %d values\n", c_m, ispc_m);
    exit(1);
  }

  for (int i = 0; i < c_m; i++) {
    if (c_output[i] != ispc_output[i]) {
      fprintf(stderr, "Results differ at [%d]: %f != %f\n", i, c_output[i], ispc_output[i]);
      return 1;
    }
  }
}
