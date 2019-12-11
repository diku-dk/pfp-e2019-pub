#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "timing.h"
#include "scan.h"

static inline void scan_c(float *output, float *input, int n) {
  float acc = 0;
  for (int i = 0; i < n; i++) {
    acc += input[i];
    output[i] = acc;
  }
}

int main() {
  int n = 100000;
  float *input = calloc(n, sizeof(float));
  float *c_output = calloc(n, sizeof(float));
  float *ispc_output = calloc(n, sizeof(float));

  for (int i = 0; i < n; i++) {
    input[i] = (float)rand()/RAND_MAX;
  }

  int runtime;

  TIMEIT(runtime) {
    scan_c(c_output, input, n);
  }
  printf("C:                 %8d microseconds\n", runtime);

  TIMEIT(runtime) {
    scan_ispc(ispc_output, input, n);
  }
  printf("ISPC:              %8d microseconds\n", runtime);

  for (int i = 0; i < n; i++) {
    // If necessary, fiddle with the tolerance here (recall that
    // floating-point addition is not actually associative).
    if (fabsf(c_output[i] - ispc_output[i]) > 0.001) {
      fprintf(stderr, "Results differ at [%d]: %f != %f\n", i, c_output[i], ispc_output[i]);
      return 1;
    }
  }
}
