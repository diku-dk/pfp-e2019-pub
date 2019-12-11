#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "timing.h"
#include "relax.h"

static inline void relax_c(float *output, float* input, int n) {
  for (int i = 0; i < n; i++) {
    float l, c, r;
    l=input[i == 0 ? n-1 : i-1];
    c=input[i];
    r=input[(i+1) % n];
    output[i] = (l + c + r)/3;
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
    relax_c(c_output, input, n);
  }
  printf("C:                 %8d microseconds\n", runtime);

  TIMEIT(runtime) {
    relax_ispc_naive(ispc_output, input, n);
  }
  printf("ISPC (naive):      %8d microseconds\n", runtime);

  TIMEIT(runtime) {
    relax_ispc(ispc_output, input, n);
  }
  printf("ISPC:              %8d microseconds\n", runtime);

  for (int i = 0; i < n; i++) {
    if (c_output[i] != ispc_output[i]) {
      fprintf(stderr, "Results differ at [%d]: %f != %f\n", i, c_output[i], ispc_output[i]);
      return 1;
    }
  }
}
