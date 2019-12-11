#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "timing.h"
#include "sum.h"

static inline int sum_c(int *input, int n) {
  int res = 0;
  for (int i = 0; i < n; i++) {
    res += input[i];
  }
  return res;
}

int main() {
  int n = 100000000;
  int *input = calloc(n, sizeof(int));

  for (int i = 0; i < n; i++) {
    input[i] = rand();
  }

  int res_c, res_ispc;

  int runtime;

  TIMEIT(runtime) {
    res_c = sum_c(input, n);
  }
  printf("C:                 %8d microseconds\n", runtime);

  TIMEIT(runtime) {
    res_ispc = sum_ispc(input, n);
  }
  printf("ISPC:              %8d microseconds\n", runtime);

  if (res_c != res_ispc) {
    fprintf(stderr, "Results differ: %d != %d\n", res_c, res_ispc);
    return 1;
  }

  TIMEIT(runtime) {
    res_ispc = sum_ispc_intrinsics(input, n);
  }
  printf("ISPC (intrinsics): %8d microseconds\n", runtime);

  if (res_c != res_ispc) {
    fprintf(stderr, "Results differ: %d != %d\n", res_c, res_ispc);
    return 1;
  }

}
