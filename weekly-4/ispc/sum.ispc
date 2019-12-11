export uniform int sum_ispc(uniform int input[], uniform int n) {
  int sum = 0;
  foreach (i = 0 ... n) {
    sum += input[i];
  }

  uniform int sums[programCount];
  sums[programIndex] = sum;

  uniform int total_sum = 0;
  if (programIndex == 0) {
    for (uniform int i = 0; i < programCount; i++) {
      total_sum += sums[i];
    }
  }

  return total_sum;
}

export uniform int sum_ispc_intrinsics(uniform int input[], uniform int n) {
  int sum = 0;
  foreach (i = 0 ... n ) {
    sum += input[i];
  }

  return reduce_add(sum);
}
