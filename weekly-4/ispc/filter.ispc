export uniform int filter_ispc_slow(uniform float output[], uniform float input[], uniform int n) {
  uniform int m = 0;
  foreach (i = 0 ... n) {
    float j = input[i];
    int keep = j >= 0;
    int offset = exclusive_scan_add(keep);
    if (keep) {
      output[m + offset] = j;
    }
    m += reduce_add(keep);
  }
  return m;
}

export uniform int filter_ispc(uniform float output[], uniform float input[], uniform int n) {
  uniform int m = 0;
  foreach (i = 0 ... n) {
    float j = input[i];
    int keep = j >= 0;
    int offset = exclusive_scan_add(keep);
    if (!keep) {
      offset = programCount-1;
    }
    output[m + offset] = j;
    m += reduce_add(keep);
  }
  return m;
}
