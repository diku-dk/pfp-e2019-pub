export void relax_ispc_naive(uniform float output[], uniform float input[], uniform int n) {
  foreach (i = 0 ... n) {
    float l, c, r;
    l=input[i == 0 ? n-1 : i-1];
    c=input[i];
    r=input[(i+1) % n];
    output[i] = (l + c + r)/3;
  }
}

export void relax_ispc(uniform float output[], uniform float input[], uniform int n) {
  if (programIndex == 0) {
    output[0] = (input[n-1]+input[0]+input[1])/3;
    output[n-1] = (input[n-2]+input[n-1]+input[0])/3;
  }
  foreach (i = 1 ... n-2) {
    float l, c, r;
    c=input[i];
    l=input[i-1];
    r=input[i+1];
    output[i] = (l + c + r)/3;
  }
}
