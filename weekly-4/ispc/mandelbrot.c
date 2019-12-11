#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "timing.h"
#include "mandelbrot.h"

static inline int mandel(float c_re, float c_im, int count) {
  float z_re = c_re, z_im = c_im;
  int i;
  for (i = 0; i < count; ++i) {
    if (z_re * z_re + z_im * z_im > 4.)
      break;
    float new_re = z_re*z_re - z_im*z_im;
    float new_im = 2.f * z_re * z_im;
    z_re = c_re + new_re;
    z_im = c_im + new_im;
  }
  return i;
}

void mandelbrot_c(float x0, float y0,
                  float x1, float y1,
                  int width, int height,
                  int maxIterations,
                  int output[]) {
  float dx = (x1 - x0) / width;
  float dy = (y1 - y0) / height;

  for (int j = 0; j < height; j++) {

    for (int i = 0; i < width; i++) {
      float x = x0 + i * dx;
      float y = y0 + j * dy;

      // Explicit row-major indexing.
      int index = j * width + i;

      output[index] = mandel(x, y, maxIterations);
    }
  }
}

int main() {
  unsigned int width = 768, height = 512;
  float x0 = -2., x1 = 1.;
  float y0 = -1., y1 = 1.;
  int maxIterations = 256;
  int *buf_c = calloc(width*height, sizeof(int));
  int *buf_ispc = calloc(width*height, sizeof(int));

  int runtime;

  TIMEIT(runtime) {
    mandelbrot_c(x0, y0, x1, y1, width, height, maxIterations, buf_c);
  }
  printf("C:                 %8d microseconds\n", runtime);

  TIMEIT(runtime) {
    mandelbrot_ispc(x0, y0, x1, y1, width, height, maxIterations, buf_ispc);
  }
  printf("ISPC:              %8d microseconds\n", runtime);

  int ret = 0;
  for (unsigned int i = 0; i < width*height; i++) {
    if (buf_c[i] != buf_ispc[i]) {
      fprintf(stderr, "Difference at index %d: %d != %d\n", i, buf_c[i], buf_ispc[i]);
      ret = 1;
    }
  }

  return ret;
}
