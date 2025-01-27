#include "flux_tools.h"

bool monotonic(double *x, int n) {
  int i;
  bool monotonic = true;
  for (i = 1; i < n; i++) {
    if (x[i] < x[i - 1]) {
      monotonic = false;
      break;
    }
  }
  return monotonic;
}