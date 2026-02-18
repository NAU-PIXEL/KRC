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

double interpolate(double t1, double t2, double val1, double val2,
  double t_target) {
// this shouldn't ever happen, but saves us from a potential divide by zero if
// it does.
  if (t1 == t2) {
    return val1;
  }
  double slope = (val2 - val1) / (t2 - t1);
  return val1 + slope * (t_target - t1);
}
