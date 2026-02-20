#ifndef FLUX_TOOLS_H
#define FLUX_TOOLS_H

#include <stdbool.h>

bool monotonic(double *x, int n);

bool non_negative(double *x, int n);

double interpolate(double t1, double t2, double val1, double val2,
  double t_target);

#endif