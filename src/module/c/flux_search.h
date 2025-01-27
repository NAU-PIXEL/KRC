#ifndef FLUX_SEARCH_H
#define FLUX_SEARCH_H

#include "flux_structs.h"

double get_jd_lt_flux(flux_table *flux_table, double search_jd, double search_lt);

int search_for_jd(double search_jd, jd_table *jd);

int search_for_lt(double search_lt, lt_fluxes *lt_fluxes);

double interpolate(double x1, double x2, double y1, double y2, double x_target);

#endif