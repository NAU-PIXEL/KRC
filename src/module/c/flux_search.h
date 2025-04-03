#ifndef FLUX_SEARCH_H
#define FLUX_SEARCH_H

#include "flux_structs.h"

double get_jd_lt_flux(lt_fluxes *flux_table, int search_jd, double search_lt, FLUX_TYPE flux_type);

int search_for_time(double search_jd_lt, lt_fluxes *flux_table);

double interpolate(double t1, double t2, double val1, double val2,
    double t_target);
#endif