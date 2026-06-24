#ifndef EXTERNAL_H
#define EXTERNAL_H

#include <stdbool.h>

#include "flux_structs.h"
#include "read_tools.h"
#include "flux_tools.h"
#include "flux_load.h"
#include "flux_search.h"

static flux_table * global_table;
static bool initialized = false;

void f_flux_init(char *file, bool *success, bool *asol, bool *soldif,
                 bool *planv, bool *atmrad, bool *planh, bool *raw);

double f_get_jd_lt_flux(int search_jd, double search_lt, int flux_type);

double f_get_jd_lt_asol(int search_jd, double search_lt);

double f_get_jd_lt_soldif(int search_jd, double search_lt);

double f_get_jd_lt_planv(int search_jd, double search_lt);

double f_get_jd_lt_atmrad(int search_jd, double search_lt);

double f_get_jd_lt_planh(int search_jd, double search_lt);

double f_get_jd_lt_raw(int search_jd, double search_lt);

#endif