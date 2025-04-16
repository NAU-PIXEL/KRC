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

bool f_flux_init(char * file);

double f_get_jd_lt_flux(int search_jd, double search_lt, int flux_type);

double f_get_jd_lt_vis(int search_jd, double search_lt);

double f_get_jd_lt_ir(int search_jd, double search_lt);

#endif