#include "flux_search.h"

double get_jd_lt_flux(lt_fluxes *flux_table, int search_jd, double search_lt,
                     FLUX_TYPE flux_type) {
  // thinking about making this static to save between calls,
  // since the indexes should only increase this saves a linear search
  int table_index = 0;
  double result_flux;
  // jd is integer component, lt is float from 0 to 1
  double search_jd_lt = search_jd + search_lt;

  table_index = search_for_time(search_jd_lt, flux_table);

  if (table_index == -1) {
    return -1;
  }

  switch (flux_type) {
  case JD_FLUX_VIS:
    result_flux = interpolate(flux_table->lt[table_index - 1], flux_table->lt[table_index],
                              flux_table->vis[table_index - 1], flux_table->vis[table_index], 
                              search_lt);
    break;

  case JD_FLUX_IR:
    result_flux = interpolate(flux_table->lt[table_index - 1], flux_table->lt[table_index],
                              flux_table->ir[table_index - 1], flux_table->ir[table_index], 
                              search_lt);
    break;

  default:
    result_flux = -1;
    break;
  }
  return result_flux;
}

int search_for_time(double search_jd_lt, lt_fluxes *flux_table) {
  for (int i = 0; i < flux_table->n_lt; i++) {
    // lt must start with 0
    // because the search lt will be 0.xxx > 0, says no that's greater than 0
    // returns 1, then does linear interpolation between i - 1 and i, i.e. 0 and
    // 1
    if (search_jd_lt <= flux_table->lt[i]) {
      return i;
    }
  }
  return -1;
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
