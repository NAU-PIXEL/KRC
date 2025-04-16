#include "flux_search.h"

double get_jd_lt_flux(lt_fluxes *flux_table, int search_jd, double search_lt,
                     FLUX_TYPE flux_type) {
  // thinking about making this static to save between calls,
  // since the indexes should only increase this saves a linear search
  static int table_index = 0;
  double result_flux;
  // jd is integer component, lt is float from 0 to 1
  double search_jd_lt = (search_jd % (int)(flux_table->lt[flux_table->n_lt])) + search_lt;

  table_index = search_for_time(search_jd_lt, flux_table, &table_index);

  if (table_index == -1) {
    return -1;
  }

  switch (flux_type) {
  case FLUX_VIS:
    result_flux = interpolate(flux_table->lt[table_index], flux_table->lt[table_index + 1],
                              flux_table->vis[table_index], flux_table->vis[table_index + 1], 
                              search_jd_lt);
    break;

  case FLUX_IR:
    result_flux = interpolate(flux_table->lt[table_index], flux_table->lt[table_index + 1],
                              flux_table->ir[table_index], flux_table->ir[table_index + 1], 
                              search_jd_lt);
    break;

  default:
    result_flux = -1;
    break;
  }
  return result_flux;
}

int search_for_time(double search_jd_lt, lt_fluxes *flux_table, int *table_index) {
  int start_index = *table_index;
  bool looped = false;
  for (int working_index = start_index; (working_index != start_index) || !looped; working_index = (working_index + 1) % flux_table->n_lt) {
    // lt must start with 0
    // because the search lt will be 0.xxx > 0, says no that's greater than 0
    // returns 1, then does linear interpolation between i - 1 and i, i.e. 0 and
    // 1
    if (search_jd_lt <= flux_table->lt[working_index + 1] && search_jd_lt >= flux_table->lt[working_index]) {
      return working_index;
    }
    if (working_index == flux_table->n_lt - 1) {
      looped = true;
    }
  }
  return -1;
}

