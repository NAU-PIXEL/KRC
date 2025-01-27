# include "flux_search.h"

// searches for a sepcific JD in a table,
// then interpolates 
double get_jd_lt_flux(flux_table *flux_table, double search_jd, double search_lt) {
  int ls_index = 0;
  double result_flux;

  lt_fluxes * lt_fluxes;

  ls_index = search_for_jd(search_jd, flux_table->jd_table);

  if (ls_index == -1) {
    return -1;
  }

  lt_fluxes = flux_table->lt_tables[ls_index];

  int lt_index = search_for_lt(search_lt, lt_fluxes);

  result_flux = interpolate(
    lt_fluxes->flux[lt_index], lt_fluxes->flux[lt_index + 1],
    lt_fluxes->lt[lt_index], lt_fluxes->lt[lt_index + 1],
    search_lt);

  return result_flux;
}

int search_for_jd(double search_jd, jd_table *jd) {
  for (int i = 0; i < jd->n_jd; i++) {
    if (search_jd == jd->jd[i]) {
      return i;
    }
  }
  return -1;
}

int search_for_lt(double search_lt, lt_fluxes *lt_fluxes) {
  for (int i = 0; i < lt_fluxes->n_lt; i++) {
    if (search_lt == lt_fluxes->lt[i]) {
      return i;
    }
  }
  return -1;
}


double interpolate(double x1, double x2, double y1, double y2, double x_target) {
  // TODO 
}