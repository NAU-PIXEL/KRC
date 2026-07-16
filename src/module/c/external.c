#include "external.h"

void f_flux_init(char *file, bool *success, bool *asol, bool *soldif,
                 bool *planv, bool *atmrad, bool *planh, bool *raw) {
  bool status = false;
  flux_booleans bools = (flux_booleans){.asol = asol,
                                        .soldif = soldif,
                                        .planv = planv,
                                        .atmrad = atmrad,
                                        .planh = planh,
                                        .raw = raw};
  if (!initialized) {
    global_table = build_table(file, &bools);
    if (global_table == NULL) {
      status = false;
    } else {
      status = true;
    }
    initialized = true;
  }
  *success = status;
}

double f_get_jd_lt_flux(int search_jd, double search_lt, int flux_type) {
  return get_jd_lt_flux(global_table->lt_table, search_jd, search_lt,
                        (FLUX_TYPE)flux_type);
}

double f_get_jd_lt_asol(int search_jd, double search_lt) {
  return f_get_jd_lt_flux(search_jd, search_lt, FLUX_ASOL);
}
double f_get_jd_lt_soldif(int search_jd, double search_lt) {
  return f_get_jd_lt_flux(search_jd, search_lt, FLUX_SOLDIF);
}
double f_get_jd_lt_planv(int search_jd, double search_lt) {
  return f_get_jd_lt_flux(search_jd, search_lt, FLUX_PLANV);
}

double f_get_jd_lt_atmrad(int search_jd, double search_lt) {
  return f_get_jd_lt_flux(search_jd, search_lt, FLUX_ATMRAD);
}

double f_get_jd_lt_planh(int search_jd, double search_lt) {
  return f_get_jd_lt_flux(search_jd, search_lt, FLUX_PLANH);
}

double f_get_jd_lt_raw(int search_jd, double search_lt) {
  return f_get_jd_lt_flux(search_jd, search_lt, FLUX_RAW);
}