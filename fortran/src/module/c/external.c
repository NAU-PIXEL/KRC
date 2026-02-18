#include "external.h"

bool f_flux_init(char * file) {
  bool status = false;
  if (!initialized) {
    global_table = build_table(file);
    if (global_table == NULL) {
      status = false;
    }
    else {
      status = true;
    }
    initialized = true;
  }
  return status;
}

double f_get_jd_lt_flux(int search_jd, double search_lt, int flux_type)
{
  return get_jd_lt_flux(global_table->lt_table, search_jd, search_lt, (FLUX_TYPE)flux_type);
}

double f_get_jd_lt_vis(int search_jd, double search_lt) 
{
  return f_get_jd_lt_flux(search_jd, search_lt, FLUX_VIS);
}


double f_get_jd_lt_ir(int search_jd, double search_lt) 
{
  return f_get_jd_lt_flux(search_jd, search_lt, FLUX_IR);
}
