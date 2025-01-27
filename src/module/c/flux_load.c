#include "flux_load.h"

// overall process flow:
/*
1. Open file
2. Read header for number of L_s steps
3. Init read loop, i = 0 to i = n_L_s
  a. Read header row, L_s, n_lt, allocate struct
  b. read LT row
  c. read flux row
4. Error checking.
  a.


*/

// We're going to write at least this C right. Unit tests.

/*
Unit tests:

*/
jd_table *build_table(char *filename)
{
  flux_table *result = NULL;
  flux_table *building_flux_table = malloc(sizeof(flux_table));

  JD_TABLE_FORMAT format = JD_TABLE_UNKNOWN;

  FILE *file = fopen(filename, "r");

  if (file == NULL)
  {
    result = NULL;
    return result;
  }

  format = read_table_format(file);

  if (format == JD_TABLE_UNKNOWN)
  {
    fprintf(stderr, "Error: Unknown table format.\n");
    exit(1);
  }
  
  // rectangular tables have the same number of LT steps per day, and these steps are all aligned to the same time steps. The local time steps may be arbitrarily spaced.
  // Note that this is accomplished by using the standard data structure, which supports jagged arrays, but sets them to all be the same length, and points to the same LT steps in each day.
  jd_table *building_jd_table = read_jd_table(file);
  building_flux_table->jd_table = building_jd_table;
  if (format == JD_TABLE_RECTANGULAR) {
    building_flux_table->lt_tables = read_rect_fluxes(file, building_jd_table->n_jd);
  }

  if (format == JD_TABLE_JAGGED) {
    building_flux_table->lt_tables = read_jagged_fluxes(file, building_jd_table->n_jd);
  }

  // assert(verify_table(building_flux_table));

  // assert(monotonic(building_jd_table->jd, building_jd_table->n_jd));

  result = building_flux_table;
  return result;
}

bool read_jd_header(FILE *fp, jd_table *table)
{

  table->n_jd = read_int(fp, '\n');

  table->jd = malloc(sizeof(double) * table->n_jd);

  // for (int i = 0; i < table->n_jd; i++)
  // {
  //   *table->jd[i] = read_double(fp, ',');
  // }

  return read_delimiter(fp, '\n');
}

bool read_lt_header(FILE *fp, lt_fluxes *table)
{
  table->n_lt = read_int(fp, '\n');

  table->lt = malloc(sizeof(double) * table->n_lt);

  return true;
  // return read_delimiter(fp, '\n');
}


bool read_lt(FILE *fp, lt_fluxes *table)
{
  return read_double_row(fp, table->lt, table->n_lt);
}


bool read_flux(FILE *fp, lt_fluxes *table)
{
  return read_double_row(fp, table->flux, table->n_lt);
}
