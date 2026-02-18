#include "flux_load.h"

// overall process flow:
/*
1. Open file
2. Read header for version and number of rows
3. Init read loop, i = 0 to i = n_L_s
4. Error checking.
  a.


*/

// We're going to write at least this C right. Unit tests.

/*
Unit tests:

*/
flux_table * build_table(char *filename)
{
  bool success;

  flux_table *result = NULL;
  flux_table *building_flux_table = malloc(sizeof(flux_table));

  JD_TABLE_FORMAT format = JD_TABLE_UNKNOWN;

  FILE *file = fopen(filename, "r");

  if (file == NULL)
  {
    result = NULL;
    free(building_flux_table);
    return result;
  }

  // with change in format we expect all tables to possibly have variable step spacing
  format = JD_TABLE_JAGGED;
  building_flux_table->format = format;

  building_flux_table->VERSION = 1;

  // rectangular tables have the same number of LT steps per day, and these steps are all aligned to the same time steps. The local time steps may be arbitrarily spaced.
  // Note that this is accomplished by using the standard data structure, which supports jagged arrays, but sets them to all be the same length, and points to the same LT steps in each day.
  success = read_table(file, building_flux_table);

  // assert(verify_table(building_flux_table));

  // assert(monotonic(building_jd_table->jd, building_jd_table->n_jd));

  result = building_flux_table;
  if (!success) {
    free(building_flux_table);
    result = NULL;
  }
  return result;
}

bool read_table(FILE *fp, flux_table *table) {
  bool success;
  // read number of rows (header)
  // init the arrays for the rows
  // read the rows
  lt_fluxes *building_table = malloc(sizeof(lt_fluxes));

  success = read_lt_header(fp, building_table);

  if (!success) {
    free(building_table);
    return false;
  }
  
  // what are cases if n_lt doesn't match the number of rows
  // more rows in file: won't error, just won't read them. that should be an error. do an extra read and check for EOF
  for (int i = 0; i < building_table->n_lt; i++) {
    // the most likely error from this is a FEOF, haven't thought about how to handle that yet
    success &= read_lt_row(fp, building_table, i);
  }

  // make sure we've fully consumed the file
  if (fgetc(fp) != EOF) {
    success = false;
  }

  // make last timestamp the next integer after the last timestamp
  building_table->lt[building_table->n_lt] = (int)building_table->lt[building_table->n_lt - 1] + 1;
  // duplicate the first row at the end
  building_table->vis[building_table->n_lt] = building_table->vis[0];
  building_table->ir[building_table->n_lt] = building_table->ir[0];

  if (!success) {
    free(building_table->lt);
    free(building_table->vis);
    free(building_table->ir);
    free(building_table);
    return false;
  }

  table->lt_table = building_table;
  return true;
}

bool read_lt_header(FILE *fp, lt_fluxes *table)
{
  table->n_lt = read_int(fp, '\n');
  
  // +1 for final table row that is repeat of first
  // this makes wrapping easier
  table->lt = malloc(sizeof(double) * table->n_lt + 1);
  table->vis = malloc(sizeof(double) * table->n_lt + 1);
  table->ir = malloc(sizeof(double) * table->n_lt + 1);

  return true;
}


bool read_lt_row(FILE *fp, lt_fluxes *table, int index)
{
  int result;
  // lt rows are `jd.lt vis ir` with spaces to separate. 
  // since we want to scan the newline we require the file to be newline terminated
  result = fscanf(fp, "%lf %lf %lf\n", &table->lt[index], &table->vis[index], &table->ir[index]);
   
  if (result == EOF) {
    return false;
  }
  return true;
}
