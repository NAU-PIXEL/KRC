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
flux_table *build_table(char *filename, flux_booleans * bools) {
  bool success;

  flux_table *result = NULL;
  flux_table *building_flux_table = malloc(sizeof(flux_table));

  FILE *file = fopen(filename, "r");

  if (file == NULL) {
    result = NULL;
    free(building_flux_table);
    return result;
  }

  // with change in format we expect all tables to possibly have variable step
  // spacing
  building_flux_table->VERSION = 1;

  // rectangular tables have the same number of LT steps per day, and these
  // steps are all aligned to the same time steps. The local time steps may be
  // arbitrarily spaced. Note that this is accomplished by using the standard
  // data structure, which supports jagged arrays, but sets them to all be the
  // same length, and points to the same LT steps in each day.
  success = read_table(file, building_flux_table, bools);

  // assert(verify_table(building_flux_table));

  // assert(monotonic(building_jd_table->jd, building_jd_table->n_jd));

  result = building_flux_table;
  if (!success) {
    free(building_flux_table);
    result = NULL;
  }
  return result;
}

bool read_table(FILE *fp, flux_table *table, flux_booleans * bools) {
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

  success = read_header_columns(fp, building_table, bools);

  if (!success) {
    free(building_table);
    return false;
  }

  // what are cases if n_rows doesn't match the number of rows
  // more rows in file: won't error, just won't read them. that should be an
  // error. do an extra read and check for EOF
  for (int i = 0; i < building_table->n_rows; i++) {
    // the most likely error from this is a FEOF, haven't thought about how to
    // handle that yet
    success &= read_lt_row(fp, building_table, i, building_table->n_cols);
  }

  // make sure we've fully consumed the file
  if (fgetc(fp) != EOF) {
    success = false;
  }

  // make last timestamp the next integer after the last timestamp
  building_table->time[building_table->n_rows] =
      (int)building_table->time[building_table->n_rows - 1] + 1;
  // duplicate the first row at the end
  building_table->asol[building_table->n_rows] = building_table->asol[0];
  building_table->soldif[building_table->n_rows] = building_table->soldif[0];
  building_table->planv[building_table->n_rows] = building_table->planv[0];
  building_table->planh[building_table->n_rows] = building_table->planh[0];
  building_table->raw[building_table->n_rows] = building_table->raw[0];

  if (!success) {
    free(building_table->time);
    free(building_table->asol);
    free(building_table);
    return false;
  }

  table->lt_table = building_table;
  return true;
}

bool read_lt_header(FILE *fp, lt_fluxes *table) {
  table->n_rows = read_int(fp, '\n');

  // +1 for final table row that is repeat of first
  // this makes wrapping easier
  // not all columns are always going to exist, but it's much easier to just
  // allocate the memory now
  table->time = malloc(sizeof(double) * table->n_rows + 1);
  table->asol = malloc(sizeof(double) * table->n_rows + 1);
  table->soldif = malloc(sizeof(double) * table->n_rows + 1);
  table->planv = malloc(sizeof(double) * table->n_rows + 1);
  table->planh = malloc(sizeof(double) * table->n_rows + 1);
  table->raw = malloc(sizeof(double) * table->n_rows + 1);
  table->unordered_columns[0] = (table->time);
  table->unordered_columns[1] = (table->asol);
  table->unordered_columns[2] = (table->soldif);
  table->unordered_columns[3] = (table->planv);
  table->unordered_columns[4] = (table->planh);
  table->unordered_columns[5] = (table->raw);

  return true;
}

bool read_header_columns(FILE *fp, lt_fluxes *table, flux_booleans * bools) {
  // get the whole header row with fgets,
  // then chop it up properly using sgets and strcmp
  // to match to the header strings exactly
  int max_header_length = 255;
  char header_buffer[max_header_length];
  int header_count;

  char header1[20], header2[20], header3[20], header4[20], header5[20],
      header6[20], header7[20];
  char *header_results[7] = {header1, header2, header3, header4,
                             header5, header6, header7};
  char header_found1[20], header_found2[20], header_found3[20],
      header_found4[20], header_found5[20], header_found6[20],
      header_found7[20];
  char *already_matched[7] = {header_found1, header_found2, header_found3,
                              header_found4, header_found5, header_found6,
                              header_found7};

  fgets(header_buffer, max_header_length, fp);

  header_count = sscanf(header_buffer, "%s %s %s %s %s %s %s", header1, header2,
                        header3, header4, header5, header6, header7);

  bool matched_header = false;
  bool duplicate_header = false;

  for (int header_num = 0; header_num < header_count; header_num++) {
    for (int flux_compare = 0; flux_compare < FLUX_TYPE_COUNT; flux_compare++) {
      if (strcmp(header_results[header_num], FLUX_TABLE_TYPES[flux_compare]) ==
          0) {
        matched_header = true;
        strcpy(already_matched[header_num], FLUX_TABLE_TYPES[flux_compare]);
        table->ordered_columns[header_num] = table->unordered_columns[flux_compare];
        update_bools(bools, header_results[header_num]);

        for (int double_check = 0; double_check < header_num; double_check++) {
          if (strcmp(header_results[header_num],
                     already_matched[double_check]) == 0) {
            duplicate_header = true;
          }
        }
      }
    }
    if (!matched_header) {
      return false;
    }
  }
  if (duplicate_header) {
    return false;
  }

  table->n_cols = header_count;

  return true;
}

void update_bools(flux_booleans * bools, char * found_column) {
  if (strcmp(found_column, ASOL_STRING) == 0 ) {
    *(bools->asol) = true;
  }
  if (strcmp(found_column, SOLDIF_STRING) == 0 ) {
    *(bools->soldif) = true;
  }
  if (strcmp(found_column, PLANV_STRING) == 0 ) {
    *(bools->planv) = true;
  }
  if (strcmp(found_column, PLANH_STRING) == 0 ) {
    *(bools->planh) = true;
  }
  if (strcmp(found_column, RAW_STRING) == 0 ) {
    *(bools->raw) = true;
  }
}

bool read_lt_row(FILE *fp, lt_fluxes *table, int index, int num_columns) {
  int result;

  double res1, res2, res3, res4, res5, res6, res7;
  double *read_results[] = {&res1, &res2, &res3, &res4, &res5, &res6, &res7};
  // rows are columns in any order, with spaces to separate.
  // since we want to scan the newline we require the file to be newline
  // terminated
  result = fscanf(fp, read_strings[num_columns], &res1, &res2, &res3, &res4,
                  &res5, &res6, &res7);

  for (int i = 0; i < result; i++) {
    table->ordered_columns[i][index] = *read_results[i];
  }

  if (result == EOF) {
    return false;
  }
  return true;
}
