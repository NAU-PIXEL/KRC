#ifndef FLUX_LOAD_H
#define FLUX_LOAD_H

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "flux_structs.h"
#include "read_tools.h"
#include "flux_tools.h"

#define TIME_STRING "TIME"
#define ASOL_STRING "ASOL"
#define SOLDIF_STRING "SOLDIF"
#define PLANV_STRING "PLANV"
#define PLANH_STRING "PLANH"
#define RAW_STRING "RAW"

flux_table *build_table(char *filename, flux_booleans * bools);
bool read_table(FILE *fp, flux_table *table, flux_booleans * bools);
bool read_lt_header(FILE *fp, lt_fluxes *table);
bool read_lt_row(FILE *fp, lt_fluxes *table, int index, int num_columns);
bool read_header_columns(FILE *fp, lt_fluxes *table, flux_booleans * bools);
void update_bools(flux_booleans * bools, char * found_column);

const static char * const FLUX_TABLE_TYPES[] = {
  TIME_STRING,
  ASOL_STRING,
  SOLDIF_STRING,
  PLANV_STRING,
  PLANH_STRING,
  RAW_STRING
};

static const int FLUX_TYPE_COUNT = sizeof(FLUX_TABLE_TYPES) / sizeof(char *);


#define read_zero_double "\n"
#define read_one_double "%lf \n"
#define read_two_double "%lf %lf \n"
#define read_three_double "%lf %lf %lf \n"
#define read_four_double "%lf %lf %lf %lf \n"
#define read_five_double "%lf %lf %lf %lf %lf \n"
#define read_six_double "%lf %lf %lf %lf %lf %lf \n"
#define read_seven_double "%lf %lf %lf %lf %lf %lf %lf \n"

static const char * const read_strings[] = {
    read_zero_double,
    read_one_double,
    read_two_double,
    read_three_double,
    read_four_double,
    read_five_double,
    read_six_double,
    read_seven_double
};

#endif