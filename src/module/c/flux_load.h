#ifndef FLUX_LOAD_H
#define FLUX_LOAD_H

#include <stdio.h>
#include <stdbool.h>

#include "flux_structs.h"
#include "read_tools.h"
#include "flux_tools.h"

flux_table *build_table(char *filename);
bool read_table(FILE *fp, flux_table *table);
bool read_lt_header(FILE *fp, lt_fluxes *table);
bool read_lt_row(FILE *fp, lt_fluxes *table, int index);

#endif