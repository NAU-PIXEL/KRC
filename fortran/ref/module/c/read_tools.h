#ifndef READ_TOOLS_H
#define READ_TOOLS_H

#include <stdlib.h>
#include <stdio.h>

#include "flux_structs.h"

int read_int(FILE *fp, char delimiter);

double read_double(FILE *fp, char delimiter);

bool read_delimiter(FILE *fp, char delimiter);

bool read_double_row(FILE *fp, double *row, int n);

JD_TABLE_FORMAT read_table_format(FILE *fp);

#endif