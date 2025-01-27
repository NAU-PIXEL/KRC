#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "read_tools.h"

int read_int(FILE *fp, char delimiter) {
  int ret;
  int n = fscanf(fp, "%d", &ret);
  if (n != 1) {
    fprintf(stderr, "Error: Could not read an integer from file.\n");
    exit(1);
  }
  char read_delimiter;
  n = fscanf(fp, "%c", &read_delimiter);
  if (n != 1 || read_delimiter != delimiter) {
    fprintf(stderr, "Error: Could not read '%c' from file.\n", delimiter);
    exit(1);
  }
  return ret;
}

double read_double(FILE *fp, char delimiter) {
  double ret;
  int n = fscanf(fp, "%lf", &ret);
  if (n != 1) {
    fprintf(stderr, "Error: Could not read a double from file.\n");
    exit(1);
  }
  char read_delimiter;
  n = fscanf(fp, "%c", &read_delimiter);
  if (n != 1 || read_delimiter != delimiter) {
    fprintf(stderr, "Error: Could not read '%c' from file.\n", delimiter);
    exit(1);
  }
  return ret;
}

bool read_delimiter(FILE *fp, char delimiter) {
  char read_delimiter;
  int n = fscanf(fp, "%c", &read_delimiter);
  if (n != 1 || read_delimiter != delimiter) {
    fprintf(stderr, "Error: Could not read '%c' from file.\n", delimiter);
    exit(1);
  }
  return true;
}

bool read_double_row(FILE *fp, double *row, int n) {
  for (int i = 0; i < n; i++) {
    row[i] = read_double(fp, ' ');
  }
  return read_delimiter(fp, '\n');
}

JD_TABLE_FORMAT read_table_format(FILE *fp) {
  JD_TABLE_FORMAT format = JD_TABLE_UNKNOWN;
  // scan to end of line into string buffer
  char buffer[64];
  if (fgets(buffer, 64, fp) == NULL) {
    fprintf(stderr, "Error: Could not read table format from file.\n");
    exit(1);
  }
  // null terminate string to remove '\n'
  else {
    buffer[strlen(buffer) - 1] = '\0';
  }

  for (int i = 0; i < JD_TABLE_FORMAT_COUNTS; i++) {
    if (strcmp(buffer, JD_TABLE_FORMAT_STRINGS[i]) == 0) {
      format = (JD_TABLE_FORMAT)JD_TABLE_UNKNOWN + 1 + i;
    }
  }
  return format;
}

jd_table *read_jd_table(FILE *fp) {
  jd_table *table = malloc(sizeof(jd_table));
  table->n_jd = read_int(fp, '\n');
  table->jd = malloc(sizeof(double) * table->n_jd);

  read_double_row(fp, table->jd, table->n_jd);
  return table;
}

lt_fluxes *read_rect_fluxes(FILE *fp, int n_lt_tables) {
  lt_fluxes *building_lt_tables = malloc(sizeof(lt_fluxes) * n_lt_tables);
  int n_lt = read_int(fp, '\n');
  double *lt = malloc(sizeof(double) * n_lt);

  read_double_row(fp, lt, n_lt);

  for (int i = 0; i < n_lt_tables; i++) {
    building_lt_tables[i].lt = lt;
    building_lt_tables[i].n_lt = n_lt;

    building_lt_tables[i].flux = malloc(sizeof(double) * n_lt);
    read_double_row(fp, building_lt_tables[i].flux, n_lt);
  }
  return building_lt_tables;
}

lt_fluxes *read_jagged_fluxes(FILE *fp, int n_lt_tables) {
  lt_fluxes *building_lt_tables = malloc(sizeof(lt_fluxes) * n_lt_tables);
  for (int i = 0; i < n_lt_tables; i++) {
    int n_lt = read_int(fp, '\n');
    double *lt = malloc(sizeof(double) * n_lt);

    read_double_row(fp, lt, n_lt);

    building_lt_tables[i].lt = lt;
    building_lt_tables[i].n_lt = n_lt;

    building_lt_tables[i].flux = malloc(sizeof(double) * n_lt);
    read_double_row(fp, building_lt_tables[i].flux, n_lt);
  }
  return building_lt_tables;
}

bool verify_table(flux_table *table) { return true; }