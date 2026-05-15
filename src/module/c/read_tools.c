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


bool verify_table(flux_table *table) { return true; }