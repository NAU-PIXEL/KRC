#include "flux_from_table.h"

#include <stdio.h>

double get_val(FILE *fp) {
  char buffer[64];
  char current_char;
  int buffer_loc = 0;
  do {
    current_char = fgetc(fp);
    buffer[buffer_loc] = current_char;
    buffer_loc++;
  } while (current_char != '\t' && !feof(fp));

  buffer[buffer_loc] = NULL;

  return atof(buffer);
}

FILE *open_qi(int *status) {
  static FILE *fp = NULL;
  if (fp == NULL) {
    fp = fopen("down_vis.csv", "r");
    if (fp == NULL) {
      printf("Could not open file\n");
    }
  }
  if (feof(fp)) {
    *status = 1;
  }
  return fp;
}

FILE *open_atmrad(int *status) {
  static FILE *fp = NULL;
  if (fp == NULL) {
    fp = fopen("down_ir.csv", "r");
    if (fp == NULL) {
      printf("Could not open file\n");
    }
  }
  if (feof(fp)) {
    *status = 1;
  }
  return fp;
}

void reset_csv(enum table_select table) {
  int status = 0;

  FILE *fp = NULL;

  switch (table) {
  case QI:
    fp = open_qi(&status);
    break;

  case ATMRAD:
    fp = open_atmrad(&status);
    break;

  default:
    printf("Invalid table selected\n");
    break;
  }

  rewind(fp);
}

// function to open csv and iterate through the values in it
double read_csv(enum table_select table) {
  int status = 0;
  FILE *fp = NULL;
  switch (table) {
  case QI:
    fp = open_qi(&status);
    break;

  case ATMRAD:
    fp = open_atmrad(&status);
    break;

  default:
    printf("Invalid table selected\n");
    break;
  }
  if (fp == NULL) {
    printf("Could not open file\n");
    return -1;
  }
  if (status != 0) {
    printf("Could not open file\n");
    return -1;
  }
  // lines can be very long, so read character by character
  // until we read a tab (separator) and then parse the current buffered string
  double res = get_val(fp);
  return res;
}

void insert_from_csv(f_real_array *arr, int insert_index,
                     enum table_select table) {
  // Fortran is 1 based, C is 0 based
  arr->data[insert_index - 1] = read_csv(table);
}

double read_from_csv(enum table_select table) {
  return read_csv(table);
}

enum table_select get_qi_select_val() { return QI; }

enum table_select get_atmrad_select_val() { return ATMRAD; }