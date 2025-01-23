# include "array_structs.h"
# include <stdio.h>
# include <stdlib.h>

enum table_select {
  QI,
  ATMRAD
} table_select;

void insert_from_csv(f_real_array *arr, int insert_index, enum table_select table);
double read_from_csv(enum table_select table);

void reset_csv(enum table_select table);

double get_val(FILE *fp);
FILE *open_atmrad(int *status);
FILE *open_qi(int *status);
double read_csv();

enum table_select get_qi_select_val();
enum table_select get_atmrad_select_val();