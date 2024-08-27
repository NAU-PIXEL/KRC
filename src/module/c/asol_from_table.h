# include "array_structs.h"
# include <stdio.h>
# include <stdlib.h>

void insert_from_csv(f_real_array *arr, int insert_index);

double get_val(FILE *fp);
FILE *open_csv(int *status);
double read_csv();
