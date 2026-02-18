# include <stdbool.h>

typedef struct f_int_array {
  int *data;
  int size;
} f_int_array;

typedef struct f_real_array {
  double *data;
  int size;
} f_real_array;

typedef struct f_logical_array {
  bool *data;
  int size;
} f_logical_array;

typedef struct nd_int_array {
  int *data;
  int *dims;
  int ndims;
} nd_int_array;

typedef struct nd_real_array {
  double *data;
  int *dims;
  int ndims;
} nd_real_array;

f_int_array wrap_int_array(int *data, int size);

f_real_array wrap_float_array(double *data, int size);

f_logical_array wrap_logical_array(bool *data, int size);

nd_int_array wrap_nd_int_array(int *data, int *dims, int ndims);

nd_real_array wrap_nd_float_array(double *data, int *dims, int ndims);
