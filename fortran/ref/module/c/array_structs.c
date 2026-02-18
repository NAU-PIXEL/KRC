# include <stdlib.h>
# include <stdio.h>

# include "array_structs.h"


// general procedure for these is to just take provided vars,
// which are passed from Fortran, allocate the struct, set pointers, and return
// the struct
f_int_array wrap_int_array(int *data, int size) {
  f_int_array wrapped_arr;
  wrapped_arr.data = data;
  wrapped_arr.size = size;
  return wrapped_arr;
}

f_real_array wrap_real_array(double *data, int size) {
  f_real_array wrapped_arr;
  wrapped_arr.data = data;
  wrapped_arr.size = size;
  return wrapped_arr;
}

f_logical_array wrap_logical_array(bool *data, int size) {
  f_logical_array wrapped_arr;
  wrapped_arr.data = data;
  wrapped_arr.size = size;
  return wrapped_arr;
}

// is there a way to get the dims automatically?
// yes, shape(arr) in Fortran
nd_int_array wrap_nd_int_array(int *data, int *dims, int ndims) {
  nd_int_array wrapped_arr;
  wrapped_arr.ndims = ndims;
  wrapped_arr.dims = dims;
  wrapped_arr.data = data;
  return wrapped_arr;
}

nd_real_array wrap_nd_real_array(double *data, int *dims, int ndims) {
  nd_real_array wrapped_arr;
  wrapped_arr.ndims = ndims;
  wrapped_arr.dims = dims;
  wrapped_arr.data = data;
  return wrapped_arr;
}
