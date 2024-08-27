      MODULE array_structs
        USE iso_c_binding
        IMPLICIT NONE

        TYPE, BIND(C) :: INT_ARRAY
        TYPE(C_PTR) :: data
        INTEGER(C_INT) :: size
        END TYPE INT_ARRAY

        TYPE, PUBLIC, BIND(C) :: REAL_ARRAY
        TYPE(C_PTR) :: data
        INTEGER(C_INT) :: size
        END TYPE REAL_ARRAY

        TYPE, BIND(C) :: LOGICAL_ARRAY
        TYPE(C_PTR) :: data
        INTEGER(C_INT) :: size
        END TYPE LOGICAL_ARRAY

        TYPE , BIND(C) :: ND_INT_ARRAY
          INTEGER(C_INT) :: ndim
          TYPE(C_PTR) :: dims
          TYPE(C_PTR) :: data
        END TYPE ND_INT_ARRAY

        TYPE , BIND(C) :: ND_REAL_ARRAY
          INTEGER(C_INT) :: ndim
          TYPE(C_PTR) :: dims
          TYPE(C_PTR) :: data
        END TYPE ND_REAL_ARRAY

        INTERFACE 
          FUNCTION wrap_int_array(data, size) RESULT (c_int_array) BIND(C)
            USE iso_c_binding
            import :: INT_ARRAY
            TYPE(INT_ARRAY) :: c_int_array
            TYPE(C_PTR), INTENT(in), VALUE :: data
            INTEGER(C_INT), INTENT(in), VALUE :: size
          END FUNCTION wrap_int_array

          FUNCTION wrap_real_array(data, size) RESULT (c_real_array) BIND(C)
            USE iso_c_binding
            import :: REAL_ARRAY
            TYPE(REAL_ARRAY) :: c_real_array
            TYPE(C_PTR), INTENT(in), VALUE :: data
            INTEGER(C_INT), INTENT(in), VALUE :: size
          END FUNCTION wrap_real_array
          FUNCTION wrap_logical_array(data, size) RESULT (c_logical_array) BIND(C)
            USE iso_c_binding
            import :: LOGICAL_ARRAY
            TYPE(LOGICAL_ARRAY) :: c_logical_array
            TYPE(C_PTR), INTENT(in), VALUE :: data
            INTEGER(C_INT), INTENT(in), VALUE :: size
          END FUNCTION wrap_logical_array

          FUNCTION wrap_nd_int_array(data, dims, ndim) RESULT (c_nd_int_array) BIND(C)
            USE iso_c_binding
            import :: ND_INT_ARRAY
            TYPE(ND_INT_ARRAY) :: c_nd_int_array
            TYPE(C_PTR), INTENT(in), VALUE :: data
            TYPE(C_PTR), INTENT(in), VALUE :: dims
            INTEGER(C_INT), INTENT(in), VALUE :: ndim
          END FUNCTION wrap_nd_int_array

          FUNCTION wrap_nd_real_array(data, dims, ndim) RESULT (c_nd_real_array) BIND(C)
            USE iso_c_binding
            import :: ND_REAL_ARRAY
            TYPE(ND_REAL_ARRAY) :: c_nd_real_array
            TYPE(C_PTR), INTENT(in), VALUE :: data
            TYPE(C_PTR), INTENT(in), VALUE :: dims
            INTEGER(C_INT), INTENT(in), VALUE :: ndim
          END FUNCTION wrap_nd_real_array

          SUBROUTINE insert_from_csv(c_real_array, insert_index) BIND(C)
            USE iso_c_binding
            import :: REAL_ARRAY
            TYPE(REAL_ARRAY), INTENT(inout) :: c_real_array
            INTEGER(C_INT), INTENT(in), VALUE :: insert_index
          END SUBROUTINE insert_from_csv

          SUBROUTINE reset_csv() BIND(C)
            use iso_c_binding
          END SUBROUTINE reset_csv
        END INTERFACE
      END MODULE array_structs