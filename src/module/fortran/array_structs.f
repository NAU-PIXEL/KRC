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
          
          FUNCTION read_from_csv(table) RESULT (csv_val) BIND(C)
            use iso_c_binding
            TYPE(REAL*8) :: csv_val
            INTEGER(C_INT), INTENT(in), VALUE :: table
          END FUNCTION read_from_csv

          FUNCTION get_qi_select_val() RESULT (qi_select_val) BIND(C)
            use iso_c_binding
            TYPE(INTEGER) :: qi_select_val
          END FUNCTION get_qi_select_val

          FUNCTION get_atmrad_select_val() RESULT (atmrad_select_val) BIND(C) 
            use iso_c_binding
            TYPE(INTEGER) :: atmrad_select_val
          END FUNCTION get_atmrad_select_val

          SUBROUTINE insert_from_csv(c_real_array, insert_index, table) BIND(C)
            USE iso_c_binding
            import :: REAL_ARRAY
            TYPE(REAL_ARRAY), INTENT(inout) :: c_real_array
            INTEGER(C_INT), INTENT(in), VALUE :: insert_index, table
          END SUBROUTINE insert_from_csv

          SUBROUTINE reset_csv(select_table) BIND(C)
            use iso_c_binding
            INTEGER(C_INT), INTENT(in), VALUE :: select_table
          END SUBROUTINE reset_csv
        END INTERFACE
      END MODULE array_structs