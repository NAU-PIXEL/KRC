      MODULE array_structs
        USE iso_c_binding
        IMPLICIT NONE

        ENUM, BIND(C)
          enumerator :: FLUX_VIS = 201
          enumerator :: FLUX_IR = 202
        END ENUM

        TYPE, BIND(C) :: INT_ARRAY
        TYPE(C_PTR) :: data
        INTEGER(C_INT) :: size
        END TYPE INT_ARRAY

        INTERFACE 
          FUNCTION f_flux_init(filename) RESULT (status) BIND(C)
            USE iso_c_binding
            LOGICAL(C_BOOL) :: status
            CHARACTER(C_CHAR), INTENT(in) :: filename
          END FUNCTION f_flux_init
          FUNCTION f_get_jd_lt_vis(jd, lt) RESULT (flux) BIND(C)
            USE iso_c_binding
            ! the way we use the "jd" is actually as a season index, 
            ! because everywhere else day and L_s are floats, 
            ! and trying to translate them into an index doesn't make sense
            INTEGER(C_INT), INTENT(in), VALUE :: jd
            REAL(C_DOUBLE), INTENT(in), VALUE :: lt
            REAL(C_DOUBLE) :: flux
          END FUNCTION f_get_jd_lt_vis

          FUNCTION f_get_jd_lt_ir(jd, lt) RESULT (flux) BIND(C)
            USE iso_c_binding
            ! the way we use the "jd" is actually as a season index, 
            ! because everywhere else day and L_s are floats, 
            ! and trying to translate them into an index doesn't make sense
            INTEGER(C_INT), INTENT(in), VALUE :: jd
            REAL(C_DOUBLE), INTENT(in), VALUE :: lt
            REAL(C_DOUBLE) :: flux
          END FUNCTION f_get_jd_lt_ir
          END INTERFACE
      END MODULE array_structs