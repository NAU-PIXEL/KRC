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
        SUBROUTINE f_flux_init(filename, success, lasol, lsoldif, lplanv, lplanh, lraw) BIND(C)
            USE iso_c_binding
            LOGICAL(C_BOOL), INTENT(out) :: success, lasol, lsoldif, lplanv, lplanh, lraw
            CHARACTER(C_CHAR), INTENT(in) :: filename
          END SUBROUTINE f_flux_init

          FUNCTION f_get_jd_lt_asol(jd, lt) RESULT (flux) BIND(C)
            USE iso_c_binding
            ! the way we use the "jd" is actually as a season index, 
            ! because everywhere else day and L_s are floats, 
            ! and trying to translate them into an index doesn't make sense
            INTEGER(C_INT), INTENT(in), VALUE :: jd
            REAL(C_DOUBLE), INTENT(in), VALUE :: lt
            REAL(C_DOUBLE) :: flux
          END FUNCTION f_get_jd_lt_asol

          FUNCTION f_get_jd_lt_soldif(jd, lt) RESULT (flux) BIND(C)
            USE iso_c_binding
            ! the way we use the "jd" is actually as a season index, 
            ! because everywhere else day and L_s are floats, 
            ! and trying to translate them into an index doesn't make sense
            INTEGER(C_INT), INTENT(in), VALUE :: jd
            REAL(C_DOUBLE), INTENT(in), VALUE :: lt
            REAL(C_DOUBLE) :: flux
          END FUNCTION f_get_jd_lt_soldif

          FUNCTION f_get_jd_lt_planv(jd, lt) RESULT (flux) BIND(C)
            USE iso_c_binding
            ! the way we use the "jd" is actually as a season index, 
            ! because everywhere else day and L_s are floats, 
            ! and trying to translate them into an index doesn't make sense
            INTEGER(C_INT), INTENT(in), VALUE :: jd
            REAL(C_DOUBLE), INTENT(in), VALUE :: lt
            REAL(C_DOUBLE) :: flux
          END FUNCTION f_get_jd_lt_planv
          
          
          FUNCTION f_get_jd_lt_planh(jd, lt) RESULT (flux) BIND(C)
            USE iso_c_binding
            ! the way we use the "jd" is actually as a season index, 
            ! because everywhere else day and L_s are floats, 
            ! and trying to translate them into an index doesn't make sense
            INTEGER(C_INT), INTENT(in), VALUE :: jd
            REAL(C_DOUBLE), INTENT(in), VALUE :: lt
            REAL(C_DOUBLE) :: flux
          END FUNCTION f_get_jd_lt_planh
          
          FUNCTION f_get_jd_lt_raw(jd, lt) RESULT (flux) BIND(C)
            USE iso_c_binding
            ! the way we use the "jd" is actually as a season index, 
            ! because everywhere else day and L_s are floats, 
            ! and trying to translate them into an index doesn't make sense
            INTEGER(C_INT), INTENT(in), VALUE :: jd
            REAL(C_DOUBLE), INTENT(in), VALUE :: lt
            REAL(C_DOUBLE) :: flux
          END FUNCTION f_get_jd_lt_raw
          END INTERFACE
      END MODULE array_structs