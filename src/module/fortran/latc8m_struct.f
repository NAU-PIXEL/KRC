      MODULE latcm8m_struct
            USE array_structs
            INCLUDE '../../krcc8m.f'
            INCLUDE '../../latc8m.f'

            
            TYPE, BIND(C) :: LATCOM
                  integer(C_INT) :: NWLAT
                  type(REAL_ARRAY) :: DTM4
                  type(REAL_ARRAY) :: TST4
                  type(REAL_ARRAY) :: TTS4
                  type(REAL_ARRAY) :: TTB4
                  type(REAL_ARRAY) :: FROST4
                  type(REAL_ARRAY) :: AFRO4
                  type(REAL_ARRAY) :: TTA4
                  type(REAL_ARRAY) :: TTX4
                  type(REAL_ARRAY) :: TMN4
                  type(REAL_ARRAY) :: TIN
                  type(REAL_ARRAY) :: TAX
                  type(REAL_ARRAY) :: TSF
                  type(REAL_ARRAY) :: TPF
                  type(INT_ARRAY) :: NDJ4
            END TYPE LATCOM

            INTERFACE
                  FUNCTION wrap_latcom(NWLAT_local, DTM4_local, TST4_local, TTS4_local, TTB4_local, FROST4_local, AFRO4_local, TTA4_local, TTX4_local, TMN4_local, TIN_local, TAX_local, TSF_local, TPF_local, NDJ4_local) RESULT (C_LATCOM) BIND(C)
                        USE iso_c_binding
                        import :: LATCOM
                        TYPE(LATCOM) :: C_LATCOM
                        INTEGER(C_INT), INTENT(in), VALUE :: NWLAT_local
                        TYPE(C_PTR), INTENT(in), VALUE :: DTM4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TST4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TTS4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TTB4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: FROST4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: AFRO4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TTA4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TTX4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TMN4_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TIN_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TAX_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TSF_local
                        TYPE(C_PTR), INTENT(in), VALUE :: TPF_local
                        TYPE(C_PTR), INTENT(in), VALUE :: NDJ4_local
                  END FUNCTION wrap_latcom
            END INTERFACE

            CONTAINS
                  
      END MODULE latcm8m_struct

