        REAL FUNCTION ALSUBS (KODE, ARG)
C_Titl  ALSUBS  Convert L-sub-s  <-> days into a Martian year
      IMPLICIT NONE
      INTEGER KODE ! IN. Controls which direction to do convertion
C    1:  Convert days from start of Martian year (L_s=0) to L_s
C    2:  Convert L_s to days (of 86400 seconds) into a martian year
      REAL ARG               ! IN  days into year (kode=1) or Ls (kode=2)
C Uses cosine series analytic approximation. Error < .01 degree L_s. ~ 1990.
C_Hist 2002mar07  Hugh_Kieffer Adopted from l_sub_s.pro
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      DOUBLE PRECISION F,X
      if (kode.eq.1) then       ! from Mars Day-of-year to L_s
        x=dble(arg)/686.98D0    ! fractional martian year
        x=x*2.0D0*3.1415926536D0 ! radians from start of martian year  
        f= -10.328371D0 +57.296001D0*x -10.688416D0*cos(x+3.2931221D0) 
     &       -0.62748339D0*cos(2.*x+5.0168313D0)
     &       -0.050566287D0 *cos(3.*x+0.41753547D0)
      ELSE             ! from degrees L_s to days since start of a Mars year
        x=0.017453292520D0*arg  ! convert degrees to radians
        f= 19.717923D0 + 109.33317D0*x + 20.417421D0*cos(x+3.4731399D0)
     &       -0.70853928D0*cos(2.*x+5.3719812D0) 
     &       + 0.029281483D0*cos(3.*x+0.92372042D0)
      ENDIF

      ALSUBS=REAL(F) ! convert back to sigle precision
      RETURN
      END
