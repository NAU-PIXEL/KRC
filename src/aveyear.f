      REAL FUNCTION AVEYEAR (BLIQ,XLAT)
C_Titl  AVEYEAR  Average annual exposure of surface to sunlight. 
C  This is the average annual insolation, normalized to mean annual 
C solar flux over the orbit
C_Args
        REAL*4 BLIQ ! in. Obliquity in degrees.
C                       (the maximum sub-solar latitude on the planet)
        REAL*4 XLAT ! in. latitude on surface; in degrees. 
C
C_Desc formula from  WARD in  MARS p309 Eq 12
C Input angles should be no greater magnitude than 90. degrees, 
C  this routine does  not check them. But does force to positive.
C Approximate by numerical integration
C [Formula for mean annual insolation is Eq 11 p 308. 
C Note, near poles, result is sin(obliquity)/pi
C_Hist 2012mar09   Hugh_Kieffer
C_End
        REAL PI /3.141592653589793/

        INTEGER NP /360/    ! number of integration points
        INTEGER I ! looping
        REAL*4 D2R,DELA,THETA,CON1,CON2,DELP,SUM

        D2R=PI/180.             ! math constant: Degrees to radians

        DELA=D2R*ABS(XLAT)       ! del = latitude in radian
        THETA=D2R*ABS(BLIQ)     ! theta= oblquity in radian

        CON1=SIN(DELA)*COS(THETA) ! constant in integral
        CON2=COS(DELA)*SIN(THETA) ! constant in integral
        DELP=2.*PI/NP           ! increment in psi
        
        SUM=0.                  ! summation
        DO I=0,NP               ! crude numerical integration
           SINP=SIN(I*DELP)     ! sin psi
           SUM=SUM+SQRT(1.-(CON1-CON2*SINP)**2)
        ENDDO

C 1/(2 pi^2) * sum*delp=  1/(2 pi^2) * sum*(2 pi/N)= sum/(N pi)
       
        AVEYEAR= SUM/(NP*PI)    ! transfer to caller
        RETURN
        END
