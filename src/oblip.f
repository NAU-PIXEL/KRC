      REAL FUNCTION OBLIP (PRA,PDEC,ASCEND,CLIN,LDEG)
C_Titl  OBLIP  Obliquity of a planet.  Default precision
      IMPLICIT NONE
      REAL PRA                  !in.  Planet pole Right Ascension, degrees
      REAL PDEC                 !in. Planet pole declination, degrees
      REAL ASCEND            !in. Orbit: longitude of the rising node, degrees
      REAL CLIN                 !in. Orbit: inclination, degrees 
      LOGICAL LDEG              !in. true=In/output in degrees. Default radians 
C_Hist 2012mar02  Hugh Kieffer 
C_End
      REAL PI /3.141592653589793/
      REAL DTOR,ZLON,S1,S2,A3,COSOB,PIO2

      IF (LDEG) THEN            ! simplist way to handle dual units
         DTOR=PI/180.           ! math constant: degrees to radians
         PIO2= 90.              ! 1/4 turn
      ELSE
         DTOR=1.                ! input already in radians
         PIO2=PI/2.
      ENDIF
  
      ZLON= ASCEND-PIO2          !  RA of orbit pole, degrees

      S1= DTOR*(PIO2-PDEC)       ! co-latitude of spin pole, radian
      S2= DTOR*CLIN             ! co-latitude of orbit pole, radian
      A3= DTOR*(ZLON-PRA )  ! delta  RA between orbit pole and spin axis, radian

      COSOB=COS(S1)*COS(S2) + SIN(S1)*SIN(S2)*COS(A3) ! spherical law of cosines

      OBLIP= ACOS(COSOB)/DTOR  ! obliqity of planet, degrees

      RETURN
      END
