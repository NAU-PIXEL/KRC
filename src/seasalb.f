      REAL FUNCTION SEASALB (LSUB)
C_Titl  SEASALB:  seasonally variable soil albedo
      IMPLICIT  NONE
      INTEGER*4 MROW,MSKIP
      PARAMETER (MROW=362)      ! max number of table entries
      PARAMETER (MSKIP=10)      ! max number of lines to skip through c_end
C_Vars
      INCLUDE 'units.inc'
      INCLUDE 'filcom.inc'

C_Args
      REAL*4 LSUB               !in. season; l-sub-s in degrees.
C  If large negative, will read file and save values
C_Desc
C initally reads an text table of two colums; l-sub-s and albedo
C for each positive date request . does linear interpolation, with wrap around.
C output will be negative if error occured
C_Hist  Hugh_Kieffer  2006sep09
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C local variables
      CHARACTER*80 RBUF
      REAL*4 XXX(MROW)          ! holds the table of  Ls in dgrees
      REAL*4 YYY(MROW)          ! holds albedo on that date
      INTEGER*4 I,KK                ! number of defined dates
      REAL*4 F,G,OUT
      REAL*4 RNDEX,RTERP
      INTEGER*4 READTXT360
      SAVE XXX,YYY,KK
      OUT=-1.                   ! possible error flag

      IF (LSUB .LT. -90.)  THEN ! read the file
D        WRITE (*,*) 'SEASALB FVALB=', FVALB
        KK=READTXT360(FVALB,IOD3,XXX,YYY)
D        WRITE (*,*) 'SEASALB kk=', KK
D       DO I=1,KK
D          WRITE (*,*) I,XXX(I),YYY(I)
D        ENDDO
        OUT=KK
        IF (KK.LT. 1)
     +          WRITE(IOERR,*)'SEASALB error opening input file =',FVALB

        ELSE                    ! interpolate

          G=AMOD(LSUB,360.)     ! insure within 0. to 360.
          F=RNDEX(G,XXX,KK)     ! finds floating-point index within a r*4 array
          OUT=RTERP(YYY,F)      ! linear interpolation of array at a real index
      ENDIF

 9    SEASALB=OUT
D     WRITE (*,*) 'SEASALB lsub,g,f,out=', LSUB,G,F,OUT
      RETURN
      END

