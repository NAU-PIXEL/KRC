      SUBROUTINE PRTPCOM (KODE)
C_Titl  PRTPCOM  Print the PORB system version 2 common: porbcm
      INCLUDE 'porbcm.inc'	! has  IMPLICIT NONE
C_Args 
      INTEGER KODE              ! in. Control +1= first 30  +2=rest
C_HIST 2013jun20  Hugh Kieffer
C_End&___1_________2_________3_________4_________5_________6_________.72
        
      INTEGER IDEM2             ! set not in input matrix
      PARAMETER (IDEM2=20)      ! number of words therein 
      REAL*4 PCM2(IDEM2)          ! second part
      EQUIVALENCE (PCM2,PICON) 

      INTEGER I,I2,J
      CHARACTER*8 TI1(IDEM1) /'PLANUM','TC','ODE','CLIN','ARGP','ECC'
     2,'SJA','OBL','SFLAG','ZBAA','ZBAB','WDOT','W0','OPERIOD','TJP'
     3,'SIDAY','spar17','TAV','BLIP','spar20','spar21','BFRM:1','BFRM:2'
     4,'BFRM:3','BFRM:4','BFRM:5','BFRM:6','BFRM:7','BFRM:8','BFRM:9'/

C Above set by PORB system. Below set by main program and specific time

      CHARACTER*8 TI2(IDEM2) / 'PICON','R2D','PHFXX:1','PHFXX:2'
     2,'PHFXX:3','HPBXX:1','HPBXX:2','HPBXX:3','SPAV:1','SPAV:2'
     3,'SPAV3:1','RUNTIMa','RUNTIMb','RUNTIMc','RUNTIMd','RUNTIMe' 
     4,'IOK','IOS','IOP','IOD' /

      J=MOD(KODE,2)

      IF (J.EQ.1) THEN 
         WRITE(IOP,*)'  PORBCM 1:30'
         DO J=1,IDEM1,5
            I2=MIN(J+4,IDEM1)
            WRITE(IOP,31)(TI1(I),I=J,I2) 
            WRITE(IOP,32)(PCOM(I),I=J,I2) 
         ENDDO
      ENDIF
 31   FORMAT(5A12)
 32   FORMAT(5G12.5)
      
      IF (KODE.GE.2) THEN
         WRITE(IOP,*)'  PORBCM 31:60'
         DO J=1,IDEM2,5
            I2=MIN(J+4,IDEM2)
            WRITE(IOP,31)(TI2(I),I=J,I2) 
            WRITE(IOP,32)(PCM2(I),I=J,I2) 
         ENDDO
         WRITE (IOP,34) RUNTIM,PLANUM,TC
         WRITE (IOP,33) IOK,IOS,IOP,IOD
      ENDIF
 33   FORMAT('IOK,IOS,IOP,IOD=',4I8)
 34   FORMAT ('>',5a4,'=RUNTIME.  PLANUM AND TC= ',F6.0,F8.5)
        
      RETURN
      END
