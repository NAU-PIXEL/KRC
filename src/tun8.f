      SUBROUTINE TUN8 (K15,ISTEP, I3,ARG4)
C_Titl  TUN8  Various special outputs as fortran text file
C_Vars  
      INCLUDE 'krcc8m.f'      ! has IMPLICIT NONE
      INCLUDE 'dayc8m.f'
      INCLUDE 'hatc8m.f'
C_Args
      INTEGER K15               !in. what kind of output + 100
      INTEGER ISTEP             !in.  phase: 1= write expected sizes  2=write data
      INTEGER I3                !in.  optional: meaning depends upon K15
      REAL*8 ARG4(*)            !in.  optional: meaning depends upon K15
C open/close of file fort.77 handled by FORTRAN system

C_DESC Shell for various special kinds of output, only one kind for any run.
C Each version is a text file written as fort.45; use must rename
C output is only for seasons that go to TDISK
C first line for each case indicate the expected size for this case

C 1: t(z) for each output hour
C 2: interface radient flux: direct and diffuse insolation, overhead and
C    diffuse sky temperature.
C_Hist 2014may31 Hugh Kieffer
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
      INTEGER I,J,K,KODE,NCOL
      REAL*8 DIRECT,QA,QH,QT,TSKY,TZEN

      KODE=K15-100              ! action code
      IF (ISTEP.EQ.1) THEN      ! write the dimensions
        I=N5-JDISK+1            ! expected number of seasons
        IF (KODE.EQ.1) NCOL=3+N1-1 ! total number of columns
        IF (KODE.EQ.2) NCOL=8
        WRITE(77,700) 4,NCOL,N24,N4,I,4, NRUN,NCASE ! dimensions: items,hours,lats,seasons
 700    format(8I6)
        GOTO 9                  ! do not write a data entry
      ENDIF

      IF (J5.LT.JDISK) GOTO 9 ! not past spinup

      IF (KODE.EQ.1) THEN
C KODE=1  Call each output hour 
C In krccom: j4 j5 n1       In daycom: TTJ
C I3 must be IH; output hour index
        WRITE(77,701)I3,J4,J5,(TTJ(I),I=2,N1) ! Layer temperatures
 701    FORMAT(3I4,50F8.3)
      ENDIF

      IF (KODE.EQ.2) THEN
C KODE=2:   Call at end of each final day. Use insolation values from before 
C TDAY call and atm temp from TAF in HATCOM
C ARG4 must be SOLDIF; all downward insolation except collimated
C Here, assume that N2 is an exact multiple of N24
        QA=BETA**0.25           ! BETA is in KRCCOM
        QH=(1.-EXP(-TAUIR))**0.25 ! TAUIR is in KRCCOM
        K = N2/N24              ! time steps between saving results
        DO J=1,N24              ! each output Hour
          I=J*K                 ! time step
          DIRECT=ASOL(I)-ARG4(I) ! arg4 is SOLDIF
          QT=TAF(J,J4)             ! TAF(hour,lat) is in HATCOM
          TSKY=QA*QT            ! 4-th root of BETA * TATM
          TZEN=QH*QT            ! 4'th root of 1-e^-tau_ir  *TATM
          WRITE(77,702) J,J4,J5,DIRECT,ARG4(I),QT,TZEN,TSKY
        ENDDO
 702    FORMAT(3I4,F9.3,4F8.3)
      ENDIF

 9    RETURN
      END
