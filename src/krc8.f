      PROGRAM KRC
C_Titl  KRC planet surface thermal model  MGS-TES version
C_Vars
      INCLUDE 'krcc8m.f'      ! has IMPLICIT NONE
      INCLUDE 'latc8m.f'
      INCLUDE 'dayc8m.f'
      INCLUDE 'units.inc'
      INCLUDE 'filcom.inc'
C_Calls  CATIME  R2R  TCARD8  TDAY8  TDISK8  TPRINT8  TSEAS8 
C_Hist  85oct01  Hugh_H_Kieffer  Initial version circa 1969 at UCLA
C 87nov22  HHK  send errors to screen, force parameter print if error.
C 93mar03  ECisneros Convert include filename to lowercase. Replace assign 
C   statement with an open statement, changed iokey variable from -4 to 5
C 93mar04  EC Removed excess code at end of program; add check for IEEE 
C   exceptions. previous version was running but giving IEEE exception.
C 97jan30  HHK Correct zero initialize
C 97feb11  HHK Revise commons and much alogorithm
C 97jul07  HHK From calories to  SI units
C 99dec07  HHK Add option to continue from current condition
C 2002mar07 HHK Add option for "one-point" rapid runs for Surface T
C 2006mar22 HHK Minor: change default input file to   krc.inp
C 2009mar05 HK Minor cleanup
C 2010jan11 HK Change to use of IMPLICIT NONE
C 2013jan16 HK Eliminate the multiple use of IR. Jan31: add version number 2.1.2
C 2013Feb16 HK Remove unused PORBCM
C 2013jul24 HK Start using new PORB system to fix Ls problems of v2.1.2 
C 2014jan27 HK If fatal error, try next case rather than terminating run.
C 2014feb26 HK Untabify and justify
C 2013mar10 HK Make  REAL*8  version    Change name from krc.f
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      REAL ELAPSED,TIMES(2)     ! declare the types of DTIME()
      INTEGER IOD, IQ, IRC,IRD,IRS,KREC
      INTEGER IOST              ! returned by OPEN
      CHARACTER*80 CBUF         ! temporary use
      REAL TOTIME               ! Total Elapsed Time 

      VERSIN='KRCv3.2.1'        ! set version number 2014mar10  12 bytes
      KREC=84+20                ! number of bytes in TITLE +DAYTIM
      IOD1=NUMFD*2+NUMID+NUMLD+2*MAXN4*2+104/4 ! * 4-byte words
      IF (MOD(KREC,8).NE.0 .OR. MOD(IOD1,2).NE.0) THEN 
        PRINT *,'BAD lengths',KREC,IOD1
        STOP
      ENDIF

      IF (2.GT.3) THEN  ! VVVVVVVVVVVVVV test code
        WRITE(6,*) 'This is 1 ',VERSIN !!! held in FILCOM
        print *, 'NW: KRC LAT DAY ',NWKRC,NWLAT,NWDAY !!!
        print *,'kre,iode temp.=',krec,iod1      
        print *,'DAYTIM>', DAYTIM,'<' !!
        print *,'N1K', (N1K(IQ),IQ=1,MAXBOT) !!!
        print *,'NDJ4', (NDJ4(IQ),IQ=33,38) !!
        DO IQ=1,MAXBOT 
          N1K(IQ)=IQ            ! fill the end of DAYCOM
          NDJ4(IQ+32)=IQ        ! fill the end of latCOM
        enddo
        daytim='12345678901234567890'

        print *,'DAYTIM>', DAYTIM,'<' !!
        print *,'N1K', (N1K(IQ),IQ=1,MAXBOT) !!!
        print *,'NDJ4', (NDJ4(IQ),IQ=33,38) !!!
C                       zero out some commons
        CALL R2R (0.,ALB, -2*NWKRc +1) !  KRCCOM
        WRITE(6,*) 'This is  2',VERSIN !!! held in FILCOM
        VERSIN='KRCv3.1.2'      !!! set version number 2014mar10
        WRITE(6,*) 'This is 3 ',VERSIN !!! held in FILCOM
        CALL R2R (0.,DTM4,-2*NWLAT +2) !  LATCOM
        WRITE(6,*) 'This is 4 ',VERSIN !!! held in FILCOM
        VERSIN='KRCv3.1.3'      !!! set version number 2014mar10
        WRITE(6,*) 'This is 5 ',VERSIN !!! held in FILCOM
        CALL R2R (0.,XCEN,-2*NWDAY +2) !  DAYCOM
        WRITE(6,*) 'This is 6 ',VERSIN !!! held in FILCOM
        print *,'DAYTIM>', DAYTIM,'<' !!
        print *,'N1K', (N1K(IQ),IQ=1,MAXBOT) !!!
        print *,'NDJ4', (NDJ4(IQ),IQ=33,38) !!!
      ELSE   ! ^^^^^^^^^^^^^^^^^^^^^^
C Zero out commons
        CALL R2R (0.,ALB, -2*NWKRC ) !  KRCCOM
        CALL R2R (0.,DTM4,-2*NWLAT ) !  LATCOM
        CALL R2R (0.,XCEN,-2*NWDAY ) !  DAYCOM
      ENDIF

      FDISK='krc.tdi'           ! default output file name
C               set logical units. See   units.com   for description
      IOD1 = 1
      IOD2 = 2
      IOIN = 3                  ! Assign specific values to logical units
      IOKEY= 5                  !   defined in units.inc
      IOPM = 6                  ! "
      IOSP = 7                  ! "
      IOERR = IOPM              ! "
      LOPN2 =.FALSE.            ! set to: no disk file open
      LONE  =.FALSE.            ! set to: Not one-point mode
C                       set constants
      PIVAL = 3.14159265D0        ! pi
      RADC = 180.D0/PIVAL         ! degrees/radian
      SIGSB = 5.67051D-8        !  Stephan-Boltzman constant:  SI =  W m^-2 K^-4
      RGAS = 8.3145D0             ! ideal gas constant  (MKS=J/mol/K)
C in Fortran 90, the following 3 could use intrinsic functions 
      HUGE = 3.3D38             ! largest   REAL constant  F90 HUGE
      TINY = 2.0D-38            ! smallest  REAL constant  F90 TINY
      EXPMIN = 86.80D0          ! neg exponent that would almost cause underflow
      KREC=0                    ! ensure it has a storage location
      NRUN=0                    ! no output file yet
      NCASE=0                   ! initate this
      WRITE(IOPM,*) 'This is ',VERSIN
C                       open files: input, print, save
 50   FINPUT = 'krc.inp'        ! default input file name
      WRITE (IOPM,*)'?* Input file name or / for default =',FINPUT
      READ (IOKEY,*,ERR=50,end=9) FINPUT
C     print *,'FINPUT=',FINPUT 
      OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',err=81)
C     print *,'FINPUT2=',FINPUT 

C     call U_std_init ('krc','1997-09-09','invoke traps') ! to invoke traps

 60   FOUT = 'krc.prt'          ! default print file name
      WRITE (IOPM,*)' ?* Print file name or / for default =',FOUT
      READ (IOKEY,*,ERR=60,end=9) FOUT
      OPEN (UNIT=IOSP,FILE=FOUT,err=82)
C                       read and check a complete set of input parameters
D       write(iosp,*) 'before TCARD LP2=',LP2 !<<< debug 
      CALL CATIME (CBUF)
      DAYTIM=CBUF(2:21)
      PRINT *,'DAYTIM >',DAYTIM,'<' !!!
      CALL TCARD8 (1,IRC)
D       write(iosp,*) 'after TCARD IR,LP2=',IRC, LP2 !<<< debug
D       write(*,*) 'TCARD1 return=',IRC !<<< debug
      IF (IRC.GT.4) GO TO 170
      IF (LP1) CALL TPRINT8 (1)  ! print program description
      CALL DTIME(TIMES,ELAPSED) ! Start clock, GNU recommended form 
      TOTIME=0.
C                               *****  BEGIN case  *****
 140  NCASE = NCASE+1           ! have a case defined
      IF (IRC.EQ.4) THEN        ! Switch to "one-point" mode
        CLOSE(IOIN)             ! close the card input file
D         write(*,*)'FINPUT=',finput !<<< debug
        OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',iostat=iost,err=81)
D         write(*,*)' IOSTAT=',iost !<<< debug
        READ (IOIN,'(A80)',ERR=83,END=84) CBUF ! read Users title
D         write(*,*)' k2' !<<< debug
        WRITE(IOSP,*)'---- Start of one-point mode ----'
        WRITE(IOSP,*)CBUF       ! write users title
        READ (IOIN,'(A80)',ERR=83,END=84) CBUF ! skip the col header line
D         write(*,*)' k3' !<<< debug
        WRITE(IOSP,'(A,A)')'C_END  Ls   Lat  Hour Elev  Alb Inerti '
     &      ,'Opac Slop Azim  TkSur  TbPla             Comment'
        LONE=.TRUE.
        CALL TCARD8 (2,IRC)      ! read first one-point case
D       write(IOSP,*) 'KRC TCARD2 return=',IRC !<<< debug
      ENDIF
      IQ = IRC                  ! transfer "continuing" flag from  TCARD to  TSEAS
      IF (LONE) IQ=1            ! set TSEAS to start fresh
      CALL TDAY8 (1,IRD)         ! initialize day computations
D       write(IOSP,*)'KRC TDAY 1 return, LP2',IRD,LP2 !<<< debug
      IF (LP2 .OR. IRD.NE.1) CALL TPRINT8 (2)
C     IF (.NOT.LONE .AND. (LP2 .OR. IRD.NE.1)) CALL TPRINT (2) ! print input parameters
      IF (IRD.NE.1) THEN
        WRITE(IOSP,*)' PARAMETER ERROR IN TDAY(1)'
        IF (N5.GT.0) GOTO 170
      ENDIF

      IF (MAX(N5,1).GE.JDISK) THEN ! there may be some file output 
        IF (.NOT.LOPN2 .AND. JDISK.GT.0) CALL TDISK8 (1,0) ! open output disk file
        IF (I15.GT.100) CALL TUN8 (I15,1) ! Write case header
      ENDIF
D       write(*,*) 'cond=',cond !<<< debug
D       write(*,*)'KRC LP4=',LP4 !<<< debug
C      CALL CATIME (DAYTIM)      ! reset the time at start of each model
C======

      CALL TSEAS8 (IQ,IRS)       ! %%%%% execute season loop %%%%

C======
C If TSEAS called TCARD which had an error return (IR gt 4) then IRS will have 
C that value, and action will fall out the bottom of the season loop
 
D       write(*,*)'TSEAS return IQ,IRS,N5,krec=',IQ,IRS,N5,krec !<<< debug
      IF (LONE) CALL TPRINT8(9)  ! print results at requested one-point
      
      IF (IRS.NE.1 .AND. N5.GT.0) THEN ! There was a fatal error
! GO TO 170     ! stop on error in seasonal run  Changed 2014jan27
        WRITE(IOPM,*)'Case had FATAL error. Will try next case'
        WRITE(IOSP,*)'Case had FATAL error. Will try next case'
      ENDIF

      CALL TCARD8 (2,IRC)        ! read set of parameter changes

D       WRITE(IOSP,*)'TCARD 2 IR=',IRC,krec  !<<< Debug
      IF (IRS.EQ.3) NCASE=NCASE-1 ! do not increment case for continuing from memory
      IF (.NOT. LONE .OR. IRC.EQ.5 ) THEN ! 
        CALL DTIME(TIMES,ELAPSED) ! elapsed seconds
 133    FORMAT(1X,'Case',i3,2x,a1,'TIME: total, user, system=',3f10.4)
        WRITE(   *,133)NCASE,'D', ELAPSED,TIMES
        WRITE(IOSP,133)NCASE,'D', ELAPSED,TIMES
        TOTIME=TOTIME+ELAPSED   ! increment total time
      ENDIF
      IF (IRC.LT.5) GOTO 140    ! 5 is END of data
C
 170  IF (LOPN2) CALL TDISK8 (4,KREC) ! all done: close disk files

 9    WRITE (IOSP,*)'     END KRC   Total time [s]=',totime
      WRITE (IOPM,*)'     END KRC   Total time [s]=',totime
      STOP

C error section
 81   WRITE(IOERR,*)'KRC error opening input file =',FINPUT
      WRITE(IOERR,*) 'IOSTAT=',IOST
      GOTO 50
 82   WRITE(IOERR,*)'KRC error opening print file =',FOUT
      GOTO 60
 83   WRITE(IOERR,*)'KRC error reading one-point header lines',FDISK 
      GOTO 9
 84   WRITE(IOERR,*)'KRC unexpected EOF reading one-point header' 
      GOTO 9

      END
