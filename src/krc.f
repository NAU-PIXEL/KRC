	PROGRAM KRC
C_Titl  KRC planet surface thermal model  MGS-TES version
C_Vars
	INCLUDE 'krccom.inc' ! has IMPLICIT NONE
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'units.inc'
	INCLUDE 'filcom.inc'
	REAL PC(60)	
	COMMON /PORBCM/ PC
C_Hist	85oct01  Hugh_H_Kieffer
C	87nov22  HHK  send errors to screen, force parameter print if error.
C	93mar03  ECisneros converted include filename to lowercase, also
C		           replace assign statement with an open statement,
C			   and changed iokey variable from -4 to 5
C	93mar04  ECisneros removed excess code at end of program, and added
C		           check for ieee exceptions. previous version was
C			   running but giving ieee exception.
C 97jan30  HHK correct zero initialize
C 97feb11 revise commons and much alogorithm
C 97jul07 from calories to  SI units
C 99dec07  HHK add option to continue from current condition
C 2002mar07 HHK Add option for "one-point" rapid runs for Surface T
C 2006mar22 HHK Minor: change default input file to   krc.inp
C 2009mar05 HK Minor cleanup
C 2010jan11 HK Change to use of IMPLICIT NONE
C 2013jan16 HK Eliminate the multiple use of IR
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

	REAL ELAPSED,TIMES(2)	! declare the types of DTIME()
	INTEGER IOD, IQ, IRC,IRD,IRS,KREC
	INTEGER IOST		!? returned by OPEN
	CHARACTER*80 CBUF	! temporary use
C			zero out some commons
	CALL R2R (0.,ALB, -NWKRC) !  KRCCOM
	CALL R2R (0.,NDJ4,-NWLAT) !  LATCOM
	CALL R2R (0.,XCEN,-NWDAY) !  DAYCOM
	FDISK='krc.tdi'
C		set logical units. See   units.com   for description
	IOD1 = 1
	IOD2 = 2
	IOIN = 3
	IOKEY= 5
	IOPM = 6
	IOSP = 7
	IOERR = IOPM
	LOPN2 = .FALSE.
	LONE =.FALSE.		! true when in one-point mode
C			set constants
	PIVAL = 3.14159265      ! pi
	RADC = 180./PIVAL	! degrees/radian
	SIGSB = 5.67051e-8	!  Stephan-Boltzman constant:  SI =  W m^-2 K^-4
	RGAS = 8.3145		! ideal gas constant  (MKS=J/mol/K)
	HUGE = 3.3E38		! largest   REAL constant
	TINY = 2.0E-38		! smallest  REAL constant
	EXPMIN = 86.80		! neg exponent that would almost cause underflow
	KREC=0			! ensure it has a storage location
	NRUN=0			! no output file yet
C			open files: input, print, save
50	FINPUT = 'krc.inp'
	WRITE (IOPM,*)'?* Input file name or / for default =',FINPUT
	READ (IOKEY,*,ERR=50,end=9) FINPUT
	OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',err=81)

CC	call U_std_init ('krc','1997-09-09','invoke traps') ! to invoke traps

60 	FOUT = 'krc.prt'
	WRITE (IOPM,*)' ?* Print file name or / for default =',FOUT
	READ (IOKEY,*,ERR=60,end=9) FOUT
	OPEN (UNIT=IOSP,FILE=FOUT,err=82)
C			read and check a complete set of input parameters
D	write(iosp,*) 'before TCARD LP2=',LP2 !<<< debug 
	CALL DATIME (DAYTIM)
	CALL TCARD (1,IRC)
D	write(iosp,*) 'after TCARD IR,LP2=',IRC, LP2 !<<< debug
D	write(*,*) 'TCARD1 return=',IRC !<<< debug
	IF (IRC.GT.4) GO TO 170
	IF (LP1) CALL TPRINT (1) ! print program description
	CALL DTIME(TIMES,ELAPSED) ! Start clock, GNU recommended form 
C				*****  BEGIN case  *****
 140	NCASE = NCASE+1		! have a case defined
	IF (IRC.EQ.4) THEN	! Switch to "one-point" mode
	  CLOSE(IOIN)		! close the card input file
D	  write(*,*)'FINPUT=',finput !<<< debug
	  OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',iostat=iost,err=81)
D	  write(*,*)' IOSTAT=',iost !<<< debug
	  READ (IOIN,'(A80)',ERR=83,END=84) CBUF ! read Users title
D	  write(*,*)' k2' !<<< debug
	  WRITE(IOSP,*)'---- Start of one-point mode ----'
	  WRITE(IOSP,*)CBUF	! write users title
	  READ (IOIN,'(A80)',ERR=83,END=84) CBUF ! skip the col header line
D	  write(*,*)' k3' !<<< debug
	  WRITE(IOSP,'(A,A)')'C_END  Ls   Lat  Hour Elev  Alb Inerti '
     &      ,'Opac Slop Azim  TkSur  TbPla             Comment'
	  LONE=.TRUE.
	  CALL TCARD (2,IRC)	! read first one-point case
D	write(IOSP,*) 'KRC TCARD2 return=',IRC !<<< debug
	ENDIF
	IQ = IRC		! transfer "continuing" flag from  TCARD to  TSEAS
	IF (LONE) IQ=1		! set TSEAS to start fresh
	CALL TDAY (1,IRD) 	! initialize day computations
D	write(IOSP,*)'KRC TDAY 1 return, LP2',IRD,LP2 !<<< debug
	IF (LP2 .OR. IRD.NE.1) CALL TPRINT (2)
C	IF (.NOT.LONE .AND. (LP2 .OR. IRD.NE.1)) CALL TPRINT (2) ! print input parameters
	IF (IRD.NE.1) THEN
		WRITE(IOSP,*)' PARAMETER ERROR IN TDAY(1)'
		IF (N5.GT.0) GOTO 170
	    ENDIF
	IF (.NOT.LOPN2 .AND. JDISK.GT.0 .AND. MAX(N5,1).GE.JDISK) 
     &    CALL TDISK (1,0) ! open output disk file
C======
D	write(*,*) 'cond=',cond	!<<< debug
D	write(*,*)'KRC LP4=',LP4 !<<< debug
        CALL DATIME (DAYTIM)	! reset the time at start of each model
	CALL TSEAS (IQ,IRS)		! %%%%% execute season loop %%%%
D	write(*,*)'TSEAS return IQ,IRS,N5,krec=',IQ,IRS,N5,krec !<<< debug
C======
	IF (LONE) CALL TPRINT(9) ! print results at requested one-point

	IF (IRS.NE.1 .AND. N5.GT.0) GO TO 170	! stop on error in seasonal run
	CALL TCARD (2,IRC)		! read set of parameter changes

D	WRITE(IOSP,*)'TCARD 2 IR=',IRC,krec  !<<< Debug
	IF (IRS.EQ.3) NCASE=NCASE-1 ! do not increment case for continuing from memory
	IF (.NOT. LONE .OR. IRC.EQ.5 ) THEN 	! 
	   CALL DTIME(TIMES,ELAPSED) ! elapsed seconds
 133	   FORMAT(1X,'Case',i3,2x,a1,'TIME: total, user, system=',3f10.4)
	   WRITE(   *,133)NCASE,'D', ELAPSED,TIMES
	   WRITE(IOSP,133)NCASE,'D', ELAPSED,TIMES
	ENDIF
	IF (IRC.LT.5) GOTO 140	! 5 is END of data
C
 170	IF (LOPN2) CALL TDISK (4,KREC)	! all done: close disk files

C	check for ieee exceptions raised
C	see page 127 in sun fortran manual, numerical computation guide
C???	CALL CHKMATH(IOSP)

 9	WRITE (IOSP,*)'     END KRC'
 	STOP

C error section
 81	WRITE(IOERR,*)'KRC error opening input file =',FINPUT
	WRITE(IOERR,*) 'IOSTAT=',IOST
	GOTO 50
 82	WRITE(IOERR,*)'KRC error opening print file =',FOUT
	GOTO 60
 83	WRITE(IOERR,*)'KRC error reading one-point header lines',FDISK 
	GOTO 9
 84	WRITE(IOERR,*)'KRC unexpected EOF reading one-point header' 
	GOTO 9

	END
