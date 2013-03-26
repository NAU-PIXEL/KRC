	PROGRAM KRC
C_Titl  KRC planet surface thermal model  MGS-TES version
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'units.inc'
	INCLUDE 'filcom.inc'

C	COMMON /PORBCM/ PC(60)
C       the above common block is necessary for one of the binary output
C       runs - bin51?  r.fergason 09.11.03
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
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C			zero out commons
	CALL R2R (0.,ALB, -NWKRC)		!  KRCCOM
	CALL R2R (0.,NDJ4,-NWLAT)		!  LATCOM
	CALL R2R (0.,X, -NWDAY)			!  DAYCOM
	FDISK='krc.tdi'
C			set logical units
	IOD1 = 1
	IOD2 = 2
	IOIN = 3
	IOKEY = 5
	IOPM = 6
	IOSP = 7
	IOERR = IOPM
	LOPN2 = .FALSE.
	LONE =.FALSE.		! true when in one-point mode
C			set constants
	PI = 3.14159265
	RAD = 180./PI		! degrees/radian
	SIGMA = 5.67051e-8 !  Stephan-Boltzman constant:  SI =  W m^-2 K^-4
CC	SIGMA = 1.354518E-12 ! stephan-boltzman constant: cal:cm:sec:k
C			open files: input, print, save
50	FINPUT = 'krcin.inp'
	FINPUT = 'krcone.inp'
	WRITE (IOPM,*)'?* Input file name or / for default =',FINPUT
	READ (IOKEY,*,ERR=50,end=9) FINPUT
	OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',err=81)

CC	call U_std_init ('krc','1997-09-09','invoke traps') ! to invoke traps

60 	FOUT = 'krc.prt'
	WRITE (IOPM,*)' ?* Print file name or / for default =',FOUT
	READ (IOKEY,*,ERR=60,end=9) FOUT
	OPEN (UNIT=IOSP,FILE=FOUT,err=82)
C			read and check a complete set of input parameters
	CALL TCARD (1,IR)

	IF (IR.EQ.4) THEN	! Switch to "one-point" mode
	  CLOSE(IOIN)		! close the card input file
C	  write(*,*)'FINPUT=',finput
	  OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',iostat=iost,err=81)
C	  write(*,*)' IOSTAT=',iost
	  READ (IOIN,'(A80)',ERR=83,END=84) FDISK ! read Users title
C	  write(*,*)' k2'
	  WRITE(IOSP,*)'---- Start of one-point mode ----'
	  WRITE(IOSP,*)FDISK	! write users title
	  READ (IOIN,'(A80)',ERR=83,END=84) FDISK ! skip the col header line
C	  write(*,*)' k3'
	  WRITE(IOSP,*)'11    Ls   Lat  Hour Elev  Alb Inerti Opac '
	1      ,'Slop_Azim  TkSur  TbPla'
	  LONE=.TRUE.
	  CALL TCARD (2,IR)	! read first one-point case
	ENDIF
	IF (IR.GT.4) GO TO 170
C				*****  BEGIN case  *****
140	NCASE = NCASE+1
	IQ = IR	  	!transfer "continuing" flag from  TCARD to  TSEAS
	IF (LONE) IQ=1
	IF (LP1) CALL TPRINT (1)	! print program description
	CALL TDAY (1,IR)		! initialize day computations
	IF (LP2 .OR. IR.NE.1) CALL TPRINT (2)
C	IF (.NOT.LONE .AND. (LP2 .OR. IR.NE.1)) CALL TPRINT (2)		! print input parameters
	IF (IR.NE.1) THEN
		WRITE(IOSP,*)' PARAMETER ERROR IN TDAY(1)'
		IF (N5.GT.0) GOTO 170
	    ENDIF
	IF (IQ.LT.2) CALL R2R (TFROST,TTS4,-MAXN4) !  TLATS uses for atm rad.
	IF (.NOT.LOPN2 .AND. JDISK.GT.0 .AND. MAX(N5,1).GE.JDISK) 
     &    CALL TDISK (1,0) ! open output disk file
C======

	CALL TSEAS (IQ,IR)		! %%%%% execute season loop %%%%
C	write(*,*)'TSEAS return IQ,IR=',IQ,IR
C======
	IF (LONE) CALL TPRINT(9)

	IF (IR.NE.1 .AND. N5.GT.0) GO TO 170	! stop on error in seasonal run
	CALL TCARD (2,IR)		! read set of parameter changes
	IF (IR.EQ.3) NCASE=NCASE-1 ! do not increment case for continuing from memory
	IF (IR.LT.5) GOTO 140
C
 170	IF (LOPN2) CALL TDISK (4,KREC)	! all done: close disk files

C	check for ieee exceptions raised
C	see page 127 in sun fortran manual, numerical computation guide
C???	CALL CHKMATH(IOSP)

 9	WRITE (IOSP,*)'     END KRC'
 	STOP

C error section
 81	WRITE(IOERR,*)'KRC error opening input file =',FINPUT
	write(ioerr,*) 'IOSTAT=',iost
	GOTO 50
 82	WRITE(IOERR,*)'KRC error opening print file =',FOUT
	GOTO 60
 83	WRITE(IOERR,*)'KRC error reading one-point header lines',FDISK 
	GOTO 9
 84	WRITE(IOERR,*)'KRC unexpected EOF reading one-point header lines' 
	GOTO 9

	END



