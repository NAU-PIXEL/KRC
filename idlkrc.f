	Integer function idlkrc (kode)
C_Titl  IDLKRC  Interface to IDL. Planet surface thermal model  MGS-TES version
C     kode IN	Integer that controls action
C	0  = initiate
C	+ = start a run ;  > 1000 means mode will be coming, leave disk open
C     	- = continue this run
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'units.inc'
	INCLUDE 'filcom.inc'

	data izero /0/ 
	data im1 /-1/ 
	data im2 /-2/ 
	data im3 /-3/ 

	if (kode.eq.0) then	! initiate
C       set logical units
	  IOD1 = 1		! 	set logical units
	  IOD2 = 2
	  IOIN = 3
	  IOKEY = 5
	  IOPM = 6
	  IOSP = 7
	  IOERR = IOPM
	  LOPN2 = .FALSE.
	  LONE =.FALSE.		! true when in one-point mode
	  PI = 3.14159265	! 	set constants
	  RAD = 180./PI		! degrees/radian
	  SIGSB = 5.67051e-8	!  Stephan-Boltzman constant:  SI =  W m^-2 K^-4
	  CLOSE (IOIN)		! insurance, in case called twice from IDL
	  CLOSE (IOSP)
	  OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',err=81) !  open input file
C 	call U_std_init ('krc','1997-09-09','invoke traps') ! to invoke traps
	  OPEN (UNIT=IOSP,FILE=FOUT,err=82) !  open print file
	  CALL TCARD (1,IR) !  read and check a complete set of input parameters
	  idlkrc=ir		! return code as function
	  return
	endif
C				*****  BEGIN case  *****
	NCASE = mod(iabs(kode),1000)
	IQ = 1			! normal restart. transfer  flag  to  TSEAS
	if (kode.lt.0) then IQ =3 ! transfer "continuing" flag
	IF (LP1) CALL TPRINT (1) ! print program description
	CALL TDAY (1,IR)		! initialize day computations
	IF (.NOT.LONE .AND.(LP2 .OR. IR.NE.1)) CALL TPRINT (2) ! print inputs
	IF (IR.NE.1) THEN
		WRITE(IOSP,*)' PARAMETER ERROR IN TDAY(1)'
		IF (N5.GT.0) GOTO 83
	    ENDIF
	IF (IQ.LT.2) CALL R2R (TFROST,TTS4,-MAXN4) !  TLATS uses for atm rad.
	IF (.NOT.LOPN2 .AND. JDISK.GT.0) CALL TDISK (1,0) ! open output disk file
C======

	CALL TSEAS (IQ,IR)		! %%%%% execute season loop %%%%

C======
        IF (LONE) CALL TPRINT(9)
C close disk files if open and no-more coming
	if (JDISK.gt.0 .and. abs(kode).lt.1000) CALL TDISK (4,KREC)

	WRITE (IOSP,*)'     END KRC'
 9	idlkrc=ir
	return

C error section
 81	WRITE(IOERR,*)'KRC error opening input file =',FINPUT
	ir=im1
	GOTO 9
 82	WRITE(IOERR,*)'KRC error opening print file =',FOUT
	ir=im2
	GOTO 9
 83	WRITE(IOERR,*)'KRC TDAY error at middle season=',IR
	ir=im3
	GOTO 9
	END
