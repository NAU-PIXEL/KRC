	PROGRAM PORBMN
C main program of planetary orbit (porb) software system.
C  this system uses conical approximation to orbits.
C routines interface through /porbcm/ and a file of same name
C  containing several examples.
C most of the terminology and variable names are based on hugh kieffer's
C    geometry conventions (the same as used for irtm seed files).
C
Calls: porb1 porbio ephemr vector(rotax rotdia MPROD3)
C 85---- HHK convert to revised vector routines with input>output argument order
C@ hhk mid 75  l sub s corrected july 78. vax version 84may24  85JAN13
C 2009dec24 HK modify some names used in common

	INCLUDE 'porbcm.inc'

	DIMENSION PCOM(60)
	CHARACTER CHAR*1
	EQUIVALENCE (PCOM,PLANUM)
C Next 5 linse load items into Common
	IOK = 5
	IOS = 6
	IOP = 3
	IOD = 4
	PICON = 3.14159265	! 358979 

1	WRITE(IOS,*) '?* OUTPUT TO PRINTER OR TERMINAL: [P]/T?'
	READ(IOK,*,END=9,ERR=1) CHAR
	IF(CHAR.EQ.'T' .OR. CHAR.EQ.'t') THEN
		IOP=IOS
	    ELSE
C		UNIX has no equivalent to VMS ASSIGN statement
C		CALL ASSIGN(IOP,'PORB.PRT')
		OPEN(UNIT=IOP,FILE='porb.prt')
	    ENDIF
2	WRITE(IOS,3)
3	FORMAT(' ? Option Kode   0=quit'
     &/' 1 = Initiate COMMON from an orbital elements file'
     &/' 2 = Read/Save COMMON from/to ASCII or binary file '
     &/' 3 = print Ephemeris tables based upon current COMMON'
     &/' 4 = print COMMON'
     &/' 5 = test computation and print of rotation matrixes')
	READ(IOK,'(I1)', ERR=2,END=9)KODE
	IF(KODE.LT.1) GOTO 9	! quit
	IF(KODE.GT.5) GOTO 2	! invalid, ask again
	CALL DATIME (RUNTIM)
	WRITE(IOP,7)RUNTIM
7	FORMAT(' PORBMN:85JAN13  RUN_TIME=',5A4)
	GOTO (10,20,30,40,50),KODE
10	CALL PORB1
	GOTO 2
20	CALL PORBIO
	GOTO 2
30	CALL EPHEMR
	GOTO 2
40	WRITE (IOP,43)PCOM
43	FORMAT ('OPLANUM TC ODE ='/3G15.7/' CLIN ARGP ECC ='/3G15.7
     1/' SJA TJO PERIOD ='/3G15.7/' ZFQA ZFQB SIDAY ='/3G15.7
     2/' TLO OBL CQA ='   /3G15.7/' CQB --- --- ='    /3G15.7
     3/' ZFEB ZFEC XFEXB ='/3G15.7/' ARGV SLP ---'/3G15.7
     4/' PHOXX ='/3F15.7/' PHEXX ='/3F15.7/' HFXX ='/3F15.7
     5/' [FQ] =',3(/1X,3F15.7)
     6/' [EO] =',3(/1X,3F15.7)
     5/' [HO] =',3(/1X,3F15.7) )
	GOTO 2
50	CALL PORBQQ
	GOTO 2
C
9	STOP
	END
