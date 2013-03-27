	SUBROUTINE PORB1
C read orbital elements from disk file 
C computes orbital elements and rotation matrices of date into common
CAlls: oblix, vector pkg
C_HIST  Hugh Kieffer  mid-1975  78may28  79may11. vax version 84may24  85jan13
C	85sep13; combine all PORBEx into one, REVIS CALLED ARGUMENT ORDER
C 2009dec14  HK minor changes to allow 4th file code
C 2009dec24  HK change PERIOD and PI names to agree with revised porbcm
C 2012mar02  HK

	INCLUDE 'porbcm.inc'

	CHARACTER TITLE*40
C
	PI2=PICON/2.
	TWOPI=2.*PICON
C
1	WRITE (IOS,*) ' ?* SOURCE DATA: 0=STOP'
	WRITE (IOS,*) ' 1=SEIDEL,2=STURMS,3=BOWELL 4=minor '
	READ (IOK,*,END=9,ERR=1) INCODE
	IF (INCODE.LT.1) RETURN
11	WRITE (IOS,*) ' ?* WHICH item in file.'
	READ (IOK,*, END=9,ERR=11) IPLAN
	IF (INCODE.NE.3) THEN
2	WRITE (IOS,3)
3	FORMAT (' ?* EPOCH DESIRED AS CENTURIES AFTER 1950.0')
	READ (IOK,*,ERR=2,END=9)TC
	  ENDIF
	CALL PORBEL (IOP,IOD,INCODE,IPLAN,TC, ODE,CLIN,ARGP
     &,ECC,PERIOD,SJA,SIDAY,ZFQB,ZFQA,TJO,TITLE)
	OPERIOD=PERIOD		! transfer to common
	WRITE (IOS,*)TITLE
	WRITE (IOP,*)' INPUT FILE WAS: ',TITLE
	OBL= .40931975	! earths obliquity in radians
	CQA=0.
	CQB=0.
C----------------------------Calculations-----------------------------

	PLANUM=100.*incode+IPLAN
C get planets pole in ecliptic coord., and location of planets vernal equinox
	CALL OBLIQ (ZFQA,ZFQB,CLIN,ODE,OBL,ZFEB,ZFEC,XFEXB,ARGV)
C get obliquity: planes pole relative to planet orbit normal
	BLIP=OBLIP(ZFQB,ZFQA,ODE,CLIN,.FALSE.) ! angles all radians
C set offset for l-sub-s
	SLP=ARGP-ARGV+SLC
C compute: fq=to planet equator-ecliptic from earth equitorial
	CALL ROTDIA (1., FQ)
	CALL ROTAX  (1,OBL, FQ)
	CALL ROTAX  (3,ZFEB+PI2, FQ)
	CALL ROTAX  (1,ZFEC, FQ)
	CALL ROTAX  (3,XFEXB+PICON, FQ)
C from planet orbit to heliocentric ecliptic
	CALL ROTDIA (1., EO)
	CALL ROTAX  (3,-ARGP, EO)
	CALL ROTAX  (1,-CLIN, EO)
	CALL ROTAX  (3,-ODE, EO)
C switch to sun around planet
	DO 80 I=1,9
80		HO(I)=-EO(I)
C to planet fixed
	CALL ROTAX  (3,ZFEB+PI2, HO)
	CALL ROTAX  (1,ZFEC, HO)
	CALL ROTAX  (3,XFEXB+PICON, HO)
9	RETURN
	END
