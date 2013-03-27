	SUBROUTINE EPHEMR
C_Titl  EPHEMR prints orbital position and date table.  PORB system
C_DESC
C computes heliocentric and planetocentric positions of planet and
C   the sun at uniform intervals of time.
C uses the current contents of  /PORBCM/,  so common must be set as desired
C_Call  PORB(ORBIT,ROTATE)  CALDATE  SPCREV  ANG360  [ATAN2]
C_HIST
C	76jul25  HUGH_H_KIEFFER  UCLA
C	78may27  HUGH_H_KIEFFER  U.S.G.S._Flagstaff
C	84may27  85jan13  85dec18(format only)
C	91may02  HHK format, use  ANG360, full  JD if before base time
C 2004sep21  HK replace   TYPE *,  with   WRITE(*,*) 
C 2012feb29  HK Replace  JDATE with  CALDATE 

	INCLUDE 'porbcm.inc'

	CHARACTER*28 TITLE
	REAL*4 MONTH

	RAD = 180./PICON
1	N2 = 0
	WRITE(*,*)'?* #TIMES, Date1 as MJD4, DEL. days'
	READ(IOK,*,END=9,ERR=1) N2,DJUL,DELJUL
	IF (N2.EQ.0) GO TO 9
2	WRITE(*,*)'?* ORBIT PERIOD, MJD4 OF ORBIT 0., TITLE'
	WRITE(*,*)'  NEGATIVE PERIOD IS SPACECRAFT NUMBER; JD0 IGNORED'
	WRITE(*,*)'  ZERO PERIOD; JD0 IGNORED.'
	READ(IOK,*,END=9,ERR=2) ORBPER,ORB0,TITLE
4	WRITE(IOP,320)
320	FORMAT('1')
	WRITE (IOP,301) DAYTIM,TITLE,N2,DJUL,DELJUL,ORBPER,ORB0
301	FORMAT(1X,5a4,4X,A,/' N2=',I4,' DJUL=',F10.2,' DELJUL=',F8.2
     &,'  ORBPER=',F10.4,' ORB0=',F12.4,1X,A)
	WRITE (IOP,300)
300	FORMAT('0YEAR MON  DAY   REV #  DIS A.U.  H.LON  H.DEC',
     & ' L.SUB S  S.DEC      MJD4  DAY DAY#'/)
	LINE=5
C--------------------------------------------------------top of loop 
	DO 404 I=1,N2
	DJU5=DJUL+FLOAT(I-1)*DELJUL
	CALL PORB (DJU5, DAU,PEA,PEB,HFA,HFB)
	CALL CALDATE (DJU5, IYEAR,IMON,IDAY,FDAY,MONTH,WDAY,IDATE)
	FDAY=FLOAT(IDAY)+FDAY
C compute rev number
	CALL SPCREV (ORBPER,ORB0,DJU5, REV)
	HDEC=PEA*RAD
	HLON= ANG360(PEB*RAD)
	SDEC=HFA*RAD
	TANOM= ATAN2 (PHOXX(2),PHOXX(1))
	SUBS=ANG360 ((TANOM+SLP)*RAD)
	IF (LINE.GT.57) THEN
		WRITE(IOP,320)
		WRITE(IOP,300)
		LINE=3
	    ENDIF
C include as many digits as needed to get positive  Julian  Day
C	IF (DJU5.LT.0.)DJU5=DJU5+BASE_JD5
C	IF (DJU5.LT.0.)DJU5=DJU5+BASE_JD6
C	IF (DJU5.LT.0.)DJU5=DJU5+BASE_JD7
	WRITE(IOP,302)IYEAR,MONTH, FDAY,REV,DAU,HLON,HDEC,SUBS,SDEC
     &,DJU5,WDAY,IDATE
302	FORMAT(1X,I4,A4,F6.2,F7.1,F10.6,2F7.2,1X,2F7.2,F10.2,1X,A4,I5)
404	LINE=LINE+1
C-----------------------------------------------------bottom of loop
	GO TO 1
9	RETURN
	END
