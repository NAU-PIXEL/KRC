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
C 2013jul24 HK Revise to V2 PORB system
C_END

	INCLUDE 'porbcm.inc' ! has  IMPLICIT  NONE

	INTEGER I,LINE,N2 
 
	REAL*4 FDAY		!  CALDAT args.  The last two I*4 are 
	INTEGER *4 IYEAR,IMON,IDAY,IDATE, MONTH,WDAY ! " 4-byte Character

	REAL*4 DJUL,DELJUL,RADSDEC, DAU,SUBS,SDEC,TANOM
	REAL*4 ORBPER,ORB0,DJU5, REV !  SPCREV args

	CHARACTER*28 TITLE

1	N2 = 0
	WRITE(*,*)'?* #TIMES, Date1 as MJD, DEL. days'
	READ(IOK,*,END=9,ERR=1) N2,DJUL,DELJUL
	IF (N2.EQ.0) GO TO 9
2	WRITE(*,*)'?* ORBIT PERIOD, MJD4 OF ORBIT 0., TITLE'
	WRITE(*,*)'  NEGATIVE PERIOD IS SPACECRAFT NUMBER; JD0 IGNORED'
	WRITE(*,*)'  ZERO PERIOD; JD0 IGNORED.'
	READ(IOK,*,END=9,ERR=2) ORBPER,ORB0,TITLE
4	WRITE(IOP,320)
320	FORMAT('1')
	WRITE (IOP,301) RUNTIM,TITLE,N2,DJUL,DELJUL,ORBPER,ORB0
301	FORMAT(1X,5a4,4X,A,/' N2=',I4,' DJUL=',F10.2,' DELJUL=',F8.2
     &,'  ORBPER=',F10.4,' ORB0=',F12.4,1X,A)
	WRITE (IOP,300)
300	FORMAT('0YEAR MON  DAY   REV #  DIS A.U.  ',
     & ' L.SUB S  S.DEC      MJD4  DAY DAY#'/)
	LINE=5
C--------------------------------------------------------top of loop 
	DO 404 I=1,N2
	DJU5=DJUL+FLOAT(I-1)*DELJUL ! J2000 MJD
	CALL PORBIT (1, DJU5, SUBS,SDEC,DAU)
	CALL CALDATE (DJU5, IYEAR,IMON,IDAY,FDAY,MONTH,WDAY,IDATE)
	FDAY=FLOAT(IDAY)+FDAY
C compute rev number
	CALL SPCREV (ORBPER,ORB0,DJU5, REV) ! all args  R*4
	IF (LINE.GT.57) THEN
		WRITE(IOP,320)
		WRITE(IOP,300)
		LINE=3
	    ENDIF
	WRITE(IOP,302)IYEAR,MONTH, FDAY,REV,DAU,SUBS,SDEC
     &,DJU5,WDAY,IDATE
302	FORMAT(1X,I4,A4,F6.2,F7.1,F10.6,1X,2F7.2,F10.2,1X,A4,I5)
404	LINE=LINE+1
C-----------------------------------------------------bottom of loop
	GO TO 1
9	RETURN
	END
