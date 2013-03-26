	SUBROUTINE TLATS (IQ,IRL)
C_Titl  TLATS latitude computations for the  KRC thermal model system
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'hatcom.inc'
	INCLUDE 'units.inc'
C_Args
	INTEGER IQ  !in. not used
	INTEGER IRL !out return codes: 1=normal  2=blow-up
C_hist  1969 thru 97  Hugh_H_Kieffer
C   97feb11  HHK major revision
C   97may30  HHK 2nd version of atmospheric radiation
C   97jul22  HHK 3rd version of atmospheric radiation
C   97sep08  HHK add planetary temperatures
C   97jul07  HHK from calories to  SI units
C   99dec10  HHK revise insolation
C 2002jul12  HHK Revise atmosphere to use Delta-Eddington
C 2005nov18  HK Add optional solar xenith angle limit 
C 2005dec28  HK Fix bug using ZENLIM. Additional comments
C 2006jan25  HK Modify SKYFAC from linear with slope to (1+cos s)/2
C 2008sep27  HK fix error in G1: was MIN(1.,(90.-AINC)/ZENLIM))
C 2008oct01  HK Use slope azimuth as a flag for a pit of slope SLOPE
C 2008oct04  HK Fix transfer of wrong BETA to KRCCOM
C 2010jan12 HK Use IMPLICIT NONE, move TINT call from TLATS to TSEAS
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

	REAL DERI(2,2)		! diffuse irradiances from Delta-Eddington
	REAL COSI_AM(2)	! cos_incidence angle: average and noon
	REAL COLL,COL3,BOND	! returned by Delta-Eddington

      REAL*4 ACOSLIM,AH,AINC,ANGLE,AVE_A,AVE_E,ATMHEAT,AVEI,AVEH,BETH
     &,BOTDOWN,BOUNCE,CC,CC2,CD,CL,CL2,COSAM,COSI,COSZLIM,COS2,COS3
     &,DIFAC,DIFFUSE,DIRECT,DIRFLAT,EF,EFP,FACTOR,F23,FP,G0,G1,OMEGA
     &,PCAP,PFACTOR,PCO2G,PCO2M,QI,QA,QH,QHS,QS
     &,RADEAST,RANG,RLAT,RSDEC,SLONOR,SD,SL,SL2,SOL,SS,SS2
     &,TAEQ4,TATMAVE,TATMSIG,TBOT,TOPUP,TSEQ4,TSUR,TWILFAC,TWILIM,ZENLIM

      INTEGER*4 I,IH,IR, J,JE,JJ,JJH,JHOT,J3P1,KODE

      REAL*4 VLPRES,AVEDAY,CO2PT,EPRED ! Function names

	IF (IDB2.NE.0) WRITE(IOSP,*)'TLATSa',N1,N1PIB,N2,N24,J5

	IRL=1			! set return code to normal

C============ factors that do not depend upon season ===================
	RANG=2.0*PIVAL/N2		! time step expressed in radians
	F23=2./3.
        JHOT=INT(FLOAT(N24)*13.5/24.) ! index of warmest time of day
C
C radiation parameters:
C  ASOL = direct + diffuse insolation as a function of local time
	TWILFAC = 90./(90.+TWILI)     ! twilight factor
	TWILIM = COS((90.+TWILI)/RADC) ! minimum cosine i for twilight

C for sloping terrain
	SLONOR = SLOPE*COS(SLOAZI/RADC) ! north component of dip, degrees
	RADEAST = (SLOPE/RADC)*SIN(SLOAZI/RADC) ! east " " , radians
	IF (SLOAZI .GT. -360.) THEN ! slope is regional
 	   SKYFAC = (1.+ COS(SLOPE/RADC))/2. ! effective isotropic radiation.
	   COSZLIM=0.		! zenith angle default limit is 90 degrees
	ELSE			! slope is of conical pit wall
	   QA=(90.-SLOPE)/RADC	! zenith angle of slope, in radians
	   QI=SIN(QA)
	   SKYFAC = QI**2	! effective isotropic radiation.
	   COSZLIM=COS(QA)	! 
	ENDIF
C============ factors constant over latitude that depend upon season ==========

	SOL=SOLCON/(DAU*DAU)	! solar flux at this heliocentric range
	RSDEC=SDEC/RADC		! current solar declination
	SD=SIN(RSDEC)
	CD=COS(RSDEC)
C set blowup test to a factor larger than perpendicular black surface
	TBLOW = 2.0 *  ( SOL / (EMIS*SIGSB) )**0.25
	
C get current total pressure at 0 elevation
	IF (N4.GT.8) THEN	! use global integrations
	  PCAP = SUMF*GRAV	! cap_frost equivalent surface pressure
	ELSE
	  PCAP=0.
	ENDIF
	PCO2M = (1.-FANON)*PTOTAL ! initial partial pres. of  CO2 at 0 elev
	IF (KPREF.EQ.0) THEN ! constant 
	  PZREF = PTOTAL	 ! current total pressure at 0 elevation
	  PCO2G = PCO2M !  partial pres. of  CO2 at 0 elev. now
	ELSEIF (KPREF.EQ.1) THEN ! follows Viking
	  KODE=4		! average of all years and both landers
	  PZREF = PTOTAL*VLPRES(KODE, DJU5)! current total P at 0
	  PCO2G = PCO2M+(PZREF-PTOTAL) ! all changes are pure CO2
	ELSEIF (KPREF.EQ.2) THEN ! based on polar cap balance
	  PZREF = PTOTAL - PCAP
	  PCO2G = PCO2M -PCAP ! all changes are pure CO2
	ENDIF
C
	IF (LPGLOB) THEN	! print global properties
	   CALL TPRINT(8)	! print page heading
	  WRITE(IOSP,'(A,F10.4)')' GLOBAL AVERAGE FROST; kg/m^2 =',SUMF
	ENDIF

D 	write(42,*) n1,j3,j4,j5,n1pib     !dbw 42
	J4=0
C  ----------------new latitude loop loop loop--------------------------
C
100	J4=J4+1
	DLAT=ALAT(J4)		! current latitude
	RLAT=DLAT/RADC
	CL=COS(RLAT)
	SL=SIN(RLAT)
	SS=SL*SD
	CC=AMAX1(1.E-10,CL*CD)
C for slope
	RLAT=(DLAT+SLONOR)/RADC ! effective latitude in radians
	CL2=COS(RLAT)
	SL2=SIN(RLAT)
	SS2=SL2*SD
	CC2=AMAX1(1.E-10,CL2*CD)

	COSI_AM(2) = (SS+CC)	!  cos ( incidence angle at noon )
	COSAM=-SS/CC		!  find length of day and daylight intervals
	IF (ABS(COSAM).LT.1.) THEN
		DAM=RADC*ACOS(COSAM)
	    ELSEIF (COSAM.GE.1.) THEN
		DAM=0.
		COSI_AM(2) = 1.E-6 ! sun never rises
	    ELSE
		DAM=180.
	    ENDIF

	    IF (J5.EQ.1) THEN	! first season
	   TATMAVE=TATM		! diurnal average
	   EFROST = 0.		! frost on the ground
	ELSE			! use results from prior season
	   CALL SIGMA(TAF(1,J4),N24,TATMAVE,TATMSIG) ! diurnal average Tatm.
	   EFROST = FROST4(J4)	! starting frost amount
	ENDIF
	SCALEH = TATMAVE*RGAS/(AMW*GRAV) ! scale height in km
	PFACTOR = EXP(-ELEV(J4)/SCALEH) ! relative to global annual mean
	PRES = PZREF * PFACTOR		! current local total pressure 
	OPACITY = TAUD*PRES/PTOTAL	! scale tau with current local pressure
	ACOSLIM = AMAX1(OPACITY/EXPMIN,0.001) ! limit to avoid math checks
c not used?	SLOLIM = ACOS(ACOSLIM) 	      ! or 89.94 deg
	COSI_AM(1)= MAX(1.E-6,AVEDAY(SDEC,DLAT)) ! get average cosine incidence

	IF (LVFA) THEN		! use variable frost albedo
	  CALL ALBVAR (COSI_AM(1)*SOL, AFNOW)	! var. frost albedo
	ELSE
	  AFNOW = AFROST
        ENDIF
	IF (LVFT) THEN			! use variable frost temperature
	  TFNOW = CO2PT(1,PFACTOR*PCO2G)! get local frost temperature
	ELSE
	  TFNOW = TFROST
	ENDIF
	TATMIN = CO2PT(1,PFACTOR*PCO2G/2.71828)	! frost point for 1-layer atm
        IF (EFROST.LE.0.) THEN ! use proper emissivity and albedo
          AVE_E=EMIS
          AVE_A=ALB	! surface albedo; frost if frosty at end of prior day
        ELSE
          AVE_E=FEMIS
          AVE_A=AFNOW
        ENDIF

 777	FORMAT(9G12.5) ! default format
C insolation at mid-step and its average. time origin is midnight.
C  angle from noon is (j2/n2 * 2.pi) - pi, so cos of this is -cos(j2/n2 *2pi)
C  cosi = cosine of solar incidence angle onto horizontal
C  cos2 = cosine of solar incidence angle onto surface slope
C  cos3 = cosine of twilight angle onto horizontal
	AVEI=.0
	AVEH=.0
	OMEGA=DUSTA		! single scattering albedo
	G0=ARC2		! Henyey-Greenstein asymmetry parameter
	DIFAC=SKYFAC+AVE_A*(1.-SKYFAC) ! sky plus regional surface reflection
	IH = 1			! saving "hour" count
	AH = FLOAT(N2)/FLOAT(N24) ! time steps between saving results
	JJH = AH+.5		! round to time step of first saving
	DO JJ =1,N2 ! time of day loop.................................
	  ANGLE=(FLOAT(JJ)-0.5)*RANG ! angle at middle of time step
	  COSI= SS  -CC*COS (ANGLE) ! cos of incidence angle on horizontal
	  COS2= SS2 -CC2*COS (ANGLE+RADEAST) ! " " onto slope
C Get atmosphere transmission and heating for horizontal surface
	  IF (COSI.GT.ACOSLIM) THEN ! Day: Sun is above horizon
	    CALL DEDING2 (OMEGA,G0,AVE_A,COSI,OPACITY, BOND,COLL,DERI)
	    TOPUP  =PIVAL*(DERI(1,1)-F23*DERI(2,1)) ! diffuse flux up at top atm.
	    BOTDOWN=PIVAL*(DERI(1,2)+F23*DERI(2,2)) ! diffuse flux down at surf.
	    ATMHEAT=COSI-TOPUP-(1.-AVE_A)*(BOTDOWN+COSI*COLL) ! atmosph. heating
	    DIRFLAT=COSI*COLL	! collimated onto regional flat plane
	  ELSE			! set several values for night
	    ATMHEAT=0.
	    DIRFLAT=0.
	    TOPUP=0.
	    COLL=0.	
	  ENDIF
C Set direct surface insolation
	  IF (COS2.GT.COSZLIM) then ! target is directly illuminated
	    DIRECT=COS2*COLL	! slope is in sunlight
	  ELSE
	    DIRECT=0.		! Target is in shadow
	  ENDIF
C Get diffuse insolation, including twilight and first-order surface reflection 
	  IF (COSI.GT.TWILIM) THEN ! in day or twilight zone
	    COS3= COS(TWILFAC*ACOS(COSI)) ! twilight effective cosine
	    IF (COS3.GT.1.E-5) THEN
	       CALL DEDING2 (OMEGA,G0,AVE_A,COS3,OPACITY, BOND,COL3,DERI)
	       BOTDOWN=PIVAL*(DERI(1,2)+F23*DERI(2,2)) !diffuse flux down at surf
	    ELSE
	       BOTDOWN=0.
	    ENDIF
	    DIFFUSE=SKYFAC*BOTDOWN ! diffuse flux onto surface
	    IF (SLOAZI .LE. -360.) THEN ! bounce in a pit
	      AINC=ACOS(COSI)*RADC  ! incidence angle in degrees
	      G1=MIN(1.,(90.-AINC)/SLOPE) ! (90-i)/slope
	    ELSE
	      G1=1.0
	    ENDIF 
	    BOUNCE=(1.-SKYFAC)*AVE_A*(G1*DIRFLAT+DIFFUSE)
	  ELSE
	    DIFFUSE=0.
	    BOUNCE=0.
	  ENDIF

	  QI=(DIRECT+DIFFUSE+BOUNCE)*SOL ! solar onto slope surface
 997	  IF (QI.NE.QI)WRITE(IOERR,*)'TLatc',JJ,COSI,COS3,DIRECT,DIFFUSE 
	  QA=ATMHEAT*SOL	! solar heating of atm.
	  ASOL(JJ)=QI		! total insolation onto slope surface
	  ADGR(JJ)=QA
	  AVEI=AVEI+QI
	  AVEH=AVEH+QA
	  IF (LD19) WRITE(79,777) QA,QI,DIRECT,DIFFUSE,BOUNCE
	  IF (JJ.EQ.JJH) THEN	!  JJH is next saving hour
	    TOFALB(IH,J4)=TOPUP
	    IH = IH+1		! increment to next hour
	    JJH = IH*AH+.5	! next time-step to save
	  ENDIF
	ENDDO			! end time of day loop....................
C
C  find equilibrium temperature at current latitude
	TAUIR=(CABR+TAUD*TAURAT)*(PRES/PTOTAL)! thermal opacity, zenith
	FACTOR= 1.50307 -0.121687*ALOG(TAUIR) ! from fit to hemisphere integrals
	FACTOR=AMIN1(2.0,AMAX1(1.0,FACTOR)) ! asymptotic limits
	TAUEFF=FACTOR*TAUIR
	BETA=1.-EXP(-TAUEFF)	! hemispheric thermal absorption of atmosphere
	AVEH=AMAX1(AVEH/FLOAT(N2),0.)	! average atm. solar heating
	AVEI=AMAX1((1.-AVE_A)*AVEI/FLOAT(N2),0.) ! average absorbed insolation
	TAEQ4=(AVEH/BETA+AVEI)/(SIGSB*(2.-AVE_E*BETA)) ! equilib T_a^4
	TSEQ4=BETA*TAEQ4+AVEI/(SIGSB*AVE_E) ! equilib T_s^4
	TEQUIL = AMAX1( TSEQ4,0.)**0.25	! equilib T_s
	IF (TEQUIL.LT.TFNOW) TEQUIL=TFNOW
	JJO=1
C if at start, use linear profile, else  continuing from prior season
	IF (J5.LE.1) THEN	! start with linear profile
	  IF (N3.GT.3) JJO=.75*N2+1.5	! start first day just past 3/4 day
	  TSUR=TEQUIL		!  isothermal if bottom is insulating
	  TBOT=TEQUIL		!  "  "
	  IF(IB.GE.1) TBOT=TDEEP! case for constant bottom  T
	  IF(IB.GT.1) TSUR=TDEEP! case for isothermal initial condition
	  DO  I=1,N1
	    TTJ(I)=TSUR+(TBOT-TSUR)*(XCEN(I)-XCEN(1))/(XCEN(N1)-XCEN(1))
	  ENDDO
	  TTJ(N1PIB)=TBOT
	  TTS(1)=TEQUIL
	  TTB(1)=TEQUIL
	  TATMJ=TAEQ4**0.25
	ELSE	! start with final value from previous season
	  DO  I=1,N1PIB
 	    TTJ(I)=TMN4(I,J4)
	  ENDDO
	  TTS(1)=TTS4(J4)
	  TTB(1)=TTB4(J4)
	  TATMJ=TTA4(J4)	! predicted final atm temp from prior season
	ENDIF
D	write(iosp,*)'tauir,taueff,BETA=',tauir,taueff,BETA
D	write(iosp,*)'aveh,avei=',aveh,avei
D	write(iosp,*)'tatmj,tequil=',tatmj,tequil
C	write(iosp,*) asol
C	write(iosp,*) adgr
	IF (LP3) CALL TPRINT (3) ! print header for hourly summary
C======

	CALL TDAY (2,IR)		! execute day loop

C======
	IF (IR.EQ.2) THEN		! if blow-up; stop the model
	  IRL=2
	  GOTO 9
	ENDIF
C  save results for current latitude
C
	J3P1=J3+1
C	i=n24/2			! noonish
D	write(44,344) j3,j4,j5,ncase,efrost,ave_a,taud,pres !dbw 44
D    & ,DTMJ(J3),DTMJ(J3P1),TMIN(2),TMAX(2) !dbw 44
D 344	 format(i3,i3,i4,i3,f12.6,f12.9,f12.9,f12.6,2f12.9,2f12.6) 
C
	TST4(J4)=TEQUIL
	DTM4(J4)=DTMJ(J3P1) !tday: RMS change of layer temperatures in prior day
	NDJ4(J4)=J3
	DO  I=1,N24
	  TSF(I,J4)=TSFH(I)
	  TPF(I,J4)=TPFH(I)
	ENDDO
	DO  I=1,N1		! save extrema for each layer
	   TIN(I,J4)=TMIN(I)	!   and predict next season's
	   TAX(I,J4)=TMAX(I)	!   temperatures.
	ENDDO

	FP=DELJUL/PERIOD-J3 ! undone iterations to end of this season.
C  The 4 items predicted are stored for the end of the day prior to their index;
C so they are defined up to J3P1.  DTMJ(1)==-1.
C  If 2 or more new days were computed this season, do aymptotic prediction.
C  If have 1 new day, do linear predication; signaled by negative extrapol.
	IF (J3.GE.2) THEN	! have at least 3 points in array to predict
	  JE=J3-1
	ELSEIF (FP.LT.0.1) THEN ! can use 2-points
	  FP=FP-1.		! force  EPRED into 2-point mode
	  JE=1
	ELSE
	  JE=-1			! turn off any interpolation
	ENDIF
	IF (JE.GE.1) THEN	! can use extrapolation
	   DO  I=1,N1		! predict next season's temperatures
	      TMN4(I,J4)= EPRED(TT1(I,JE),FP,MAXN1,TFNOW,TBLOW)
	   ENDDO
	   TTS4(J4)   = EPRED(TTS(JE),FP,1,TFNOW,TBLOW) ! surface average
	   TTB4(J4)   = EPRED(TTB(JE),FP,1,TFNOW,TBLOW) ! bottom layer average
	   TTA4(J4)   = EPRED(TTA(JE),FP,1,TFNOW,TBLOW) ! end-of-day Atm 
	   EFP        = EPRED(FRO(JE),FP,1,0.,9999.)
	 ELSE			! extrapolation not possible
	   DO  I=1,N1		!  start next season where this one ended
	      TMN4(I,J4)= TTJ(I)
	   ENDDO
	   TTS4(J4) = TTS(J3P1)
	   TTB4(J4) = TTB(J3P1)
	   TTA4(J4) = TTA(J3P1)
	   EFP      = FRO(J3P1)
	ENDIF
	IF (IB.GE.1) TMN4(N1PIB,J4)=TTJ(N1PIB)
	TEXTRA(J4,1) = TTS4(J4)-TTS(J3P1) ! amount of extrapolation,Top
	TEXTRA(J4,2) = TTB4(J4)-TTB(J3P1) ! " " , bottom
C	TAX(MAXN1,J4)= TEXTRA(J4,1) ! overload into tax
C	TIN(MAXN1,J4)= TEXTRA(J4,2) ! overload 
	HEATMM(J4) = HEAT1M	! daily average surface heat flow
	AFRO4(J4) = AFNOW	! current cap frost albedo
	IF (J5.EQ.JBARE) EFP=0.	! remove any remaining frost
	FROST4(J4) = EFP	! current cap frost amount
C
	IF (LP4) CALL TPRINT (4)	! print daily convergence summary
	IF (LD16) THEN
	  WRITE(76,761)SUBS,DLAT,ALB,SKRC,TAUD,PRES
 761	  FORMAT(/,'      Ls      Lt       A       I    TauD       P'
     &	     /F8.2,F8.2,F8.3,F8.1,F8.3,F8.2
     &	     //'   Hour      T_K  IR_Inc    Solar T_Planet')
 762	  FORMAT(F7.2,F9.3,F8.3,F9.3,F9.3)
	  QHS=24./FLOAT(N24)
	  DO I=1,N24
	    J=(I*N2)/N24
	    QH=I*QHS
	    QS=(1.-ALB)*ASOL(J)	! absorbed insolation
	    WRITE(76,762)QH,TSFH(I),ADGR(J),QS,TPFH(I)
	  ENDDO
	ENDIF
	IF (J4.LT.N4) GO TO 100
C
C  ---------------------------------------------------------------------
C
 9	IF (IDB2.GE.3) WRITE(IOSP,*)'TLATSx',N1,N1PIB,N2,N24,J3
	RETURN
	END
