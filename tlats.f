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
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
	REAL*4 BUF(MAXN4)	! fractional surface area in each latitude zone
	DATA BUF(1) /0./
	DATA AMW  /43.5/	! atomic weight of general atmosphere (g/mole)
	DATA RGAS /8.3145/	! ideal gas constant  (MKS=J/mol/K)
	DATA HUGE / 3.3E38/	! largest  REAL*4 constant
	DATA TINY / 2.0E-38/	! smallest  REAL*4 constant
	DATA EXPMIN / 86.80/	! neg exponent that would cause underflow
	REAL*4 RI(2,2)		! diffuse irradiances from Delta-Eddington
	REAL*4 COSI_AM(2)	! cos_incidence angle: average and noon

	REAL*4 COLL,COL3,BOND	! returned by Delta-Eddington
	IRL=1			! set return code to normal

C============ factors that do not depend upon season ===================
	RANG=2.0*PI/N2		! time step expressed in radians
	F23=2./3.
C
C radiation parameters:
C  ASOL = direct + diffuse insolation as a function of local time
	TWILFAC = 90./(90.+TWILI)     ! twilight factor
	TWILIM = COS((90.+TWILI)/RAD) ! minimum cosine i for twilight

C for sloping terrain
	SLONOR = SLOPE*COS(SLOAZI/RAD) ! north component of dip, degrees
	RADEAST = (SLOPE/RAD)*SIN(SLOAZI/RAD) ! east " " , radians
	SKYFAC = 1.-SLOPE/180.  ! fraction of sky remaining
        JHOT=INT(FLOAT(N24)*13.5/24.) ! index of warmest time of day

C============ factors constant over latitude that depend upon season ==========

	SOL=SOLCON/(DAU*DAU)	! solar flux at this heliocentric range
	RSDEC=SDEC/RAD		! current solar declination
	SD=SIN(RSDEC)
	CD=COS(RSDEC)
C set blowup test to a factor larger than perpendicular black surface
	TBLOW = 2.0 *  ( SOL / (EMIS*SIGSB) )**0.25
	
C integrate total sublimation & get current total pressure at 0 elevation
	IF (J5.EQ.1) BUF(1)=0.	! flag for  TINT; 1st time computes
	IF (N4.GT.8) THEN	! do global integrations
	  CALL TINT (FROST4, BUF, SUMF) !  MKS units  BUF = normalized area 
	  PCAP = SUMF*GRAV	! cap_frost equivalent surface pressure
	ELSE
	  PCAP=0.
	ENDIF
	PCO2M = PTOTAL*(1.-FANON) ! partial pres. of  CO2 at 0 elev., initial
	IF (KPREF.EQ.0) THEN
	  PZREF = PTOTAL
	ELSEIF (KPREF.EQ.1) THEN
	  KODE=4
	  PZREF = PTOTAL*VLPRES(KODE, DJU5)
	ELSEIF (KPREF.EQ.2) THEN
	  PZREF = PTOTAL - PCAP
	ENDIF
	PCO2G = (PZREF-PTOTAL) + PCO2M !  partial pres. of  CO2 at 0 elev. now
C
	IF (LPGLOB) THEN	! print global properties
	  CALL TPRINT(8)
	  WRITE(IOSP,'(A,F10.4)')' GLOBAL AVERAGE FROST; kg/m^2 =',SUMF
	ENDIF


C2004jul07	FAC7=COND/X(2) ! X is in DAYCOM.  COND is in KRCCOM
	J4=0
C  ----------------new latitude loop loop loop--------------------------
C
100	J4=J4+1
	DLAT=ALAT(J4)		! current latitude
	RLAT=DLAT/RAD
	CL=COS(RLAT)
	SL=SIN(RLAT)
	SS=SL*SD
	CC=AMAX1(1.E-10,CL*CD)
C for slope
	RLAT=(DLAT+SLONOR)/RAD ! effective latitude in radians
	CL2=COS(RLAT)
	SL2=SIN(RLAT)
	SS2=SL2*SD
	CC2=AMAX1(1.E-10,CL2*CD)

	COSI_AM(2) = (SS+CC)	!  cos ( incidence angle at noon )
	COSAM=-SS/CC		!  find length of day and daylight intervals
	IF (ABS(COSAM).LT.1.) THEN
		DAM=RAD*ACOS(COSAM)
	    ELSEIF (COSAM.GE.1.) THEN
		DAM=0.
		COSI_AM(2) = 1.E-6 ! sun never rises
	    ELSE
		DAM=180.
	    ENDIF

	EFROST = FROST4(J4) ! starting frost amount
	TATMJ=TTA4(J4) ! predicted final atm temp from prior season
	IF (J5.EQ.1) THEN
	  TATMAVE=TATM
	ELSE
	  CALL SIGMA(TAF(1,J4),N24,TATMAVE,TATMSIG) ! diurnal avrage Tatm.
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
          AVE_A=ALB
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
	AS=AVE_A	! surface albedo; frost if frosty at end of prior day
	DIFAC=SKYFAC+AS*(1.-SKYFAC) ! sky plus regional surface reflection
	IH = 1			! saving "hour" count
	AH = FLOAT(N2)/FLOAT(N24) ! time steps between saving results
	JJH = AH+.5		! round to time step of first saving
	DO JJ =1,N2 ! time of day loop.................................
	  ANGLE=(FLOAT(JJ)-0.5)*RANG
	  COSI= SS -CC*COS (ANGLE) ! cos of incidence angle on horizontal
	  COS2= SS2 -CC2*COS (ANGLE+RADEAST) ! " " onto slope
	  COLL=0.		! set several values for night
	  ATMHEAT=0.
	  DIRECT=0.
	  DIFFUSE=0.
	  TOPUP=0.
	  IF (COSI.GT.ACOSLIM) THEN ! day
	    CALL DEDING2 (OMEGA,G0,AS,COSI,OPACITY, BOND,COLL,RI)
	    TOPUP  =PI*(RI(1,1)-F23*RI(2,1)) ! diffuse flux up at top of atm.
	    BOTDOWN=PI*(RI(1,2)+F23*RI(2,2)) ! diffuse flux down at surf.
	    ATMHEAT=COSI-TOPUP-(1.-AS)*(BOTDOWN+COSI*COLL) ! atmospheric heating
	  ENDIF
	  IF (COS2.GT.0.) DIRECT=COS2*COLL	! slope is in sunlight
	  IF (COSI.GT.TWILIM) THEN ! in day or twilight zone
	    COS3= COS(TWILFAC*ACOS(COSI)) ! twilight effective cosine
	    CALL DEDING2 (OMEGA,G0,AS,COS3,OPACITY, BOND,COL3,RI)
	    BOTDOWN=PI*(RI(1,2)+F23*RI(2,2)) ! diffuse flux down at surf.
	    DIFFUSE=DIFAC*BOTDOWN ! diffuse flux onto surface
	  ENDIF
	  QI=(DIRECT+DIFFUSE)*SOL ! solar onto slope surface
	  QA=ATMHEAT*SOL	! solar heating of atm.
	  ASOL(JJ)=QI		! total insolation onto slope surface
	  ADGR(JJ)=QA
	  AVEI=AVEI+QI
	  AVEH=AVEH+QA
	  IF (LD19) WRITE(79,777) ADGR(JJ),QI,DIRECT,DIFFUSE
	  IF (JJ.EQ.JJH) THEN	!  JJH is next saving hour
	    TOFALB(IH,J4)=TOPUP
	    IH = IH+1		! increment to next hour
	    JJH = IH*AH+.5	! next time-step to save
	  ENDIF
	ENDDO			! end time of day loop....................
C
C  find equilibrium temperature at current latitude
	TAUIR=(CABR+TAUD*TAURAT)*(PRES/PTOTAL)! thermal opacity, zenith
	BETA=1.-EXP(-TAUIR)	! vertical thermal absorption of atmosphere
	FACTOR= 1.50307 -0.121687*ALOG(TAUIR) ! from fit to hemisphere integrals
	FACTOR=AMIN1(2.0,AMAX1(1.0,FACTOR)) ! asymptotic limits
	TAUEFF=FACTOR*TAUIR
	BETH=1.-EXP(-TAUEFF)	! hemispheric thermal absorption of atmosphere
	AVEH=AVEH/FLOAT(N2)	! average atm. solar heating
	AVEI=(1.-AS)*AVEI/FLOAT(N2) ! average absorbed insolation
	TAEQ4=(AVEH/BETH+AVEI)/(SIGSB*(2.-AVE_E*BETH)) ! equilib T_a^4
	TSEQ4=BETH*TAEQ4+AVEI/(SIGSB*AVE_E) ! equilib T_s^4
	TEQUIL = TSEQ4**0.25	! equilib T_s
	IF (TEQUIL.LT.TFNOW) TEQUIL=TFNOW
	JJO=1
C if continuing, use prior season; else, start with linear profile
	IF (J5.GT.1) THEN	! start with final value from previous season
	  DO  I=1,N1PIB
 	    T(I)=T4(I,J4)
	  ENDDO
	  TTS(1)=TTS4(J4)
	  TTB(1)=TTB4(J4)
	ELSE			! start with linear profile
	  IF (N3.GT.3) JJO=.75*N2+1.5	! start first day just past 3/4 day
	  TSUR=TEQUIL		!  isothermal if bottom is insulating
	  TBOT=TEQUIL		!  "  "
	  IF(IB.GE.1) TBOT=TDEEP! case for constant bottom  T
	  IF(IB.GT.1) TSUR=TDEEP! case for isothermal initial condition
	  DO  I=1,N1
	    T(I)=TSUR + (TBOT-TSUR) * (X(I)-X(1))/(X(N1)-X(1))
	  ENDDO
	  T(N1PIB)=TBOT
	  TTS(1)=0.
	  TTB(1)=0.
	  TATMJ=TAEQ4**0.25
	ENDIF
C	write(iosp,*)'tauir,taueff,BETH=',tauir,taueff,BETH
C	write(iosp,*)'aveh,avei=',aveh,avei
C	write(iosp,*)'tatmj,tequil=',tatmj,tequil
	IF (LP3) CALL TPRINT (3) ! print header for hourly summary
C======

	CALL TDAY (2,IR)		! execute day loop

C======
	IF (IR.EQ.2) THEN		! if blow-up; stop the model
	  IRL=2
	  RETURN
	ENDIF
C
C  save results for current latitude
C
	J3P1=J3+1
	TST4(J4)=TEQUIL
	DTM4(J4)=DTMJ(J3P1)
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
	      T4(I,J4)= EPRED(TT(I,JE),FP,MAXN1,TFNOW,TBLOW)
	   ENDDO
	   TTS4(J4)   = EPRED(TTS(JE),FP,1,TFNOW,TBLOW) ! surface average
	   TTB4(J4)   = EPRED(TTB(JE),FP,1,TFNOW,TBLOW) ! bottom layer average
	   TTA4(J4)   = EPRED(TTA(JE),FP,1,TFNOW,TBLOW) ! end-of-day Atm 
	   EF         = EPRED(FRO(JE),FP,1,0.,9999.)
	 ELSE			! extrapolation not possible
	   DO  I=1,N1		!  start next season where this one ended
	      T4(I,J4)= T(I)
	   ENDDO
	   TTS4(J4) = TTS(J3P1)
	   TTB4(J4) = TTB(J3P1)
	   TTA4(J4) = TTA(J3P1)
	   EF       = FRO(J3P1)
	ENDIF
	IF (IB.GE.1) T4(N1PIB,J4)=T(N1PIB)
	TEXTRA(J4,1) = TTS4(J4)-TTS(J3P1) ! amount of extrapolation,Top
	TEXTRA(J4,2) = TTB4(J4)-TTB(J3P1) ! " " , bottom
C	TAX(MAXN1,J4)= TEXTRA(J4,1) ! overload into tax
C	TIN(MAXN1,J4)= TEXTRA(J4,2) ! overload 
	HEATMM(J4) = HEAT1M	! daily average surface heat flow
	AFRO4(J4) = AFNOW	! current cap frost albedo
	IF (J5.EQ.JBARE) EF=0.	! remove any remaining frost
	FROST4(J4) = EF		! current cap frost amount
C
	IF (LP4) CALL TPRINT (4)	! print daily convergence summary
	IF (LD16) THEN
	  WRITE(76,761)SUBS,DLAT,ALB,SKRC,TAUD,PRES
 761	  FORMAT(/,'      Ls      Lt       A       I    TauD       P'
	1      /F8.2,F8.2,F8.3,F8.1,F8.3,F8.2
	2      //'   Hour      T_K  IR_Inc    Solar T_Planet')
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
	RETURN
	END
