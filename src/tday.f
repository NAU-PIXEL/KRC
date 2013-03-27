	SUBROUTINE TDAY (IQ,IRET)
C_Titl  TDAY  KRC day and layer computations
C_Vars	
	INCLUDE 'krccom.inc'	! has IMPLICIT NONE
	INCLUDE 'daycom.inc'
	INCLUDE 'hatcom.inc'
	INCLUDE 'units.inc'
C_Args
	INTEGER IQ !in. 1=initialization   2=day computations
	INTEGER IRET !out. 1=normal return  2=numerical blowup
C_Hist	97feb11  Hugh_Kieffer  USGS_Flagstaff major revision
C 98sep04  HHK move setting  N1PIB to  TCARD, minor code cleanup
C 00jan22  HHK Adjust layer thickness if required for stability and
C       redefine convergance factor to be square of earlier code.
C 02jul12 HK Revise atmosphere.
C 04jul07 HK Change  MIN (FROEXT,0.1) to MAX (FROEXT,0.01)
C 2004sep30 HK Allow output of surface fluxes every hour
C 2008sep21 HK Allow for Snow formation in cold atm and fall to surface
C 2008nov07-2008feb14 HK Incorporate temperature-dependent conductivity
C 2010jan09 HK Fix so FAC7 conforms to temperature-dependent conductivity
C 2010jan12 HK Use IMPLICIT NONE
C 2010feb16 HK Incorporate temperature-dependent specific heat
C 2010sep04 HK Remove the 0.9 pad on  N3 limit of deljul/period
C 2011jul16 HK Remove one unused statement   I=MIN0(KN+1,IC-1)
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C  QQ is used in several places as temporary value


C new variables for k(T)
	REAL FBI(MAXN1),FCI(MAXN1),FKI(MAXN1) !kt layer factors
	REAL KTT(MAXN1P)	!kt thermal conductivity of each layer
	REAL VTT(MAXN1P)	!kt specific heat of each layer
	REAL FBK,FBKL,FA1J,FA3J !kt temporary factors
	LOGICAL LKOW ! true if there are lower layers of different properties
	INTEGER IK2,IK3,IK4 ! layer indices for kofT
	EQUIVALENCE (ASOL,QK),(TOUT,QR,TAVE) ! allow temporary shared space
	REAL TOFF/220./		!kt TEMPERATURE SCALING
	REAL TMUL/0.01/		!kt TEMPERATURE SCALING

	REAL FA1(MAXN1), FA2(MAXN1), FA3(MAXN1),DTJ(MAXN1) ! each max # layers
	REAL KJ(MAXN2)		! bottom layer for calculations at each time step
        REAL QK(MAXN1P),QR(MAXN1P),TAVE(MAXN1),DIFFI(MAXN1)
        EQUIVALENCE (ASOL,QK),(TOUT,QR,TAVE) ! allow temporary shared space

        INTEGER I,II,IH,IP,J,JJ,JJH,JJJ,JJP,JRSET,J3P1,K,KN,KM,KM1
     &, N1P1,N1PBM1

        REAL A,ABRAD,ADEL,ADELN,AH,AP,ATMRAD,B,CPOG,DELT,DFROST,DTAFAC
     &,DTIM,DTIMI,DTM,EMTIR,FAC3,FAC4,FAC45,FAC5,FAC6,FAC6F,FAC7,FAC8
     &,FAC82,FAC9,FEMIT,FROEX,HEAT,HEATFM,OMBETA,PERSEC,POWER,QQ,SAFE2
     &,SHEATF,SNOW,TATM4,TBOTM,TRSET,TSUR,TSURM,TS3,TSUR4,ZD,FCJ

	LOGICAL LDAY,LFROST,LRESET
	SAVE KTT

 991	IF (IDB2.GE.5) WRITE(IOSP,*) 'TDAY IQ,J4=',IQ,J4,jjo

	IRET=1
	GOTO (100,200), IQ

C  initialization  (IQ = 1)
C Set up grid based upon nominal conductivity, then use T-dependant values
C in the time loops. Assumption here is that conductivity could depend on
C several variables, but that having all but T constant for a given case is 
C adequate. Hence, compute K of T once per case, store as a table with no 
C finer resolution than 1 K.
C The convergence safty factor should be chosen to be adequate to cover 
C the conductivity variation

100	JRSET=NRSET
	IF (IB.GE.1) JRSET=999	! never reset the lower boundary
	IF (JRSET.LT.1) JRSET=2
C insure day loop does not go past next season
	IF (N5.GT.1) N3=MIN(MAX(N3,IFIX(DELJUL/PERIOD)),MAXN3-1)
	N1P1=N1+1
	N1M1=N1-1
	N1PBM1=N1PIB-1
	NLW=1+(N1-3)/10
	PERSEC = PERIOD * 86400. ! get solar period in seconds
	DTIM=PERSEC/N2		! size of one time step
	COND=SKRC*SKRC/(DENS*SPHT) ! conductivity
	DIFFU=COND/(DENS*SPHT)	! diffusivity
	SCALE=SQRT(DIFFU*PERSEC/PIVAL)
	IF (DEPTH.GE.1.0) THEN	! calculate  FLAY for bottom(n1) =  DEPTH
	  QQ=1.
	  B=0.
	  DO J=2,N1
	    QQ=QQ*RLAY
	    B=B+QQ
	  ENDDO
	  FLAY=DEPTH/B
	ENDIF
C
C  calculate frequently used constants
C
	FAC4 = 1.+1./RLAY
C temporary  QK=cond  QR=dens*specific heat
C  IC is first of lower material, including the virtual layer
	DO  J=1,N1P1
	  IF(J.LT.IC) THEN
	    QK(J)=COND
	    QR(J)=DENS*SPHT
	  ELSE
	    QK(J)=COND2
	    QR(J)=DENS2*SPHT2
	  ENDIF
	  DIFFI(J)=QK(J)/QR(J)	! layer diffusivity
	  K=1
	  IF (LOCAL) K=J
	  BLAY(J)= FLAY * RLAY**(J-1) * SQRT (DIFFI(K)*PERSEC/PIVAL) ! thickness
	  KTT(J)=QK(J)		! thermal conductivity, in case not T-dependent
D	  write(*,*)'J,diffi,blay=',J,diffi(J),blay(J)
	ENDDO

C Print table of T-dependant inertias.  Temporary use of  FBI,FCI,FKI,FA1,FA2
	IF (LKOFT) THEN		!kt   k of  T section
	   K=20
	   DO I=1,K
	      FKI(I)=120.+10.*I ! vector of temperatures
	   ENDDO
	   CALL EVMONO3(CCKU,K,FKI,TOFF,TMUL, FBI) ! upper conductivities
	   CALL EVMONO3(CCKL,K,FKI,TOFF,TMUL, FCI) ! lower "
	   CALL EVMONO3(CCPU,K,FKI,TOFF,TMUL, FA1) ! upper specific heat
	   CALL EVMONO3(CCPL,K,FKI,TOFF,TMUL, FA2) ! lower "
	   WRITE(IOSP,*) 'CCKU/L=',CCKU,CCKL
	   WRITE(IOSP,*) 'CCPU/L=',CCPU,CCPL
	   WRITE(IOSP,*)'   T     cond_SpHeat_UPPER_iner'
     &   ,'    cond_SpHeat__LOWER_iner'
C                         130.0  0.02590  375.46  124.74 

	   DO I=1,K
	      QQ=SQRT(FBI(I)*FA1(I) *DENS) ! upper inertia
	      A =SQRT(FCI(I)*FA2(I)*DENS2)! lower inertia
	      WRITE(IOSP,120) FKI(I),FBI(I),FA1(I),QQ,FCI(I),FA2(I),A
	   ENDDO
 120	    FORMAT(1X,F6.1,F9.5,2F8.2,F10.5,2F8.2)
	 ENDIF
C
C  Calculate layer dependent factors
C  K=index of binary depth  N1K=bottom layer for binary  K
C		00jan22
C  Allow doubling of time step at successive layers; but never more than a 
C factor of 2 between layers, and never a decrease in the time step with 
C increasing layers.
C  Thus, a single pass of increasing the time step downwards would suffice 
C except for the case of an increase of diffusivity at layer  IC.
C  So, if  IC invoked, determine the time step increase that might be possible
C there, and do not allow higher layers to exceed this value.
C  If layer  IC is unstable, need to increase its thickness (and possibly lower
C layers) to insure stability.
C
	KM1= MIN(MAXBOT,IFIX(ALOG(FLOAT(N2))/ALOG(2.)+.001) ) -1
	KM=KM1			! number of time doublings allowed
	K=0			! number of time doublings so far
	KKK=N2			! current number of times steps per day 
	SCONVG(1)=0.
	XCEN(1)=-BLAY(1)/2.	! x is depth to layer center, [cm]
	DTIMI=DTIM		! current time step
	QQ=CONVF		! input safety factor
	IF (QQ.LT.0.8) QQ=1.	! avoid unstable binary time division
	SAFE2=2.*QQ		! safety convergence factor
	IF (IC .LE. N1-2) THEN	! check safety at first modified layer
	   QQ=BLAY(IC)**2/(SAFE2*DTIM* DIFFI(IC)) ! extra safety factor
	  IF (QQ .LT. 1.) THEN ! must increase layer thickness
	    DO J=IC,N1 ! set layer  IC just stable for local diffusivity
	      BLAY(J)= RLAY**(J-IC) * SQRT(SAFE2*DTIM *DIFFI(J) )
	    ENDDO
	    KM=0		! allow no time doubling above this layer
	  ELSE ! allow upper layer changes
	    KM=LOG(QQ)/LOG(2.) ! max # of doublings before  IC
	  ENDIF
	ENDIF
	  WRITE(*,*)'SAFE2,actual+',SAFE2,QQ,KM1,KM,KKK,DTIMI

	DO J=2,N1		! layer loop
	  XCEN(J)= XCEN(J-1)+ (BLAY(J)+BLAY(J-1))/2. ! center depth
	  SCONVG(J)=BLAY(J)**2/(2.*DTIMI* DIFFI(J))
	  IF (J.EQ.IC) KM=KM1	! remove constraint on upper layers
	  IF (K.LT.KM .AND. J.GT.2 .AND. MOD(KKK,2).EQ.0 
     &    .AND. SCONVG(J).GT. SAFE2) THEN ! increase time step
	    DTIMI=2.*DTIMI
	    SCONVG(J)=SCONVG(J)/2.
	    K=K+1		! have new binary interval
	    N1K(K)=J-1		! bottom layer of prior interval
	    KKK=KKK/2		! number of time steps for lower layers
	  ENDIF
	  IF (SCONVG(J).LT. 1.) THEN
	    IRET=2		! error return if unstable
	    WRITE (IOERR,137)J,K,SCONVG(J)
 137	    FORMAT (' UNSTABLE; Layer, #_doubleings, factor =',2I3,F6.4)
	  ENDIF
C for constant conductivity
	  FCJ= 2.* DTIMI /(QR(J) * BLAY(J)**2) !kt
	  FA1(J)=FCJ*QK(J)/(1.+(BLAY(J+1)*QK(J)/(BLAY(J)*QK(J+1))))
	  FA3(J)= (BLAY(J)/QK(J) + BLAY(J+1)/QK(J+1))
     &       / (BLAY(J)/QK(J) + BLAY(J-1)/QK(J-1))
	  FA2(J)=-1.-FA3(J)
C T-dependant conductivity
	  FBI(J)=  BLAY(J+1)/BLAY(J)   !kt 
	  FAC7=DENS		! temporary use of FAC7
	  IF (J.GE.IC ) FAC7=DENS2 ! for upper/lower density
	  FCI(J)= 2.* DTIMI /(FAC7 * BLAY(J)**2) !kt
 125	  FORMAT(1X,I4,E12.5,f10.1,F8.5,f10.5, F9.6,2F8.5,F9.5,2E12.5)
D	  WRITE(41,125)J,DIFFI(J),DTIMI, BLAY(J),SCONVG(J)  !dbw
D     &     ,FA1(J),FA3(J),FBI(J),FCI(J),QK(J),QR(J) !dbw
	ENDDO			! end of layer loop

	FAC7=COND/XCEN(2)
	KKK=K+1			! one larger than number of time doublings
	N1K(KKK)=N1
C set last layer for each time.  KJ(JJ)=K for  JJ time increment
	II=1 ! spacing
	DO K=1,KKK		! each doubling
	   I=N1K(K)		! bottom layer for that doubling
	   DO JJ=II,N2,II	! each time-step along current spacing
	      KJ(JJ)=I		! set the deepest layer to do
	   ENDDO
	   II=II+II		! double the spacing
	ENDDO
	
	GOTO 9
C=======================================================================
C=============================================== day computations  (IQ = 2)
C
200	DTMJ(1)=-1.
	LDAY=N3.LE.1		! normally false
	LRESET=.FALSE.
	FAC3  = 1.-ALB
	FAC5  = SKYFAC*EMIS*SIGSB
	FAC45 = 4.*FAC5
	FAC6  = SKYFAC*EMIS
	FAC6F = SKYFAC*FEMIS
        FAC9=SIGSB*BETA		! factor for downwelling hemispheric flux
        CPOG=ATMCP*(PRES/GRAV)	! atmosphere:  D_Energy / d_T
	DTAFAC=DTIM/CPOG	! dT=heat*dtafac
	EMTIR = EXP(-TAUIR)	! Zenith Infrared transmission of atm
	FAC82=1.-EMTIR		!  " absorption "
D       write(iosp,*)'CPOG,DTAFAC,CFROST =',CPOG,DTAFAC,CFROST !!!
D	write(iosp,*) 'FAC9,DTAFAC=',FAC9,DTAFAC,EMTIR,FAC82
	TSUR=TTJ(1)
	DO J=1,N1		! save  T each layer at start of first day
	  TT1(J,1)=TTJ(J)
	ENDDO
	IK2=MIN0(N1+1,IC-1)	! number of upper layers 2010jan20 KN >> N1
	LKOW=IK2.LT.N1+1	! true if there are lower layers
	IK3=IK2+1		! first of lower layers
	IK4=N1+1-IK2		! number of lower layers
C  TATMJ and  EFROST enter via  KRCCOM	
	FROEX = MAX (FROEXT,0.01)	! scale-mass for insolation attenuation
	FRO(1) = EFROST
	IF (EFROST.GT.0.) THEN	! frost is present  kg/m^2
	  LFROST = .TRUE.
	  TSUR = TFNOW
	  FEMIT = FAC6F*SIGSB*TFNOW**4
	  FAC8=EMTIR*FEMIS
	ELSE			! bare ground
	  LFROST = .FALSE.
	  FAC8=EMTIR*EMIS
	ENDIF
C
C  *v*v*v*v*v*v*v*v*v*v*v*v*v* new day loop v*v*v*v*v*v*v*v*v*v*v*v
C
	DO 320 JJJ=1,N3
	J3P1=JJJ+1
	TBOTM=0.
	TSURM=0.
	HEATFM=0.
	IF (LDAY) THEN 
	   LRESET=.FALSE.	! must not use  TOUT for average on last day
	   DO  J=1,N1
	      TMIN(J)=TBLOW
	      TMAX(J)=0.
	   ENDDO
	   IH = 1		! saving "hour" count
	   AH = FLOAT(N2)/FLOAT(N24) ! time steps between saving results
	   JJH = AH+.5		! round to time step of first saving
	   IP = 1		! printout "hour count"
	   AP = FLOAT(N2)/FLOAT(NMHA) ! time steps between printing results
	   JJP = AP+.5		! time step of first "hourly" printout
	ENDIF
	IF (LRESET) THEN
	  DO  J=2,N1
	    TAVE(J)=0.
	  ENDDO
	ENDIF
C
C  +v+v+v+v+v+v+v+v+v+v+v+v+v+v+ new time loop +v+v+v+v+v+v+v+v+v+v+v+v+v+v+
C
	DO 270 JJ=JJO,N2
	   TTJ(1)=TTJ(2)-FAC4*(TTJ(2)-TSUR) ! set virtual layer
	   TTJ(N1P1)=TTJ(N1PIB)	! lower boundary condition
	   KN=KJ(JJ)		! depth for this time interval
C
C  -v-v-v-v-v-v-v-v-v-v-v-v-v-v-v- layer loops v-v-v-v-v-v-v-v-v-v-v-v-v-
C
	 IF (LKOFT) THEN	!kt section ----------------------
	   CALL EVMONO3(CCKU,IK2,TTJ,TOFF,TMUL, KTT) !kt get thermal conductivity
	   CALL EVMONO3(CCPU,IK2,TTJ,TOFF,TMUL, VTT) !kt get specific heat
	   IF (LKOW) THEN
	      CALL EVMONO3(CCKL,IK4,TTJ(IK3),TOFF,TMUL,KTT(IK3)) !" lower
	      CALL EVMONO3(CCPL,IK4,TTJ(IK3),TOFF,TMUL,VTT(IK3)) !" lower
	   ENDIF
	   FBK=RLAY		!kt F_B_i * F_k_i for virtual layer
	   DO  J=2,KN		!kt
	      FBKL=FBK		!kt
	      FBK= FBI(J)*KTT(J)/KTT(J+1) !kt F_B_i * F_k_i 
	      FA1J=FCI(J)*KTT(J)/(VTT(J)*(1.+FBK)) !kt eq F1
	      FA3J=(1.+FBK)/(1.+1./FBKL)  !kt eq F3
	      DTJ(J)= FA1J* (TTJ(J+1)-(1.+FA3J)*TTJ(J)+FA3J*TTJ(J-1)) !kt diffusion
D	      IF (JJJ.EQ.2) WRITE(26,126)JJ,J,FBK,FA1J,FA3J,DTJ(J)
	   ENDDO		!kt
D 126	   FORMAT(1X,2I3,F10.6,F12.6,F11.6,E13.5)
	   FAC7=KTT(2)/XCEN(2)
	ELSE        ! original constant conductivity ----------------------
           DO  J=2,KN
              DTJ(J)=FA1(J)* (TTJ(J+1)+FA2(J)*TTJ(J)+FA3(J)*TTJ(J-1)) ! diffusion
           ENDDO
	ENDIF          !----------------------
	   DO  J=2,KN
	      TTJ(J)=TTJ(J) + DTJ(J) ! apply the delta-T
	   ENDDO

D	   if (kn.eq.n1) write(43,343)jj,jjj,j5,kn,fac7,(dtj(j),j=2,kn) !dbw
D 343	   format(i4,i3,i4,i3,32f12.7)
C
C -^-^-^-^-^-^-^-^-^-^-^-^-^-^-^- end of layer loops ^-^-^-^-^-^-^-^-^-^-^
	ATMRAD= FAC9*TATMJ**4	! hemispheric downwelling  IR flux
        IF (LFROST) THEN	! surface temperature is buffered
	  A = AFNOW + (ALB-AFNOW)*EXP(-EFROST/FROEX) ! albedo for frost layer
	  SHEATF= FAC7*(TTJ(2)-TSUR) ! upward heatflow into the surface
	  POWER = (1.-A)*ASOL(JJ) + FAC6F*ATMRAD
     &        + SHEATF - FEMIT ! unbalanced flux into surface
	  DFROST = -POWER/CFROST ! rate of frost formation or sublimation
	  EFROST=EFROST + DFROST*DTIM ! amount on ground; kg*m**-2
	  IF (EFROST.LE.0.) THEN ! reset to bare ground
	    LFROST = .FALSE.
	    EFROST = 0.
	    FAC8=EMTIR*EMIS
	  ENDIF
	ELSE	! boundary conditions includes  T**4 radiation balance

	  ABRAD = FAC3*ASOL(JJ) + FAC6*ATMRAD	! surface absorbed radiation
C iterate as needed for  Newton convergance
C 992	  j=1 ! next 11  C lines 2010apr21 tracing down NAN from Tlats
 230	  TS3=TSUR**3		! bare ground
	  SHEATF= FAC7*(TTJ(2)-TSUR)
	  POWER = ABRAD + SHEATF - FAC5*TSUR*TS3 ! unbalanced flux
	  DELT = POWER / (FAC7+FAC45*TS3)
C	  if (delt.ne.delt) then
C	     write(ioerr,*)'DELT bad',TSUR,sheatf,power,DELT
C	     write(ioerr,*) fac3,asol(jj),fac6,ATMRAD,abrad
C	     write(ioerr,'(6i6)')j,jj,j2,j3,j4,j5
C	     delt=1.e-3
C	  endif
	  TSUR=TSUR+DELT
	  ADEL=ABS(DELT)
C 993	  IF (J.EQ.IDB4) THEN
C	     WRITE(IOSP,*)'TDAYB',J,TSUR,DELT,FAC7,FAC45
C	     IF (J.GT.20) STOP
C	  ENDIF
C	  J=J+1
	  IF (ADEL.LT.GGT) GOTO 240 ! satisfies convergence test
	  ADELN=ADEL/TSUR
	  IF (ADELN.GT.0.8) GOTO 340 ! probably blowing up numerically
	  IF (ADELN.GT.0.1) TSUR=TSUR-0.7*DELT ! reduce increment, help stability
	  GOTO 230

 240	  IF (TSUR.GT.TBLOW) GOTO 340 ! numerical stability test
	  IF (TSUR.LT.TFNOW) THEN ! reset to frost on ground
	    LFROST = .TRUE.	! turn frost flag on
	    TSUR = TFNOW	! set to local frost temperature
	    FEMIT = FAC6F*SIGSB*TFNOW**4
	    FAC8=EMTIR*FEMIS
	  ENDIF
	ENDIF

	TSURM=TSURM+TSUR
	TBOTM=TBOTM+TTJ(N1)
	HEATFM=HEATFM+SHEATF
	IF (LRESET) THEN	! accumulate average temperature at each layer
	  DO  J=2,N1
	    TAVE(J)=TAVE(J)+TTJ(J)
	  ENDDO
	ENDIF
	TSUR4=TSUR**4
	TATM4=TATMJ**4
C 2002jul12  ADGR was downwelling  IR flux; becomes solar heating
	HEAT=ADGR(JJ)+FAC9*(EMIS*TSUR4-2.*TATM4) ! net atm. heating flux
	TATMJ=TATMJ+HEAT*DTAFAC ! delta Atm Temp in 1 time step
	IF (.NOT.LDAY) GOTO 270

	TOUT(JJ)=TSUR		! save surface temperatures at each time
	IF (JJ.EQ.JJH) THEN	!  JJH is next saving hour
	  TTJ(1)=TSUR
	  TSFH(IH)=TSUR		! save hourly temperatures on last iteration
C	  TPFH(IH)=(FAC8*TSUR4+BETA*TATM4)**0.25 ! planetary  
	  TPFH(IH)=(FAC8*TSUR4+FAC82*TATM4)**0.25 ! planetary  
	  TAF(IH,J4)=TATMJ	! save Atm Temp.
	  DOWNVIS(IH,J4)=ASOL(JJ) ! save downward solar flux
	  DOWNIR(IH,J4)=ATMRAD	! save downward IR flux
D	  write(iosp,*)'L299', ASOL(JJ),ATMRAD,heat,tatmj
	  DO  J=1,N1		! save extreme temperatures for each layer
C..		th(j,ih)=t(j)	! save depth-time solution [th(maxn1,MAXNH]
	    IF (TTJ(J).LT.TMIN(J)) TMIN(J)=TTJ(J)
	    IF (TTJ(J).GT.TMAX(J)) TMAX(J)=TTJ(J)
	  ENDDO
	  IH = IH+1
	  JJH = IH*AH+.5
	ENDIF
C
	IF (JJ.EQ.JJP) THEN	! print "hourly" temperatures
	 IF (LP3) WRITE(IOSP,260)IP,EFROST,TTJ(1),(TTJ(I),I=2,N1M1,NLW)
     &      ,TTJ(N1),tatmj
 260	   FORMAT (I7,F8.3,(12F8.1))
	   IP = IP+1
	   JJP = IP*AP+.5	! time step of next print
	ENDIF
270	CONTINUE  !^+^+^+^+^+^+^+^+^+^+^+ end of time loop ^+^+^+^+^+^+^+^+^+^+^
C
C  store results of day, calculate rms change

	IF (TATMJ.LT.TATMIN) THEN ! Tatm is below saturation T
	   SNOW= (TATMIN-TATMJ)*CPOG/CFROST ! snow formation  Kg/m^2 in this time step
	   IF (LFROST) EFROST=EFROST + SNOW ! let it fall to surface
D	write(*,*)'jjj,Tmin,Ta,SNOW,EFR=',jjj,TATMIN,TATMJ,SNOW,EFROST !!!
	   TATMJ=TATMIN		! keep no colder that saturation
	ENDIF
C
	TTJ(1)=TSUR
	ZD=0.
	DO  J=1,N1		! each layer
	  ZD=ZD+(TTJ(J)-TT1(J,JJJ))**2  ! add square of diff from prior midnight
	  TT1(J,J3P1)=TTJ(J)	! save new midnight temperature
	ENDDO
	DTM=SQRT(ZD/N1) ! RMS change of layer temperatures in prior day
	DTMJ(J3P1)=DTM
	QQ=FLOAT(N2+1-JJO)	! temporary divisor
	TTS(J3P1)=TSURM/QQ	! average surface T
	TTB(J3P1)=TBOTM/QQ	! average bottom T
	HEAT1M=HEATFM/QQ	! average upward heatflow into the surface
	TTA(J3P1)=TATMJ         ! final atm. temperature
	FRO(J3P1)=EFROST        ! final frost amount
C
C  are we done?
C
	IF (LDAY.AND.(DTM.LE.DTMJ(JJJ) .OR. DTM.LE.DTMAX ! some convergence
     &  .OR. JJJ.GE.N3-1 )) GOTO 330 ! or end of season
	JJO=1
C
C  not yet - is next day the last?
C
	ZD = MAX(DTMJ(JJJ),1.E-6) ! yesterdays DTM:  temporary reuse of ZD
	IF (((ABS(1.-DTM/ZD).LE.DDT .OR. DTM.LE.DTMAX) 
     &          .AND. JJJ.GE.2) .OR. JJJ.EQ.N3-1) THEN  !  initiate last day
	   LDAY=.TRUE.
	ELSE			! not last day yet
	   LDAY=.FALSE.
	   IF (LRESET) THEN	! reset lower temps to have
	      TRSET=TTS(J3P1)-TTB(J3P1)
	      DO  J=2,N1	!   same average
		 TAVE(J)= TAVE(J)/N2 -TTS(J3P1) !
		 IF (DRSET.NE.0.) THEN
		    QQ=XCEN(J)/XCEN(N1)
		    TTJ(J)=TTJ(J)+TRSET*(QQ+DRSET*QQ*(1.-QQ))
		 ELSE
		    TTJ(J)=TTJ(J)-TAVE(J) !   as  TSUR
		 ENDIF
	      ENDDO
	      IF (LD17) WRITE(IOSP,'(I7,F8.3,F8.1,12F8.3)') !+++++++
     &      JJJ,DTM,TTS(J3P1),(TAVE(I),I=2,N1M1,NLW),TAVE(N1) !+++++++
	   ENDIF
	   IF (J5.LE.1 .AND. JJJ.GE.JRSET) LRESET=.TRUE.
	ENDIF
 320	CONTINUE ! *^*^*^*^*^*^*^*^*^*^*^*^ end of day loop *^*^*^*^*^*^*^*^*^*

	JJJ=N3			! if loop finished, index value not guarenteed
 330	J3=JJJ			! reset the counter kept in common
	GOTO 9
c
340	IRET=2  !   blow-up. force a stop; print current conditions
	TTJ(1)=TSUR
	J2=JJ
	J3=JJJ
	CALL TPRINT (7)
	WRITE(IOSP,*) 'TSUR,DELT,TATMJ,TBLOW=',TSUR,DELT,TATMJ,TBLOW
	CALL TPRINT (2)
	CALL TPRINT (4)

 9	IF (IDB2.GE.6) WRITE(IOSP,*) 'TDAYx'
	RETURN
C
	END
