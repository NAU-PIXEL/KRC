	SUBROUTINE TDAY (IQ,IR)
C_Titl  TDAY  KRC day and layer computations
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'units.inc'
C_Args
	INTEGER IQ !in. 1=initialization   2=day computations
	INTEGER IR !out. 1=normal return  2=numerical blowup
C_Hist	97feb11  Hugh_Kieffer  USGS_Flagstaff major revision
C 98sep04  HHK move setting  N1PIB to  TCARD, minor code cleanup
C 00jan22  HHK Adjust layer thickness if required for stability and
C       redefine convergance factor to be square of earlier code.
C 02jul12 HK Revise atmosphere.
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

	REAL*4 FA1(MAXN1), FA2(MAXN1), FA3(MAXN1), DTJ(MAXN1)
     &, KJ(MAXN2)
	REAL*4 QK(MAXN1P),QR(MAXN1P),TAVE(MAXN1),diffi(maxn1)	! in daycom
	EQUIVALENCE (ASOL,QK),(TOUT,QR,TAVE) ! allow temporary shared space

	IR=1
C	write(iosp,*) 'tday IQ=',iq
	GOTO (100,200), IQ

C  initialization  (IQ = 1)
C
100	IF (IB.GE.1) NRSET=999	! never reset the lower boundary
	IF (NRSET.LT.1) NRSET=2
C insure day loop goes no further than .9 "day" past next season
	IF (N5.GT.1) N3=MIN(MAX(N3,IFIX(DELJUL/PERIOD +.9)),MAXN3-1)
	IF (IC.LT.3 .OR. IC.GT.N1-2) IC=999	! no changes near contacts
	N1P1=N1+1
	N1M1=N1-1
	N1PBM1=N1PIB-1
	NLW=1+(N1-3)/10
	PERSEC = PERIOD * 86400. ! get solar period in seconds
	DTIM=PERSEC/N2		! size of one time step
	COND=SKRC*SKRC/(DENS*SPHT) ! connductivity
	DIFFU=COND/(DENS*SPHT)	! diffusivity
	SCALE=SQRT(DIFFU*PERSEC/PI)
	IF (DEPTH.GE.1.0) THEN	! calculate  FLAY for bottom(n1) =  DEPTH
	  A=1.
	  B=0.
	  DO J=2,N1
	    A=A*RLAY
	    B=B+A
	  ENDDO
	  FLAY=DEPTH/B
	ENDIF
C
C  calculate frequently used constants
C
	FAC4 = 1.+1./RLAY
C	FACDWN = FDOWN*DTIM / (DENS*(1.0+RLAY)/2.0)
	A=CONVF
	IF (A.LT.0.8) A=1.E6	! avoid unstable binary time division
	SAFE2=2.*A	! 2 * safety convergence factor
C temporary  QK=cond  QR=dens*specific heat
	DO  I=1,N1P1
	  IF(I.LT.IC) THEN
	    QK(I)=COND
	    QR(I)=DENS*SPHT
	  ELSE
	    QK(I)=COND2
	    QR(I)=DENS2*SPHT2
	  ENDIF
	  diffi(i)=qk(i)/qr(i)	! layer diffusivity
	  J=1
	  IF (LOCAL) J=I
	  TLAY(I)= FLAY * RLAY**(I-1) * SQRT (diffi(j)*PERSEC/PI ) ! thickness
CC	  write(*,*)'I,diffi,tlay=',i,diffi(i),tlay(i)
	ENDDO
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
	km=km1
	K=0			! number of time doublings
	KKK=N2			! current number of times steps per day 
	SCONVG(1)=0.
	X(1)=-TLAY(1)/2.	! x is depth to layer center, [cm]
	DTIMI=DTIM		! current time step
C first pass, compute safety factor before time doublings
CC	write(*,*)'IC,dtimi=',ic,dtimi
	do I=2,n1
	  sconvg(i)=tlay(i)**2/(2.*dtim* diffi(i))
CC      write(*,*)'I,diffi,tlay,sconvg(i)=1',i,diffi(i),tlay(i),sconvg(i)
	enddo
	
	if (ic .le. n1-2) then	! check safety at first modified layer
	  if (sconvg(ic) .lt. 1.) then ! must increase layer thickness
	    do i=ic,n1 ! set layer  IC just stable
	      tlay(i)= RLAY**(I-IC) * sqrt(safe2*DTIM *diffi(i) )
	    enddo
	    km=0		! allow no time doubling above this layer
	  elseif (sconvg(ic) .gt. safe2) then ! allow upper layer changes
	    km=log(sconvg(ic))/log(safe2) ! max # of doublings before  IC
	  endif
	endif
CC	  write(*,*)'safe2,KM1,KM=',safe2,KM1,KM
	DO 140 I=2,N1
	  FA1(I)= QK(I) * DTIMI * 2. / (QR(I) * TLAY(I)**2
     &	       * (1. + ( TLAY(I+1)*QK(I) / (TLAY(I)*QK(I+1)) ) ) )
	  FA3(I)= (TLAY(I)/QK(I) + TLAY(I+1)/QK(I+1))
	1      / (TLAY(I)/QK(I) + TLAY(I-1)/QK(I-1))
	  X(I)= X(I-1)+ (TLAY(I)+TLAY(I-1))/2.
	  sconvg(i)=tlay(i)**2/(2.*dtimi* diffi(i))
CC      write(*,*)'I,diffi,tlay,sconvg(i)=2',i,diffi(i),tlay(i),sconvg(i)
	  IF (SCONVG(I).LT. 1.) THEN
	    IR=2		! error return if unstable
	    WRITE (IOERR,137)I,K,SCONVG(I)
 137	    FORMAT (' UNSTABLE; Layer, #_doubleings, factor =',2I3,F6.4)
	  ENDIF
	  if (i.eq.ic) km=km1	! remove constraint on upper layers
	  IF (K.GE.KM .OR. I.LE.2 .OR. MOD(KKK,2).NE.0) GOTO 140
	  IF (SCONVG(I).GT. safe2) THEN ! increase time step
CC      write(*,*)'I,diffi,tlay,sconvg(i)=3',i,diffi(i),tlay(i),sconvg(i)
	    DTIMI=2.*DTIMI
	    FA1(I)=2.* FA1(I)
	    SCONVG(I)=SCONVG(I)/2.
	    K=K+1		! have new binary interval
	    N1K(K)=I-1
	    KKK=KKK/2
	  ENDIF
 140	FA2(I)=-1.-FA3(I)
	FAC7=COND/X(2)
	KKK=K+1
	N1K(KKK)=N1
C set last layer for each time.  KJ(JJ)=K for  JJ time increment
	II=1
	DO K=1,KKK
	  I=N1K(K)
	  DO JJ=II,N2,II
	    KJ(JJ)=I
	  ENDDO
	  II=II+II
	ENDDO
	
	RETURN
C=======================================================================
C=============================================== day computations  (IQ = 2)
C
200	DTMJ(1)=-1.
	LDAY=N3.LE.1		! normally false
	LRESET=.FALSE.
	FAC3  = 1.-ALB
	FAC5  = SKYFAC*EMIS*SIGMA
	FAC45 = 4.*FAC5
	FAC6  = SKYFAC*EMIS
	FAC6F = SKYFAC*FEMIS
        FAC9=SIGMA*(1.-EXP(-TAUEFF)) ! BETH: factor for downwell hemispheric flux
        OMBETA=1.-BETA
	DTAFAC=DTIM/(ATMCP*(PRES/GRAV)) !  dT=heat*dtafac

	TSUR=T(1)
	DO J=1,N1		! t at start of first day
	  TT(J,1)=T(J)
	ENDDO
C  TATMJ and  EFROST enter via  KRCCOM	
	FROEX = MIN (FROEXT,0.1)	! scale-mass for insolation attenuation
	FRO(1) = EFROST
	IF (EFROST.GT.0.) THEN	! frost is present
	  LFROST = .TRUE.
	  TSUR = TFNOW
	  FEMIT = FAC6F*SIGMA*TFNOW**4
	  FAC8=OMBETA*FEMIS
	ELSE			! bare ground
	  LFROST = .FALSE.
	  FAC8=OMBETA*EMIS
	ENDIF
C
C  *v*v*v*v*v*v*v*v*v*v*v*v*v* new day loop v*v*v*v*v*v*v*v*v*v*v*v
C
	DO 320 JJJ=1,N3
	J3P1=JJJ+1
	TBOTM=0.
	TSURM=0.
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
	T(1)=T(2)-FAC4*(T(2)-TSUR)
	T(N1P1)=T(N1PIB)
	KN=KJ(JJ)
C
C  -v-v-v-v-v-v-v-v-v-v-v-v-v-v-v- layer loops v-v-v-v-v-v-v-v-v-v-v-v-v-
C
	DO  J=2,KN
	  DTJ(J)=FA1(J) * (T(J+1)+FA2(J)*T(J)+FA3(J)*T(J-1))
	ENDDO
	DO  J=2,KN
	  T(J)=T(J) + DTJ(J)
	ENDDO
C
C -^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^
	ATMRAD= FAC9*TATMJ**4	! hemispheric downwelling  IR flux
        IF (LFROST) THEN	! surface temperature is buffered
	  A = AFNOW + (ALB-AFNOW)*EXP(-EFROST/FROEX) ! albedo for frost layer
	  POWER = (1.-A)*ASOL(JJ) + FAC6F*ATMRAD
	1      + FAC7*(T(2)-TSUR) - FEMIT ! unbalanced flux into surface
	  DFROST = -POWER/CFROST ! rate of frost formation or sublimation
	  EFROST=EFROST + DFROST*DTIM ! amount on ground; kg*m**-2
	  IF (EFROST.LE.0.) THEN ! reset to bare ground
	    LFROST = .FALSE.
	    EFROST = 0.
	    FAC8=OMBETA*EMIS
	  ENDIF
	ELSE	! boundary conditions includes  T**4 radiation balance

	  ABRAD = FAC3*ASOL(JJ) + FAC6*ATMRAD	! surface absorbed radiation
C iterate as needed for  Newton convergance
 230	  TS3=TSUR**3		! bare ground
	  POWER = ABRAD + FAC7*(T(2)-TSUR) - FAC5*TSUR*TS3 ! unbalanced flux
	  DELT = POWER / (FAC7+FAC45*TS3)
	  TSUR=TSUR+DELT
	  ADEL=ABS(DELT)
	  IF (ADEL.LT.GGT) GOTO 240 ! satisfies convergence test
	  ADELN=ADEL/TSUR
	  IF (ADELN.GT.0.8) GOTO 340 ! probably blowing up numerically
	  IF (ADELN.GT.0.1) TSUR=TSUR-0.7*DELT !reduce increment to help stability
	  GOTO 230

 240	  IF (TSUR.GT.TBLOW) GOTO 340 ! numerical stability test
	  IF (TSUR.LT.TFNOW) THEN ! reset to frost on ground
	    LFROST = .TRUE.	! turn frost flag on
	    TSUR = TFNOW	! set to local frost temperature
	    FEMIT = FAC6F*SIGMA*TFNOW**4
	    FAC8=OMBETA*FEMIS
	  ENDIF
	ENDIF

D	IF (L10) WRITE(77,377)J5,JJJ,JJ,TSUR,DELT,EFROST,ABRAD,POWER
D 377	FORMAT (3I5,2F8.2,F7.3,2E14.5)

	TSURM=TSURM+TSUR
	TBOTM=TBOTM+T(N1)
	IF (LRESET) THEN	! accumulate average temperature at each layer
	  DO  J=2,N1
	    TAVE(J)=TAVE(J)+T(J)
	  ENDDO
	ENDIF
	TSUR4=TSUR**4
	TATM4=TATMJ**4
C 2002jul12  ADGR was downwelling  IR flux; becomes solar heating
	HEAT=ADGR(J)+FAC9*(EMIS*TSUR4-2*TATM4) ! net atm. heating flux
	TATMJ=TATMJ+HEAT*DTAFAC ! delta Atm Temp in 1 time step
	IF (TATMJ.LT.TATMIN) TATMJ=TATMIN ! atm. may not freeze
	IF (.NOT.LDAY) GOTO 270
	TOUT(JJ)=TSUR		! save surface temperatures at each time
	IF (JJ.EQ.JJH) THEN	!  JJH is next saving hour
	  T(1)=TSUR
	  TSFH(IH)=TSUR		! save hourly temperatures on last iteration
	  TPFH(IH)=(FAC8*TSUR4+BETA*TATM4)**0.25 ! planetary  T
	  DO  J=1,N1		! save extreme temperatures for each layer
C..		th(j,ih)=t(j)	! save depth-time solution [th(maxn1,MAXNH]
	    IF (T(J).LT.TMIN(J)) TMIN(J)=T(J)
	    IF (T(J).GT.TMAX(J)) TMAX(J)=T(J)
	  ENDDO
	  IH = IH+1
	  JJH = IH*AH+.5
	ENDIF
C
	IF (JJ.EQ.JJP) THEN	! print "hourly" temperatures
	IF (LP3) WRITE(IOSP,260)IP,EFROST,T(1),(T(I),I=2,N1M1,NLW)
     &     ,T(N1),tatmj
 260	  FORMAT (I7,F8.3,(12F8.1))
	  IP = IP+1
	  JJP = IP*AP+.5	! time step of next print
	ENDIF
270	CONTINUE  !^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^
C
C  store results of day, calculate rms change
C
	T(1)=TSUR
	ZD=0.
	DO  J=1,N1
	  ZD=ZD+(T(J)-TT(J,JJJ))**2
	  TT(J,J3P1)=T(J)
	ENDDO
	DTM=SQRT(ZD/N1)
	DTMJ(J3P1)=DTM
	TTS(J3P1)=TSURM/(N2+1-JJO)
	TTB(J3P1)=TBOTM/(N2+1-JJO)
	TTA(J3P1)=TATMJ         ! final atm. temperature
	FRO(J3P1)=EFROST        ! final frost amount
C
C  are we done?
C
	IF (LDAY.AND.(DTM.LE.DTMJ(JJJ) .OR. DTM.LE.GGT)) GOTO 330
	JJO=1
C
C  not yet - is next day the last?
C
	ZD = MAX(DTMJ(JJJ),1.E-6)
	IF (ABS(1.-DTM/ZD).LE.DDT .OR. DTM.LE.GGT
     &                            .OR. JJJ.EQ.N3-1) THEN  !  initiate last day
	   LDAY=.TRUE.
	ELSE			! not last day yet
	   LDAY=.FALSE.
	   IF (LRESET) THEN	! reset lower temps to have
	      TRSET=TTS(J3P1)-TTB(J3P1)
	      DO  J=2,N1	!   same average
		 TAVE(J)= TAVE(J)/N2 -TTS(J3P1) !
		 IF (DRSET.NE.0.) THEN
		    A=X(J)/X(N1)
		    T(J)=T(J)+TRSET*(A+DRSET*A*(1.-A))
		 ELSE
		    T(J)=T(J)-TAVE(J) !   as  TSUR
		 ENDIF
	      ENDDO
	      IF (LD17) WRITE(IOSP,'(I7,F8.3,F8.1,12F8.3)') !+++++++
	1	   JJJ,DTM,TTS(J3P1),(TAVE(I),I=2,N1M1,NLW),TAVE(N1) !+++++++
	   ENDIF
	   IF (J5.LE.1 .AND. JJJ.GE.NRSET) LRESET=.TRUE.
	ENDIF
 320	CONTINUE ! *^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*

	JJJ=N3		! if loop finished, index value not guarenteed
330	J3=JJJ	! reset the counter kept in common
	RETURN
c
340	IR=2  !   blow-up. force a stop; print current conditions
	T(1)=TSUR
	J2=JJ
	J3=JJJ
	CALL TPRINT (7)
	write(iosp,*) 'TSUR,DELT,TATMJ,TBLOW=',TSUR,DELT,TATMJ,TBLOW
	CALL TPRINT (4)
	RETURN
C
	END
