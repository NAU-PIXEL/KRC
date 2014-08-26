      SUBROUTINE TLATS8 (IQ,IRL)
C_Titl  TLATS8  latitude computations for the  KRC thermal model system DP
C_Vars
      INCLUDE 'krcc8m.f'  ! has  IMPLICIT NONE 
      INCLUDE 'latc8m.f'
      INCLUDE 'dayc8m.f'
      INCLUDE 'hatc8m.f'
      INCLUDE 'porbx8m.f'      ! need only for no-atm version
      INCLUDE 'units.inc'
C_Args
      INTEGER*4 IQ            !in. not used
      INTEGER*4 IRL            !out return codes: 1=normal  2=blow-up
C_Calls  AVEDAY+  AVEYEAR+  CO2PT+  CLIMTAU'   DEDING28  EPRED8  
C        ROTV+  SIGMA  TDAY8  TPRINT8  TUN8  VDOT+  VLPRES'  VROTV+
C xx8 = make and call R*8 routine
C xx' = use R*4 transfers
C xx+ =  compile and link R*8 groups with same routine names: 
C         ksubs8 rotmdp8 vadddp8
C_Hist  1969 thru 97  Hugh_H_Kieffer
C   97feb11  HK major revision
C   97may30  HK 2nd version of atmospheric radiation
C   97jul22  HK 3rd version of atmospheric radiation
C   97sep08  HK add planetary temperatures
C   97jul07  HK from calories to  SI units
C   99dec10  HK revise insolation
C 2002jul12  HK Revise atmosphere to use Delta-Eddington
C 2005nov18  HK Add optional solar xenith angle limit 
C 2005dec28  HK Fix bug using ZENLIM. Additional comments
C 2006jan25  HK Modify SKYFAC from linear with slope to (1+cos s)/2
C 2008sep27  HK fix error in G1: was MIN(1.,(90.-AINC)/ZENLIM))
C 2008oct01  HK Use slope azimuth as a flag for a pit of slope SLOPE
C 2008oct04  HK Fix transfer of wrong BETA to KRCCOM
C 2010jan12  HK Use IMPLICIT NONE, move TINT call from TLATS to TSEAS
C 2012feb29  HK  Remove unused variables
C 2012mar01  HK  Include logical switches for atmosphere
C 2012mar27  HK  Incorporate  CLIMTAU
C 2014feb25 HK Set most variables to *4. Align with 6 spaces and 2-space indent
C 2014mar10:29 HK Make  REAL*8  version
C 2014may31 HK  Add call to TUN8(I15=102 to output fluxes on each hour
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C
      REAL*8 DERI(2,2)            ! diffuse irradiances from Delta-Eddington
      REAL*8 COSIAM(2)            ! cos_incidence angle: average and noon
      REAL*8 COLL,COL3,BOND      ! returned by Delta-Eddington
C
      REAL*8 ACOSLIM,AH,AINC,ANGLE,AVEA,AVEE,ATMHEAT,AVEI,AVEH
     &,BOTDOWN,BOUNCE,CC,CD,CL,COSAM,COSI,COSZLIM,COS2,COS3
     &,DIFAC,DIFFUSE,DIP,DIRECT,DIRFLAT,EFP,FACTOR,F23,FP,G0,G1,OMEGA
     &,PCAP,PFACTOR,PCO2G,PCO2M,RANG,RLAT,RSDEC,SAZ
     &,SD,SL,SOLR,SS,TAEQ4,TATMAVE,TATMSIG
     &,TAUICE,TAUVIS,TBOT,TOPUP,TSEQ4,TSUR,TWILFAC,TWILIM
      REAL*8 QI,QA,QH,QHS,QS      ! temporary use
      REAL*8 FXX(3),HXX(3),MXX(3),PXX(3),RXX(3),TXX(3) ! cartesian vectors
      REAL*8 YUNIT(3) / 0,1.,0./ ! unit vector along  Y axis
      REAL*8 SOLDIF(MAXN2)      ! Solar diffuse and bounce, W/m^2
C
      INTEGER*4 I,IH,IR, J,JE,JJ,JJH,JHOT,J3P1,KODE
      LOGICAL LQ1,LQ2,LATM,LQ3
C
      REAL VLPRES,CLIMTAU          ! Function names. Default precision
      REAL*8 AVEDAY,AVEYEAR,CO2PT,EPRED8     ! Function names 
      REAL*4 DJU54,DLAT4,SUBS4  ! for *8 to *4
      REAL*4 Y4,Z4              ! for *4 to *8
C
      LATM=PTOTAL.GT.1.0        ! atmosphere present flag
      LQ1=IDB2.GE.5            ! once per season or latitude
      LQ2=IDB2.GE.9            ! each time of day
      LQ3=LD19 .AND. (J5.EQ.JDISK) ! first recorded season
      IF (IDB2.NE.0) WRITE(IOSP,*)'TLATSa',N3,N4,J5,LATM,LQ1,LQ2
D       print *,'TLATSa',N3,N4,J5,LATM,LQ1,LQ2 !<dbug
C
      IRL=1                     ! set return code to normal
      I=IQ            ! simply to avoid compiler complaint that IQ is not used
C
C============ factors that do not depend upon season ===================
      RANG=2.0D0*PIVAL/N2      ! time step expressed in radians
      F23=2.D0/3.D0
      JHOT=INT(FLOAT(N24)*13.5D0/24.D0) ! index of warmest time of day
C
C radiation parameters:
C  ASOL = direct + diffuse insolation as a function of local time
      TWILFAC = 90.D0/(90.D0+TWILI) ! twilight factor
      TWILIM = DCOS((90.D0+TWILI)/RADC) ! minimum cosine i for twilight
C
C for sloping terrain
C;      SLONOR = SLOPE*COS(SLOAZI/RADC) ! north component of dip, degrees
C;      RADEAST = (SLOPE/RADC)*SIN(SLOAZI/RADC) ! east " " , radians
      IF (SLOAZI .GT. -360.D0) THEN ! slope is regional
         SKYFAC = (1.D0+ DCOS(SLOPE/RADC))/2.D0 ! effective isotropic radiation.
         COSZLIM=0.            ! zenith angle default limit is 90 degrees
      ELSE                  ! slope is of conical pit wall
         QA=(90.D0-SLOPE)/RADC      ! zenith angle of slope, in radians
         QI=DSIN(QA)
         SKYFAC = QI**2      ! effective isotropic radiation.
         COSZLIM=DCOS(QA)      ! 
      ENDIF
C============ factors constant over latitude that depend upon season ==========
C precision conversion
      DJU54=DJU5
      SUBS4=SUBS
C
      SOLR=SOLCON/(DAU*DAU)      ! solar flux at this heliocentric range
      RSDEC=SDEC/RADC            ! current solar declination
      SD=DSIN(RSDEC)
      CD=DCOS(RSDEC)
      MXX(1)=CD            ! Sun at midnight
      MXX(2)=0.
      MXX(3)=SD
      DIP=SLOPE/RADC            ! dip in radians
      SAZ=SLOAZI/RADC            ! azimuth of dip, east from north
      CALL ROTV (YUNIT,3,-SAZ,  PXX) ! PXX is temp. axis in equator
      IF (LQ1) THEN
         WRITE(75,*) 'J5+',J5,SUBS,SDEC,DAU,SLOPE,SLOAZI
         WRITE(75,*) 'MXX+',MXX,SKYFAC
         WRITE(75,*) 'PXX+',PXX
      ENDIF
C       set blowup test to a factor larger than perpendicular black surface
      TBLOW = 2.0D0 *  ( SOLR / (EMIS*SIGSB) )**0.25D0
C      
C get current total pressure at 0 elevation
      IF (N4.GT.8) THEN      ! use global integrations
         PCAP = SUMF*GRAV      ! cap_frost equivalent surface pressure
      ELSE
         PCAP=0.
      ENDIF
      PCO2M = (1.-FANON)*PTOTAL ! initial partial pres. of  CO2 at 0 elev
      IF (KPREF.EQ.0) THEN      ! constant 
         PZREF = PTOTAL       ! current total pressure at 0 elevation
         PCO2G = PCO2M      !  partial pres. of  CO2 at 0 elev. now
      ELSEIF (KPREF.EQ.1) THEN ! follows Viking
         KODE=4            ! average of all years and both landers
         Z4=VLPRES(KODE, DJU54) ! current ormalize pressure
         PZREF = PTOTAL*DBLE(Z4) ! current total P at 0
         PCO2G = PCO2M+(PZREF-PTOTAL) ! all changes are pure CO2
      ELSEIF (KPREF.EQ.2) THEN ! based on polar cap balance
         PZREF = PTOTAL - PCAP
         PCO2G = PCO2M -PCAP      ! all changes are pure CO2
      ENDIF
C
      IF (LPGLOB) THEN      ! print global properties
         CALL TPRINT8 (8)      ! print page heading
         WRITE(IOSP,'(A,F10.4)')' GLOBAL AVERAGE FROST; kg/m^2 =',SUMF
      ENDIF
C
      IF (LQ1) PRINT *,'TLAT1 J5,TBLOW=',J5,TBLOW
      J4=0
C  ----------------new latitude loop loop loop--------------------------
C
 100  J4=J4+1
      DLAT=ALAT(J4)            ! current latitude, degrees
      RLAT=DLAT/RADC            ! latitude in radian
      CL=DCOS(RLAT)
      SL=DSIN(RLAT)
      SS=SL*SD          
      CC=DMAX1(1.D-10,CL*CD)   
C       for slope
C;      RLAT2=(DLAT+SLONOR)/RADC ! effective latitude in radians
C;      CL2=COS(RLAT2)
C;      SL2=SIN(RLAT2)
C;      SS2=SL2*SD
C;      CC2=AMAX1(1.E-10,CL2*CD)
C       
      COSIAM(1)= DMAX1 (1.D-6,AVEDAY(SDEC,DLAT)) ! get average cosine incidence
C;      COSIAM(2)= SS+CC        !  cos ( incidence angle at noon )
      COSAM=-SS/CC            !  find length of day and daylight intervals
      IF (DABS(COSAM).LT.1.) THEN
         DAM=RADC*DACOS(COSAM)      ! occurs in COMMON. Used only in tprint.f
      ELSEIF (COSAM.GE.1.) THEN
         DAM=0.
C;         COSIAM(2) = 1.E-6    ! sun never rises
      ELSE
         DAM=180.D0
      ENDIF
C Vector algebra version 2012may31  !VAv
      
! fxx=[-cos(rlat),0.,sin(rlat)]  ; level normal at midnight
      FXX(1)=-CL
      FXX(2)=0.
      FXX(3)=SL
!    rxx=ROTV(pxx,2,-!dtor*(90.-xlat)) ; axis around which to rotate dip
      QA=PIVAL/2.-RLAT ! colat in radians
      CALL ROTV (PXX,2,-QA,  RXX)  ! axis around which to rotate dip
!    txx= VROTV(fxx,rxx,!dtor*tilt[0]) 
      CALL VROTV(FXX,RXX,DIP,  TXX) ! tilted surface normal
      IF (LQ1) THEN
         WRITE(75,*)'FXX+',FXX,J4,DLAT
         WRITE(75,*)'RXX=',RXX  ! R should be 90 deg from F
         WRITE(75,*)'TXX=',TXX
      ENDIF
C       
      AVEE=EMIS
      AVEA=ALB ! surface albedo; will be frost if frosty at end of prior day
      TAUVIS=TAUD            ! solar dust opacity
      TAUICE=0.            ! IR ice-cloud opacity
C
      IF (LATM) THEN            !v-v-v-v-v  with atmosphere
         IF (J5.LE.1) THEN      ! No prior season  Repaired: 2011aug14
            TATMAVE=TATM      ! diurnal average
            EFROST = 0.      ! frost on the ground
         ELSE                  ! use results from prior season
            CALL SIGMA(TAF(1,J4),N24,TATMAVE,TATMSIG) ! diurnal average Tatm.
            EFROST = FROST4(J4) ! starting frost amount
         ENDIF
         SCALEH = TATMAVE*RGAS/(AMW*GRAV) ! scale height in km
         PFACTOR = DEXP(-ELEV(J4)/SCALEH) ! relative to global annual mean
         PRES = PZREF * PFACTOR ! current local total pressure 
         IF (KVTAU.EQ.2) THEN   ! use climate opacities
           DLAT4=DLAT
           Y4=CLIMTAU (SUBS4,DLAT4,Z4) ! IR opacity  real*4
           TAUICE=DBLE(Z4)
           TAUVIS=DBLE(Y4)/TAURAT ! solar dust opacity
         ENDIF
         OPACITY = TAUVIS*PRES/PTOTAL+TAUICE/TAURAT ! normal solar opacity
C       not used?      SLOLIM = ACOS(ACOSLIM)             ! or 89.94 deg
C       
         IF (LVFA) THEN      ! use variable frost albedo
            CALL ALBVAR8 (COSIAM(1)*SOLR, AFNOW) ! var. frost albedo
         ELSE
            AFNOW = AFROST
         ENDIF
         IF (LVFT) THEN      ! use variable frost temperature
            TFNOW = CO2PT(1,PFACTOR*PCO2G) ! get local frost temperature
         ELSE
            TFNOW = TFROST
         ENDIF
         TATMIN = CO2PT(1,PFACTOR*PCO2G/2.71828) ! frost point for 1-layer atm
C         print*,'J5,J4,PCO2G,TFNOW,TATMIN',J5,J4,PCO2G,TFNOW,TATMIN
         IF (EFROST.GT.0.) THEN ! use frost emissivity and albedo
            AVEE=FEMIS
            AVEA=AFNOW
         ENDIF
         OMEGA=DUSTA            ! single scattering albedo
         G0=ARC2            ! Henyey-Greenstein asymmetry parameter
      ELSE                  ! +-+-+-+-+  no atm.
         EFROST=0.
         OPACITY=0. 
      ENDIF                  !^-^-^-^-^
      ACOSLIM = AMAX1(OPACITY/EXPMIN,0.001D0) ! limit to avoid math checks
      IF (LQ1) print *,'TLATS: J4,SOLR...',J4,SOLR,ACOSLIM,COSIAM(1)
      IF (LQ3) WRITE(IOSP,701)'LQ3',NCASE,J5,J4,TATMAVE,PRES,OPACITY
 701  FORMAT(A4,I3,I5,I3,2F12.6,F12.8)
C     
C insolation at mid-step and its average. time origin is midnight.
C  angle from noon is (j2/n2 * 2.pi) - pi, so cos of this is -cos(j2/n2 *2pi)
C  cosi = cosine of solar incidence angle onto horizontal
C  cos2 = cosine of solar incidence angle onto surface slope
C  cos3 = cosine of twilight angle onto horizontal
      AVEI=0.                  ! to sum solar onto slope surface
      AVEH=0.                  ! to sum atm. heating
      DIFAC=SKYFAC+AVEA*(1.-SKYFAC) ! sky plus regional surface reflection
      IH = 1                  ! saving "hour" count
      AH = DFLOAT(N2)/DFLOAT(N24) ! time steps between saving results
      JJH = AH+.5D0            ! round to time step of first saving
      DO JJ =1,N2            ! ------------------time of day loop----------
         ANGLE=(DFLOAT(JJ)-0.5D0)*RANG ! rotation angle at middle of time step
!     hxx=ROTV(mxx,3,k*dt)       ; Sun progress thru day
!     cosi=VDOT(hxx,fxx)         ; incidence on level
!     if cosi gt 0. then begin   ; daytime
!         cos2=VDOT(hxx,txx)>0.  ; incidence on tilted
!         if doa then cos2=cos2+sas*cosi ; add regional reflectance
         CALL ROTV( MXX,3,ANGLE, HXX) !VAv  Sun progress thru day
         CALL VDOT(HXX,FXX,COSI)      ! cos of incidence angle on horizontal
         CALL VDOT(HXX,TXX,COS2)        ! cos of incidence angle onto slope
C;         COSI= SS  -CC*COS (ANGLE) ! cos of incidence angle on horizontal
C;         COS2= SS2 -CC2*COS (ANGLE+RADEAST) ! " " onto slope
C       Get atmosphere transmission and heating for horizontal surface
         IF (COSI.GT.ACOSLIM) THEN ! Day: Sun is above horizon
           IF (LATM) THEN      !v-v-v-v-v  with atmosphere
             CALL DEDING28 (OMEGA,G0,AVEA,COSI,OPACITY, BOND,COLL,DERI)
             TOPUP  =PIVAL*(DERI(1,1)-F23*DERI(2,1)) ! diffuse up at top atm.
             BOTDOWN=PIVAL*(DERI(1,2)+F23*DERI(2,2)) ! diffuse down at surf.
             ATMHEAT=COSI-TOPUP-(1.-AVEA)*(BOTDOWN+COSI*COLL) ! atm. heating
             DIRFLAT=COSI*COLL ! collimated onto regional flat plane
           ELSE            ! -+-+-+-+ day with no atmosphere
C As opacity goes to zero, COLL->1., topup-> cosi*ALB, botdown->0 athmheat->0
             TOPUP  =COSI*AVEA  !  Lambert surface
             BOTDOWN=0.         ! no atm scattering
             ATMHEAT=0.         ! no atm absorbtion
             COLL=1.D0          ! no atm attenuation of beam
             DIRFLAT=COSI       ! incident intensity on horizontal
            ENDIF
          ELSE                  ! night: set several values for dark
            ATMHEAT=0.
            DIRFLAT=0.
            TOPUP=0.
            COLL=0.      
         ENDIF
C Get diffuse insolation, including twilight and first-order surface reflection 
         IF (COSI.GT.TWILIM) THEN ! in day or twilight zone
           COS3= DCOS(TWILFAC*DACOS(COSI)) ! twilight effective cosine
           IF (LATM.AND.(COS3.GT.1.D-5)) THEN
             CALL DEDING28 (OMEGA,G0,AVEA,COS3,OPACITY, BOND,COL3,DERI)
             BOTDOWN=PIVAL*(DERI(1,2)+F23*DERI(2,2)) !diffuse down at surf
           ELSE
             BOTDOWN=0.
           ENDIF
           DIFFUSE=SKYFAC*BOTDOWN ! diffuse flux onto surface
           IF (SLOAZI .LE. -360.D0) THEN ! bounce in a pit
             AINC=DACOS(COSI)*RADC ! incidence angle in degrees
             G1=DMIN1 (1.D0,(90.D0-AINC)/SLOPE) ! (90-i)/slope
           ELSE
             G1=1.0D0
           ENDIF 
           BOUNCE=(1.D0-SKYFAC)*AVEA*(G1*DIRFLAT+DIFFUSE)
         ELSE
           DIFFUSE=0.
           BOUNCE=0.
         ENDIF
C       
C       Set direct surface insolation
         IF (COS2.GT.COSZLIM) THEN ! target is directly illuminated
           DIRECT=COS2*COLL     ! slope is in sunlight
         ELSE
           DIRECT=0.            ! Target is in shadow
         ENDIF
C       
         QI=(DIRECT+DIFFUSE+BOUNCE)*SOLR ! solar onto slope surface
         IF (LQ1.AND.(MOD(JJ,24).EQ.1)) THEN
           WRITE(75,*)'HXX+',HXX,JJ
           WRITE(75,*)'ANG:',ANGLE,COSI,COS2,DIRECT,QI
         ENDIF
         IF (LQ2) WRITE(IOSP,*),'TLatc',JJ,COSI,COS3,DIRECT,DIFFUSE 
         QA=ATMHEAT*SOLR      ! solar heating of atm.
         ASOL(JJ)=QI            ! total insolation onto slope surface
         SOLDIF(JJ)=(DIFFUSE+BOUNCE)*SOLR ! all but the direc. Need for TUN8
         ADGR(JJ)=QA
         AVEI=AVEI+QI
         AVEH=AVEH+QA
         IF (LQ3) WRITE(88,777) JJ,COSI,COLL,QA,QI,DIRECT,DIFFUSE,BOUNCE
 777      FORMAT(I5,2f11.7,2f12.6,3f11.7)
         IF (JJ.EQ.JJH) THEN      !  JJH is next saving hour
           TOFALB(IH,J4)=TOPUP
           IH = IH+1            ! increment to next hour
           JJH = IH*AH+.5D0       ! next time-step to save
         ENDIF
      ENDDO                  !-------------end time-of-day loop-----------
C       
C       find equilibrium temperature at current latitude !+no atm
C
      AVEI=AMAX1((1.-AVEA)*AVEI/DFLOAT(N2),0.) ! average absorbed insolation
      IF (LATM) THEN            !v-v-v-v-v  with atmosphere
        TAUIR=(CABR+TAUVIS*TAURAT)*(PRES/PTOTAL)+TAUICE ! thermal opacity, zenith
        QA=AMIN1(0.0168455D0,AMAX1(TAUIR,62.4353D0)) ! limits 1. < FACTOR < 2.
        FACTOR= 1.50307D0 -0.121687D0*DLOG(QA) ! from fit to hemisphere integrals
        TAUEFF=FACTOR*TAUIR     ! effective hemispheric opacity
        BETA=1.-DEXP(-TAUEFF)   ! hemispheric thermal absorption of atmosphere
        AVEH=AMAX1(AVEH/DFLOAT(N2),0.) ! average atm. solar heating
        QS=AVEH/BETA            ! atm heating term
        IF (TAUIR.LT .01D0) QS=TAUD*SOLR/PIVAL+TAUIR*AVEI ! small tau limit
        TAEQ4=(QS+AVEI)/(SIGSB*(2.D0-AVEE*BETA)) ! equilib T_a^4
        TSEQ4=BETA*TAEQ4+AVEI/(SIGSB*AVEE) ! equilib T_s^4
        TEQUIL = AMAX1( TSEQ4,1.D4)**0.25D0 ! equilib T_s, min of 10.
        IF (TEQUIL.LT.TFNOW) TEQUIL=TFNOW
      ELSE                  ! no atmosphere
        BETA=0.
C       start using annual average insolation
        QA=1.D0/(DSQRT(1.D0-XECC**2)) ! average orbit insolation factor
        QS=AVEYEAR(RADC*BLIP,DLAT) ! Ave. fraction.  Args. in degrees
        AVEI=QA*QS*SOLCON/SJA**2 ! average insolation in W/m^2
        TEQUIL = ((1.D0-AVEA)*AVEI/(SIGSB*AVEE))**0.25D0 ! equilib T_s
      ENDIF
      IF (LQ1) then
C        print *,'J5,J4,TEQUIL',J5,J4,TEQUIL
        PRINT *,'AVEA ...',AVEA,AVEE,AVEI,AVEH
        PRINT *,'CABR...',CABR,TAUD,TAUIR,FACTOR,TAUEFF
        PRINT *,'BETA...',BETA,QS,SIGSB 
        PRINT *,'TAEQ4,TSEQ4,TEQUIL',TAEQ4,TSEQ4,TEQUIL
      ENDIF
      JJO=1
C       if at start, use linear profile, else  continuing from prior season
      IF (J5.LE.1) THEN      ! start with linear profile
        IF (N3.GT.3) JJO=.75*N2+1.5 ! start first day just past 3/4 day
        TSUR=TEQUIL             !  isothermal if bottom is insulating
        TBOT=TEQUIL             !  "  "
        IF(IB.GE.1) TBOT=TDEEP  ! case for constant bottom  T
        IF(IB.GT.1) TSUR=TDEEP  ! case for isothermal initial condition
C       
        IF (LQ1) PRINT *,'TSUR,TBOT',TEQUIL,TSUR,TBOT 
C        PRINT *,'Teq,TSUR,TBOT',TEQUIL,TSUR,TBOT
        IF (LQ1) PRINT *,'XCEN',XCEN 
        DO I=1,N1
          TTJ(I)=TSUR+(TBOT-TSUR)*(XCEN(I)-XCEN(1))/(XCEN(N1)-XCEN(1))
        ENDDO
        TTJ(N1PIB)=TBOT
        TTS(1)=TEQUIL
        TTB(1)=TEQUIL
        IF (LATM) THEN
          TATMJ=TAEQ4**0.25     ! starting Atm T
C       Approximate radiation time constant         
          QA=ATMCP*(PRES/GRAV)*TATMJ ! heat in the atm
     &         / (BETA*SIGSB* TAEQ4) !  /IR radiation rate 
          QS=QA/(2.71828D0*86400.D0) ! 1/e about right for Mars, convert to days
          IF (LQ1) PRINT *,'Radiation time, sec',QA 
     &         ,'  Relaxation time, days',QS
        ENDIF
      ELSE                      ! start with final value from previous season
        DO  I=1,N1PIB
          TTJ(I)=TMN4(I,J4)
        ENDDO
        TTS(1)=TTS4(J4)
        TTB(1)=TTB4(J4)
        IF (LATM) TATMJ=TTA4(J4) ! predicted final atm temp from prior season
      ENDIF
D       write(iosp,*)'tauir,taueff,BETA=',tauir,taueff,BETA
D       write(iosp,*)'aveh,avei=',aveh,avei
D       write(iosp,*)'tatmj,tequil=',tatmj,tequil
C       write(iosp,*) asol
C       write(iosp,*) adgr
      IF (LP3) CALL TPRINT8 (3) ! print header for hourly summary
      IF (IDB2.EQ.4) PRINT *,'J4,5 +',J4,J5,TEQUIL,TATMJ
      IF (LQ1) PRINT *,'TTJ',TTJ
C====== 
C       
      CALL TDAY8 (2,IR)      ! execute day loop
C       
C====== 
      IF (IR.EQ.2) THEN      ! if blow-up; stop the model
        IRL=2
        GOTO 9
      ENDIF
C       save results for current latitude
      IF (I15.EQ.102 .AND. J5.GE.JDISK) CALL TUN8(I15,2,I,SOLDIF) ! I is dummy
C       
      J3P1=J3+1
C       i=n24/2                  ! noonish
D       write(44,344) j3,j4,j5,ncase,efrost,ave_a,taud,pres !dbw 44
D       & ,DTMJ(J3),DTMJ(J3P1),TMIN(2),TMAX(2) !dbw 44
D       344       format(i3,i3,i4,i3,f12.6,f12.9,f12.9,f12.6,2f12.9,2f12.6) 
C       
      TST4(J4)=TEQUIL
      DTM4(J4)=DTMJ(J3P1)      !  RMS change of layer temperatures in prior day
      NDJ4(J4)=J3
      DO I=1,N24
        TSF(I,J4)=TSFH(I)
        TPF(I,J4)=TPFH(I)
        ENDDO
      DO I=1,N1            ! save extrema for each layer
        TIN(I,J4)=TMIN(I)       !   and predict next season's
        TAX(I,J4)=TMAX(I)       !   temperatures.
        ENDDO
C  
      TTX4(J4)=FLOST*DELJUL/(J3*PERIOD)  ! linear extrap. of frost lost over this season     
      FP=DELJUL/PERIOD-J3      ! undone iterations to end of this season.
C The 4 items predicted are stored for the end of the day prior to their index;
C so they are defined up to J3P1.  DTMJ(1)==-1.
C If 2 or more new days were computed this season, do aymptotic prediction.
C If have 1 new day, do linear predication; signaled by negative extrapol.
      IF (J3.GE.2) THEN      ! have at least 3 points in array to predict
        JE=J3-1
      ELSEIF (FP.LT.0.1D0) THEN ! can use 2-points
        FP=FP-1.D0                ! force  EPRED into 2-point mode
        JE=1
      ELSE
        JE=-1                   ! turn off any interpolation
      ENDIF
      IF (JE.GE.1) THEN      ! can use extrapolation
        DO I=1,N1               ! predict next season's temperatures
          TMN4(I,J4)= EPRED8(TT1(I,JE),FP,MAXN1,TFNOW,TBLOW)
        ENDDO
        TTS4(J4)   = EPRED8(TTS(JE),FP,1,TFNOW,TBLOW) ! surface average
        TTB4(J4)   = EPRED8(TTB(JE),FP,1,TFNOW,TBLOW) ! bottom layer average
        IF (LATM) THEN          !v-v-v-v-v  with atmosphere
          TTA4(J4)   = EPRED8(TTA(JE),FP,1,TFNOW,TBLOW) ! end-of-day Atm 
          EFP        = EPRED8(FRO(JE),FP,1,0.D0,9999.D0)
        ENDIF                   ! -+-+-+-+ day with no atmosphere
      ELSE                  ! extrapolation not possible
        DO I=1,N1               !  start next season where this one ended
          TMN4(I,J4)= TTJ(I)
        ENDDO
        TTS4(J4) = TTS(J3P1)
        TTB4(J4) = TTB(J3P1)
        TTA4(J4) = TTA(J3P1)    ! wont hurt to do if no atm == WHINA
        EFP      = FRO(J3P1)    ! wont hurt to do if no atm
      ENDIF
      IF (IB.GE.1) TMN4(N1PIB,J4)=TTJ(N1PIB)
      TEXTRA(J4,1) = TTS4(J4)-TTS(J3P1) ! amount of extrapolation,Top
      TEXTRA(J4,2) = TTB4(J4)-TTB(J3P1) ! " " , bottom
C       TAX(MAXN1,J4)= TEXTRA(J4,1) ! overload into tax
C       TIN(MAXN1,J4)= TEXTRA(J4,2) ! overload 
      HEATMM(J4) = HEAT1M      ! daily average surface heat flow
      AFRO4(J4) = AFNOW      ! current cap frost albedo WHINA
      IF (J5.EQ.JBARE) EFP=0.      ! remove any remaining frost WHINA
      FROST4(J4) = EFP      ! current cap frost amount WHINA
C       
      IF (LP4) CALL TPRINT8 (4) ! print daily convergence summary
      IF (LD16) THEN
        WRITE(76,761)SUBS,DLAT,ALB,SKRC,TAUD,PRES
 761    FORMAT(/,'      Ls      Lt       A       I    TauD       P'
     & /F8.2,F8.2,F8.3,F8.1,F8.3,F8.2 
     & //'   Hour      T_K  IR_Inc    Solar T_Planet')
 762    FORMAT(F7.2,F9.3,F8.3,F9.3,F9.3)
        QHS=24.D0/DBLE(N24)
        DO I=1,N24
          J=(I*N2)/N24
          QH=I*QHS
          QS=(1.D0-ALB)*ASOL(J)   ! absorbed insolation
          WRITE(76,762)QH,TSFH(I),ADGR(J),QS,TPFH(I)
        ENDDO
      ENDIF
      IF (J4.LT.N4) GO TO 100
C       
C       ---------------------------------------------------------------------
C       
 9    IF (IDB2.GE.3) WRITE(IOSP,*)'TLATSx',N1,N1PIB,N2,N24,J3
      RETURN
      END
