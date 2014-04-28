      SUBROUTINE TDAY (IQ,IRET)
C_Titl  TDAY  KRC day and layer computations
C_Vars  
      INCLUDE 'krccom.inc'      ! has IMPLICIT NONE
      INCLUDE 'daycom.inc'
      INCLUDE 'hatcom.inc'
      INCLUDE 'units.inc'
C_Args
      INTEGER*4 IQ              !in. 1=initialization   2=day computations
      INTEGER*4 IRET            !out. 1=normal return  2=numerical blowup
C_Hist  97feb11  Hugh_Kieffer  USGS_Flagstaff major revision
C 1998sep04  HK  move setting  N1PIB to  TCARD, minor code cleanup
C 2000jan22  HK  Adjust layer thickness if required for stability and
C       redefine convergance factor to be square of earlier code.
C 2002jul12  HK  Revise atmosphere.
C 2004jul07  HK  Change  MIN (FROEXT,0.1) to MAX (FROEXT,0.01)
C 2004sep30  HK  Allow output of surface fluxes every hour
C 2008sep21  HK  Allow for Snow formation in cold atm and fall to surface
C 2008nov07-2008feb14  HK  Incorporate temperature-dependent conductivity
C 2010jan09  HK  Fix so FAC7 conforms to temperature-dependent conductivity
C 2010jan12  HK  Use IMPLICIT NONE
C 2010feb16  HK  Incorporate temperature-dependent specific heat  !kt
C 2010sep04  HK  Remove the 0.9 pad on  N3 limit of deljul/period
C 2011jul16  HK  Remove one unused statement   I=MIN0(KN+1,IC-1)
C 2012feb26  HK  Remove unused variables
C 2012feb29  HK  Add test for NAN's  UNSUCCESSFUL
C 2012mar01  HK  Include logical switches for atmosphere. 
C 2012apr24  HK  Tiny cleanup  May10: add FLOST calculation
C 2013dec04  HK Make write of SAFE2 conditional on IDB2
C 2014jan23  HK Change executable IC to IC2 so it is easily found
C 2014feb25,26 HK Set most variables to *4.  Untabify and justify
C 2014apr27 HK Add, then comment, code to trigger debugger pause 
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C
C new variables for k(T)
      REAL*4 FBI(MAXN1),FCI(MAXN1),FKI(MAXN1) !kt layer factors
      REAL*4 KTT(MAXN1P)        !kt thermal conductivity of each layer
      REAL*4 VTT(MAXN1P)        !kt specific heat of each layer
      REAL*4 FBK,FBKL,FA1J,FA3J !kt temporary factors
      LOGICAL LKOW     ! true if there are lower layers of different properties
      INTEGER IK2,IK3,IK4       ! layer indices for kofT
      REAL*4 TOFF/220./         !kt TEMPERATURE SCALING
      REAL*4 TMUL/0.01/         !kt TEMPERATURE SCALING

      REAL*4 FA1(MAXN1), FA2(MAXN1), FA3(MAXN1),DTJ(MAXN1) ! each max # layers
      REAL*4 KJ(MAXN2)          ! bottom layer for calculations at each time step
      REAL*4 QK(MAXN1P),QR(MAXN1P),TAVE(MAXN1),DIFFI(MAXN1P)
C allow temporary shared space
      EQUIVALENCE (ASOL,QK)     ! QK used only during initialization
      EQUIVALENCE (TOUT,QR,TAVE) ! QR used only during initialization
               ! TOUT used only on last day 
               ! TAVE used only when LRSET True, which is never the last day
C
      INTEGER*4 I,II,IH,IP,J,JJ,JJH,JJJ,JJP,JRSET,J3P1,K,KN
     & ,KM,KM1, N1P1,N1PBM1
C
      REAL*4 ABRAD,ADEL,ADELN,AH,AP,ATMRAD,CPOG,DELT,DFROST,DTAFAC
     &,DTIM,DTIMI,DTM,EMTIR,FAC3,FAC4,FAC45,FAC5,FAC6,FAC6F,FAC7,FAC8
     &,FAC82,FAC9,FEMIT,FROEX,HEATA,HEATFM,PERSEC,POWER,SAFE2
     &,SHEATF,SNOW,TATM4,TBOTM,TRSET,TSUR,TSURM,TS3,TSUR4,ZD,FCJ
      REAL*4 QA,QB,QQ           ! temporary use
D     REAL NAN  ! for testing if NAN
      LOGICAL LDAY              ! this will [normally] be the last iteration day
      LOGICAL LFROST            ! frost is present on the ground. 
      LOGICAL LRESET            ! it is permissable to do jump perturbation
      LOGICAL LATM              ! there is an atmosphere
C D     LOGICAL ISNAN           ! intrinsic Function NOT IN MY SYSTEM
D       integer*4 mhist,jhist   !<dbug
D       parameter (mhist=20)    !<dbug
D       integer*4 ihist(mhist)  !<dbug
C
      SAVE KTT,TOFF,TMUL
C
      IF (IDB2.GE.5) WRITE(IOSP,*) 'TDAY IQ,J4=',IQ,J4,JJO
      IRET=1
      GOTO (100,200), IQ
C
C  initialization  (IQ = 1)
C Set up grid based upon nominal conductivity, then use T-dependant values
C in the time loops. Assumption here is that conductivity could depend on
C several variables, but that having all but T constant for a given case is 
C adequate.
C The convergence safty factor should be chosen to be adequate to cover 
C the conductivity variation

 100  JRSET=NRSET
      IF (IB.GE.1) JRSET=999    ! never reset the lower boundary
      IF (JRSET.LT.1) JRSET=2
C insure day loop does not go past next season
      IF (N5.GT.1) N3=MIN(MAX(N3,IFIX(DELJUL/PERIOD)),MAXN3-1)
      N1P1=N1+1
      N1M1=N1-1
      N1PBM1=N1PIB-1
      NLW=1+(N1-3)/10
      PERSEC = PERIOD * 86400.  ! get solar period in seconds
      DTIM=PERSEC/N2            ! size of one time step
      COND=SKRC**2/(DENS*SPHT)  ! conductivity
      DIFFU=COND/(DENS*SPHT)    ! diffusivity
      SCALE=SQRT(DIFFU*PERSEC/PIVAL)
      IF (DEPTH.GE.1.0) THEN    ! calculate  FLAY for bottom(n1) =  DEPTH
        QQ=1.
        QB=0.
        DO J=2,N1
          QQ=QQ*RLAY
          QB=QB+QQ
        ENDDO
        FLAY=DEPTH/QB
      ENDIF
C
C  calculate frequently used constants
C
      FAC4 = 1.+1./RLAY
C temporary  QK=cond  QR=dens*specific heat
C  IC is first of lower material, including the virtual layer
      DO J=1,N1P1
        IF(J.LT.IC2) THEN
          QK(J)=COND
          QR(J)=DENS*SPHT
        ELSE
          QK(J)=COND2
          QR(J)=DENS2*SPHT2
        ENDIF
        DIFFI(J)=QK(J)/QR(J)    ! layer diffusivity
        K=1
        IF (LOCAL) K=J
        BLAY(J)= FLAY * RLAY**(J-1) * SQRT (DIFFI(K)*PERSEC/PIVAL) ! thickness
        KTT(J)=QK(J)            ! thermal conductivity, in case not T-dependent
D     write(*,*)'J,diffi,blay=',J,diffi(J),blay(J)
      ENDDO
C
C Print table of T-dependant inertias.  Temporary use of  FBI,FCI,FKI,FA1,FA2
      IF (LKOFT) THEN           !kt   k of  T section
        K=20
        DO I=1,K
          FKI(I)=120.+10.*I     ! vector of temperatures
        ENDDO
        CALL EVMONO3(CCKU,K,FKI,TOFF,TMUL, FBI) ! upper conductivities
        CALL EVMONO3(CCKL,K,FKI,TOFF,TMUL, FCI) ! lower "
        CALL EVMONO3(CCPU,K,FKI,TOFF,TMUL, FA1) ! upper specific heat
        CALL EVMONO3(CCPL,K,FKI,TOFF,TMUL, FA2) ! lower "
        WRITE(IOSP,*) 'CCKU/L=',CCKU,CCKL
        WRITE(IOSP,*) 'CCPU/L=',CCPU,CCPL
        WRITE(IOSP,*)'   T     cond_SpHeat_UPPER_iner'
     &       ,'    cond_SpHeat_LOWER_iner'
C                         130.0  0.02590  375.46  124.74 
        DO I=1,K
          QQ=SQRT(FBI(I)*FA1(I) *DENS) ! upper inertia
          QA =SQRT(FCI(I)*FA2(I)*DENS2) ! lower inertia
          WRITE(IOSP,120) FKI(I),FBI(I),FA1(I),QQ,FCI(I),FA2(I),QA
        ENDDO
 120    FORMAT(1X,F6.1,F9.5,2F8.2,F10.5,2F8.2)
 121    FORMAT(1X,A6,2F10.5,2F10.2)
C       Check that values agree at TOFF
        WRITE(IOSP,* )'          ConUp     ConLo     SphUp     SphLo' ! labels
        WRITE(IOSP,121 )'Input',COND,COND2,SPHT,SPHT2 ! inputs 
        WRITE(IOSP,121 )'T-dep',CCKU(1),CCKL(1),CCPU(1),CCPL(1) !  T-dep at TOFF
      ENDIF
C
C  Calculate layer dependent factors
C  K=index of binary depth  N1K=bottom layer for binary  K
C             2000jan22
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
      KM=KM1                    ! number of time doublings allowed
      K=0                       ! number of time doublings so far
      KKK=N2                    ! current number of times steps per day 
      SCONVG(1)=0.
      XCEN(1)=-BLAY(1)/2.       ! x is depth to layer center, [cm]
      DTIMI=DTIM                ! current time step
      QQ=CONVF                  ! input safety factor
      IF (QQ.LT.0.8) QQ=1.      ! avoid unstable binary time division
      SAFE2=2.*QQ               ! safety convergence factor
      IF (IC2 .LE. N1-2) THEN   ! check safety at first modified layer
        QQ=BLAY(IC2)**2/(SAFE2*DTIM* DIFFI(IC2)) ! extra safety factor
        IF (QQ .LT. 1.) THEN    ! must increase layer thickness
          DO J=IC2,N1           ! set layer  IC just stable for local diffusivity
            BLAY(J)= RLAY**(J-IC2) * SQRT(SAFE2*DTIM *DIFFI(J) )
          ENDDO
          KM=0                  ! allow no time doubling above this layer
        ELSE                    ! allow upper layer changes
          KM=LOG(QQ)/LOG(2.)    ! max # of doublings before  IC
        ENDIF
      ENDIF
      IF (IDB2.GE.2) WRITE(*,*)'SAFE2,actual+',SAFE2,QQ,KM1,KM,DTIM
C
      DO J=2,N1                 ! layer loop
        XCEN(J)= XCEN(J-1)+ (BLAY(J)+BLAY(J-1))/2. ! center depth
        SCONVG(J)=BLAY(J)**2/(2.*DTIMI* DIFFI(J)) ! Safety factor
        IF (J.EQ.IC2) KM=KM1    ! remove constraint on upper layers
        IF (K.LT.KM .AND. J.GT.2 .AND. MOD(KKK,2).EQ.0 
     &       .AND. SCONVG(J).GT. SAFE2) THEN ! increase time step
          DTIMI=2.*DTIMI
          SCONVG(J)=SCONVG(J)/2.
          K=K+1                 ! have new binary interval
          N1K(K)=J-1            ! bottom layer of prior interval
          KKK=KKK/2             ! number of time steps for lower layers
        ENDIF
        IF (SCONVG(J).LT. 1.) THEN
          IRET=2                ! error return if unstable
          WRITE (IOERR,137)J,K,SCONVG(J)
 137      FORMAT (' UNSTABLE; Layer, #_doubleings, factor =',2I3,F8.4)
        ENDIF
C for constant conductivity
        FCJ= 2.* DTIMI /(QR(J) * BLAY(J)**2) !kt
        FA1(J)=FCJ*QK(J)/(1.+(BLAY(J+1)*QK(J)/(BLAY(J)*QK(J+1))))
        FA3(J)= (BLAY(J)/QK(J) + BLAY(J+1)/QK(J+1))
     &       / (BLAY(J)/QK(J) + BLAY(J-1)/QK(J-1))
        FA2(J)=-1.-FA3(J)
C T-dependent conductivity
        FBI(J)=  BLAY(J+1)/BLAY(J) !kt 
        FAC7=DENS               ! temporary use of FAC7
        IF (J.GE.IC2 ) FAC7=DENS2 ! for upper/lower density
        FCI(J)= 2.* DTIMI /(FAC7 * BLAY(J)**2) !kt
D 125     FORMAT(1X,I4,E12.5,f10.1,F8.5,f10.5, F9.6,2F8.5,F9.5,2E12.5)
D         WRITE(41,125)J,DIFFI(J),DTIMI, BLAY(J),SCONVG(J)  !dbw
D     &     ,FA1(J),FA3(J),FBI(J),FCI(J),QK(J),QR(J) !dbw
      ENDDO                     ! end of layer loop
C
      FAC7=COND/XCEN(2)
      KKK=K+1                   ! one larger than number of time doublings
      N1K(KKK)=N1
C set last layer for each time.  KJ(JJ)=K for  JJ time increment
      II=1                      ! spacing
      DO K=1,KKK                ! each doubling
        I=N1K(K)                ! bottom layer for that doubling
        DO JJ=II,N2,II          ! each time-step along current spacing
          KJ(JJ)=I              ! set the deepest layer to do
        ENDDO
        II=II+II                ! double the spacing
      ENDDO
C     
      GOTO 9
C=======================================================================
C=============================================== day computations  (IQ = 2)
C
 200  DTMJ(1)=-1.
      LATM=PTOTAL.GT.1.         ! atmosphere present flag
      LDAY=N3.LE.1              ! normally false
      LRESET=.FALSE.            ! flag for reset of lower layers
C D     fac3=0.                 ! just for next statement
C D     NAN=PIVAL/fac3          ! generate a NAN
      FAC3  = 1.-ALB
      FAC5  = SKYFAC*EMIS*SIGSB
      FAC45 = 4.*FAC5
      FAC6  = SKYFAC*EMIS
      FAC6F = SKYFAC*FEMIS
C Following 5 used only  with atmosphere.  EMTIR used only for fac8, and FAC8 
C used only with atmosphere. Ensure divisor CPOG is not zero
      FAC9=SIGSB*BETA           ! factor for downwelling hemispheric flux
      CPOG=ATMCP*(max(PRES, 1.)/GRAV) ! atmosphere:  D_Energy / d_T
      DTAFAC=DTIM/CPOG          ! dT=heat*dtafac
      EMTIR = EXP(-TAUIR)       ! Zenith Infrared transmission of atm
      FAC82=1.-EMTIR            !  " absorption "
D       write(iosp,*)'CPOG,DTAFAC,CFROST =',CPOG,DTAFAC,CFROST !!!
D       write(iosp,*) 'FAC9,DTAFAC=',FAC9,DTAFAC,EMTIR,FAC82
      TSUR=TTJ(1)
      DO J=1,N1                 ! save  T each layer at start of first day
        TT1(J,1)=TTJ(J)
      ENDDO
      IK2=MIN0(N1+1,IC2-1)      ! number of upper layers 2010jan20 KN >> N1
      LKOW=IK2.LT.N1+1          ! true if there are lower layers
      IK3=IK2+1                 ! first of lower layers
      IK4=N1+1-IK2              ! number of lower layers
C  TATMJ and  EFROST enter via  KRCCOM  
      FROEX = MAX (FROEXT,0.01) ! scale-mass for insolation attenuation
      FRO(1) = EFROST
      IF (.NOT. LATM) EFROST=0.
      IF (EFROST.GT.0.) THEN    ! frost is present  kg/m^2
        LFROST = .TRUE.
        TSUR = TFNOW
        FEMIT = FAC6F*SIGSB*TFNOW**4 ! upward frost radiation toward space
        FAC8=EMTIR*FEMIS        ! ground effective emissivity through atmosphere ?
      ELSE                      ! bare ground
        LFROST = .FALSE.
        FAC8=EMTIR*EMIS
      ENDIF
      FLOST=0.                  ! sum of lost frost
D       do i=1,mhist    !<dbug
D          ihist(i)=0   !<dbug
D       enddo   !<dbug
D       print *,'T200: J5,J4,TSUR,FAC8',j5,j4,tsur,fac8 !<dbug
D       print *,'N4,LATM,tatmj',N4,latm,tatmj   !<dbug
C
C  *v*v*v*v*v*v*v*v*v*v*v*v*v* new day loop v*v*v*v*v*v*v*v*v*v*v*v
C
      DO 320 JJJ=1,N3
        J3P1=JJJ+1
        TBOTM=0.
        TSURM=0.
        HEATFM=0.
        IF (LDAY) THEN 
          LRESET=.FALSE.        ! must not use  TOUT for average on last day
          DO  J=1,N1
            TMIN(J)=TBLOW
            TMAX(J)=0.
          ENDDO
          IH = 1                ! saving "hour" count
          AH = FLOAT(N2)/FLOAT(N24) ! time steps between saving results
          JJH = AH+.5           ! round to time step of first saving
          IP = 1                ! printout "hour count"
          AP = FLOAT(N2)/FLOAT(NMHA) ! time steps between printing results
          JJP = AP+.5           ! time step of first "hourly" printout
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
          TTJ(N1P1)=TTJ(N1PIB)  ! lower boundary condition
          KN=KJ(JJ)             ! depth for this time interval
C
C  -v-v-v-v-v-v-v-v-v-v-v-v-v-v-v- layer loops v-v-v-v-v-v-v-v-v-v-v-v-v-
C
          IF (LKOFT) THEN       !kt section ----------------------
            CALL EVMONO3(CCKU,IK2,TTJ,TOFF,TMUL, KTT) ! get thermal conductivity
            CALL EVMONO3(CCPU,IK2,TTJ,TOFF,TMUL, VTT) ! get specific heat
            IF (LKOW) THEN      ! There are lower layers
              CALL EVMONO3(CCKL,IK4,TTJ(IK3),TOFF,TMUL,KTT(IK3)) ! " lower
              CALL EVMONO3(CCPL,IK4,TTJ(IK3),TOFF,TMUL,VTT(IK3)) ! " lower
            ENDIF
            FBK=RLAY            ! F_B_i * F_k_i for virtual layer
            DO J=2,KN          ! kt
              FBKL=FBK          ! kt
              FBK= FBI(J)*KTT(J)/KTT(J+1) ! F_B_i * F_k_i 
              FA1J=FCI(J)*KTT(J)/(VTT(J)*(1.+FBK)) ! eq F1
              FA3J=(1.+FBK)/(1.+1./FBKL) ! eq F3
              DTJ(J)= FA1J* (TTJ(J+1)-(1.+FA3J)*TTJ(J)+FA3J*TTJ(J-1)) !  diffusion
D     IF (JJJ.EQ.2) WRITE(26,126)JJ,J,FBK,FA1J,FA3J,DTJ(J)
            ENDDO               ! kt
D 126      FORMAT(1X,2I3,F10.6,F12.6,F11.6,E13.5)
            FAC7=KTT(2)/XCEN(2)
          ELSE         ! original constant conductivity ----------------------
            DO  J=2,KN
              DTJ(J)=FA1(J)* (TTJ(J+1)+FA2(J)*TTJ(J)+FA3(J)*TTJ(J-1)) ! diffusion
            ENDDO
          ENDIF                 !----------------------
          DO  J=2,KN
            TTJ(J)=TTJ(J) + DTJ(J) ! apply the delta-T
          ENDDO
D          if (kn.eq.n1) write(43,343)jj,jjj,j5,kn,fac7,(dtj(j),j=2,kn) !dbw
D 343      format(i4,i3,i4,i3,32f12.7)
C
C -^-^-^-^-^-^-^-^-^-^-^-^-^-^-^- end of layer loops ^-^-^-^-^-^-^-^-^-^-^

C Three possible boundary conditions. 1) Atm with frost 2) Just Atm 3) No atm.
          IF (LFROST) THEN      !+-+-+-+ surface temperature is frost-buffered
            ATMRAD= FAC9*TATMJ**4 ! hemispheric downwelling  IR flux
            QA = AFNOW + (ALB-AFNOW)*EXP(-EFROST/FROEX) ! albedo for frost layer
            SHEATF= FAC7*(TTJ(2)-TSUR) ! upward heatflow into the surface
C   unbalanced flux into surface
            POWER = (1.-QA)*ASOL(JJ) + FAC6F*ATMRAD + SHEATF - FEMIT
            DFROST = -POWER/CFROST ! rate of frost formation or sublimation
            EFROST=EFROST + DFROST*DTIM ! amount on ground; kg*m**-2
            IF (EFROST.LE.0.) THEN ! reset to bare ground
              LFROST = .FALSE.
              EFROST = 0.
              FAC8=EMTIR*EMIS
            ENDIF
          ELSE IF (LATM) THEN   !+-+-+-+ boundary condition with atmosphere
            ATMRAD= FAC9*TATMJ**4 ! hemispheric downwelling  IR flux
            ABRAD = FAC3*ASOL(JJ) + FAC6*ATMRAD ! surface absorbed radiation
C iterate as needed for  Newton convergance
C 992     j=1 ! next 11  C lines 2010apr21 tracing down NAN from Tlats
D          ii=0                 !<dbug
 230        TS3=TSUR**3         ! bare ground
            SHEATF= FAC7*(TTJ(2)-TSUR) ! upward heat flow to surface
            POWER = ABRAD + SHEATF - FAC5*TSUR*TS3 ! unbalanced flux
            DELT = POWER / (FAC7+FAC45*TS3)
C         if (delt.ne.delt) then
C            write(ioerr,*)'DELT bad',TSUR,sheatf,power,DELT
C            write(ioerr,*) fac3,asol(jj),fac6,ATMRAD,abrad
C            write(ioerr,'(6i6)')j,jj,j2,j3,j4,j5
C            delt=1.e-3
C         endif
            TSUR=TSUR+DELT
            ADEL=ABS(DELT)
D          ii=ii+1              !<dbug
D          if (ii.ge.mhist) then !<dbug
D             print *,'TDAY: j5,j4,j3,jj',j5,j4,j3,jj !<dbug
D             print *,'...tsur',abrad,sheatf,power,delt,tsur !<dbug
D             goto 9            !<dbug
D          endif                !<dbug
C       if (ISNAN(TSUR)) goto 340 ! FAILS
C       if( (TSUR+1.).eq.tsur) goto 340 !  FAILS. True only if is  NAN 
            IF (ADEL.LT.GGT) GOTO 240 ! satisfies convergence test
C          if (TSUR.eq.NAN) goto 340 ! FAILS. also true if  DELT is  NAN
            ADELN=ADEL/TSUR
            IF (ADELN.GT.0.8) GOTO 340 ! probably blowing up numerically
            IF (ADELN.GT.0.1) TSUR=TSUR-0.7*DELT ! reduce increment, help stability
            GOTO 230
C          
 240        CONTINUE
D          ihist(ii)=ihist(ii)+1 !<dbug
            IF (TSUR.GT.TBLOW) GOTO 340 ! numerical stability test
            IF (TSUR.LT.TFNOW) THEN ! reset to frost on ground
              LFROST = .TRUE.   ! turn frost flag on
              TSUR = TFNOW      ! set to local frost temperature
              FEMIT = FAC6F*SIGSB*TFNOW**4
              FAC8=EMTIR*FEMIS
            ENDIF
          ELSE                  !+-+-+-+ no atmosphere
            ABRAD = FAC3*ASOL(JJ) ! surface absorbed radiation
C iterate as needed for  Newton convergance
C 992     j=1 ! next 11  C lines 2010apr21 tracing down NAN from Tlats
D          ii=0                 !<dbug
 232        TS3=TSUR**3         ! bare ground
            SHEATF= FAC7*(TTJ(2)-TSUR)
            POWER = ABRAD +SHEATF - FAC5*TSUR*TS3 ! unbalanced flux
            DELT = POWER / (FAC7+FAC45*TS3) ! Newton estimate
            TSUR=TSUR+DELT
            ADEL=ABS(DELT)
            IF (ADEL.LT.GGT) GOTO 242 ! satisfies convergence test
C     if (TSUR.eq.NAN) goto 340 ! FAILS. also true if  DELT is  NAN
            ADELN=ADEL/TSUR
            IF (ADELN.GT.0.8) GOTO 340 ! probably blowing up numerically
            IF (ADELN.GT.0.1) TSUR=TSUR-0.7*DELT !reduce increment,help stability
            GOTO 232
C
 242        IF (TSUR.GT.TBLOW) GOTO 340 ! numerical stability test
          ENDIF                 !+-+-+-+ end no atmosphere
C
          TSURM=TSURM+TSUR      ! sum over the day
          TBOTM=TBOTM+TTJ(N1)   ! "
          HEATFM=HEATFM+SHEATF  ! "
          IF (LRESET) THEN      ! accumulate average temperature at each layer
            DO  J=2,N1
              TAVE(J)=TAVE(J)+TTJ(J)
            ENDDO
          ENDIF
          TSUR4=TSUR**4
C
          IF (LATM) THEN        !v-v-v-v-v  Adjust atmosphere temperatire
            TATM4=TATMJ**4
C       2002jul12  ADGR was downwelling  IR flux; becomes solar heating
            HEATA=ADGR(JJ)+FAC9*(EMIS*TSUR4-2.*TATM4) ! net atm. heating flux
            TATMJ=TATMJ+HEATA*DTAFAC ! delta Atm Temp in 1 time step
          ENDIF                 !^-^-^-^-^
          IF (.NOT.LDAY) GOTO 270
          TOUT(JJ)=TSUR         ! save surface temperatures at each time
          IF (JJ.EQ.JJH) THEN   !  JJH is next saving hour
            TTJ(1)=TSUR
            TSFH(IH)=TSUR       ! save hourly temperatures on last iteration
            IF (LATM) THEN      !v-v-v-v-v  with atmosphere
              TPFH(IH)=(FAC8*TSUR4+FAC82*TATM4)**0.25 ! planetary  
              TAF(IH,J4)=TATMJ  ! save Atm Temp.
              DOWNIR(IH,J4)=ATMRAD ! save downward IR flux
            ENDIF               !^-^-^-^-^
            DOWNVIS(IH,J4)=ASOL(JJ) ! save downward solar flux
D     write(iosp,*)'L299', ASOL(JJ),ATMRAD,heat,tatmj
            DO J=1,N1           ! save extreme temperatures for each layer
C..     th(j,ih)=t(j)   ! save depth-time solution [th(maxn1,MAXNH]
              IF (TTJ(J).LT.TMIN(J)) TMIN(J)=TTJ(J)
              IF (TTJ(J).GT.TMAX(J)) TMAX(J)=TTJ(J)
            ENDDO
D            J=(17*N2)/24 ! dbg  stop at H=17
D            IF (IDB2.GE.2 .AND. J5.EQ.JDISK+16 .AND. JJ.GE.J) THEN 
D              PRINT *,'AT HALT' !   set debugger break to this line
D            ENDIF
            IH = IH+1
            JJH = IH*AH+.5
          ENDIF
C
          IF (JJ.EQ.JJP) THEN   ! print "hourly" temperatures
            IF (LP3) WRITE(IOSP,260)IP,EFROST,TTJ(1)
     &        ,(TTJ(I),I=2,N1M1,NLW),TTJ(N1),tatmj
 260        FORMAT (I7,F8.3,(12F8.1))
            IP = IP+1
            JJP = IP*AP+.5      ! time step of next print
          ENDIF
 270    CONTINUE                !^+^+^+^+^+^+^+^+^+^+^+ end of time loop ^+^+^+^+^+
C
C  store results of day, calculate rms change
C
        IF (LATM .and. (TATMJ.LT.TATMIN)) THEN ! Tatm is below saturation T
          SNOW= (TATMIN-TATMJ)*CPOG/CFROST ! snow formation  Kg/m^2 in this time step
          IF (LFROST) THEN
            EFROST=EFROST + SNOW ! let it fall to surface
          ELSE
            FLOST=FLOST+ SNOW   ! record mass "lost" from system ??
          ENDIF
D       write(*,*)'jjj,Tmin,Ta,SNOW,EFR=',jjj,TATMIN,TATMJ,SNOW,EFROST !!!
          TATMJ=TATMIN          ! keep no colder that saturation
        ENDIF
C
        TTJ(1)=TSUR
        ZD=0.
        DO  J=1,N1              ! each layer
          ZD=ZD+(TTJ(J)-TT1(J,JJJ))**2 ! add square of diff from prior midnight
          TT1(J,J3P1)=TTJ(J)    ! save new midnight temperature
        ENDDO
        DTM=SQRT(ZD/N1)         ! RMS change of layer temperatures in prior day
        DTMJ(J3P1)=DTM
        QQ=FLOAT(N2+1-JJO)      ! temporary divisor: number of time steps this day
        TTS(J3P1)=TSURM/QQ      ! average surface T
        TTB(J3P1)=TBOTM/QQ      ! average bottom T
        HEAT1M=HEATFM/QQ        ! average upward heatflow into the surface
        IF (LATM) THEN          !v-v-v-v-v  with atmosphere
          TTA(J3P1)=TATMJ       ! final atm. temperature
          FRO(J3P1)=EFROST      ! final frost amount
        ENDIF                   !^-^-^-^-^
C
C  are we done?
C
        IF (LDAY.AND.(DTM.LE.DTMJ(JJJ) .OR. DTM.LE.DTMAX ! some convergence
     &        .OR. JJJ.GE.N3-1 )) GOTO 330 ! or end of season
        JJO=1
C
C  not yet - is next day the last?
C
        ZD = MAX(DTMJ(JJJ),1.E-6) ! yesterdays DTM:  temporary reuse of ZD
        IF (((ABS(1.-DTM/ZD).LE.DDT .OR. DTM.LE.DTMAX) 
     &        .AND. JJJ.GE.2) .OR. JJJ.EQ.N3-1) THEN !  initiate last day
          LDAY=.TRUE.
        ELSE                    ! not last day yet
          LDAY=.FALSE.
          IF (LRESET) THEN      ! reset lower temps to have
            TRSET=TTS(J3P1)-TTB(J3P1)
            DO  J=2,N1          !   same average
              TAVE(J)= TAVE(J)/N2 -TTS(J3P1) !
              IF (DRSET.NE.0.) THEN
                QQ=XCEN(J)/XCEN(N1)
                TTJ(J)=TTJ(J)+TRSET*(QQ+DRSET*QQ*(1.-QQ))
              ELSE
                TTJ(J)=TTJ(J)-TAVE(J) !   as  TSUR
              ENDIF
            ENDDO
            IF (LD17) WRITE(IOSP,'(I7,F8.3,F8.1,12F8.3)') !+
     &           JJJ,DTM,TTS(J3P1),(TAVE(I),I=2,N1M1,NLW),TAVE(N1) !+
          ENDIF
C  If it is the first season and enough iteration days have been done, then 
C may reset the lower layers on each successive day
          IF (J5.LE.1 .AND. JJJ.GE.JRSET) LRESET=.TRUE.
        ENDIF
 320  CONTINUE                  ! *^*^*^*^*^*^*^*^*^* end of day loop *^*^*^*^*^*^*
C
      JJJ=N3                    ! if loop finished, index value not guarenteed
 330  J3=JJJ                    ! reset the counter kept in common
      GOTO 9
C
 340  IRET=2                    !   blow-up. force a stop; print current conditions
      write(IOSP,*)'TDAY blowup: jjj,j4,j5=',jjj,j4,j5
      write(IOSP,*)'LRESET,LDAY,tsur=',LRESET,LDAY,TSUR
      IF (LATM) write(IOSP,*)'atm items=',atmrad,abrad,LFROST,EFROST
      write(IOSP,*)'sheatf,power,delt=',sheatf,power,delt
      TTJ(1)=TSUR
      J2=JJ
      J3=JJJ
      CALL TPRINT (7)           ! print message and  TTJ
      WRITE(IOSP,*) 'DELT,TATMJ,TBLOW=',DELT,TATMJ,TBLOW
      CALL TDISK(2,I)           ! write current season
      CALL TDISK(4,I)           ! close the file
      CALL TPRINT (2)           ! print full input set
      CALL TPRINT (4)           ! print daily convergence
C
 9    IF (IDB2.GE.6) WRITE(IOSP,*) 'TDAYx'
D       print *,'TDAY: j5,j4',j5,j4     !<dbug
D       print *,ihist           !<dbug
      RETURN
C     
      END
