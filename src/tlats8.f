      SUBROUTINE TLATS8 (IQ,IRET)
C_Titl  TLATS8  latitude computations for the  KRC thermal model system DP
C_Vars
      USE array_structs
      INCLUDE 'krcc8m.f'  ! has  IMPLICIT NONE 
      INCLUDE 'latc8m.f'
      INCLUDE 'dayc8m.f'
      INCLUDE 'hatc8m.f'
      INCLUDE 'filc8m.f'
      INCLUDE 'porbc8m.f'      ! need only for no-atm version
      INCLUDE 'unic8m.f'
C_Args
      INTEGER*4 IQ    ! in.  Not used
      INTEGER*4 IRET  ! out. return codes: 1=normal 
C                     2,3,4= error of same number in TDAY 
C                     5=no matching latitude in fff
C                     6=number of timesteps not integral multiple of hours in fff
C                     7=ECLIPSE failure here on in TDAY
C                     30=mixing pits with Solar Diffuse Flux Tables.
C_Calls  AVEDAY+  AVEYEAR+  EPRED8  
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
C 2014may31 HK Add call to TUN8(I15=102 to output fluxes on each hour
C 2015dec09 HK Minor comment typos only
C 2016feb06 HK Redefine IIB to allow geothermal flow. Non-Lambert surface.
C 2016may16 HK Incorporate far=flat capabilitiies
C 2016jul04:31 HK Add Keihm and Vasavada photometric functions. Set by ARC2
C    0=Lambert  -0.x=Minnaert  -1=Lommell-Seeliger  +x=Keihm/Vasavada 
C  Note: code as if could have photometric function with an atmosphere, 
C  but version 3 has no free parameters to undo the overload of ARC2 
C 2016sep09 HK Do not print T-equilb unless IDB2.ge.5
C 2016oct02 HK Correct effective azimuth of tilted surfaces by reversing 
C   diurnal Sun motion. Also, revise method of getting tilt surface normal.
C   Remove ancient comments and code using trigonometry to get incidence angles
C 2017mar03 HK Fix blunder in Minnaert and Lomell-Seeliger PUH. 
C 2017mar12 HK Add eclipse function and planetary fluxes
C 2017may03 HK Replace calls to  CO2PT with  GASPT, enabling any condensing gas
C 2017sep28 HK Check that Sun not below horizon for slopes
C 2018jun26 HK Ensure  TATMIN is set when no atm., although should not be used.
C      Force initiation of TTA, TTJ and FRO 
C      Increase the capability of EPRED8 and remove the code no extrapolation.
C 2018nov05 HK Prepend D to lines activated by IDBx
C 2020apr11 HK Replace archaic DMIN1 and AMIN1 by MIN
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C
      REAL*8 COSIAM(2)            ! cos_incidence angle: average and noon
      REAL*8 COLL       ! returned by Delta-Eddington
C
      REAL*8 ACOSLIM,AH,AINC,ANGLE,AVEE,AVEI
     &,BOTDOWN,BOUNCE,CC,CD,CL,COSAM,COSI,COSP,COSZLIM,COS2,COS3
     &,DIFFUSE,DIP,DIRECT,F23,FP,G1,GHF   ! ,DIFAC
     &,PCAP,RANG,RLAT,RSDEC,SAZ
     &,SD,SL,SOLR,SS
     &,TBOT,TOPUP,TSEQ4,TSUR,TWILFAC,TWILIM
      REAL*8 PGASG ! Partial pressure of condensible gas; current
      REAL*8 PGASM ! " " ; initial conditions
      REAL*8 QI,QA,QH,QHS,QS      ! temporary use
      REAL*8 FXX(3),HXX(3),MXX(3),TXX(3) !  Cartesian vectors
      REAL*8 QXX(3),PXX(3) ! " ":  Q=Temp.  P=To Planetary heat source
      REAL*8 SUMH, SUMV  ! sum the  Planetary  IR and  Visual over a day
      REAL*8 TEQQ(MAXN4)        ! equilibrium temperature at first season
C -------- variables related to albedo

C The reflectance factors are computed here but invoked in  TDAY; except the 
C  bounce albedo is used here; so would be incorrect if frost changes
C -------- variables related to albedo
C in COMMON's (not complete list)
C krcc ALB               Input albedo
C hatc SALB           ! spherical albedo of the soil
C hatc ALBJ(MAXN2)    ! hemispherical albedo at each time of day
C hatc SOLDIF(MAXN2)  ! Solar diffuse (with bounce) insolation at each time W/m^2 
C dayc ASOL(MAXN2)    ! Direct solar flux on sloped surface at each time of day
      REAL*8 AVEA ! hemispheric albedo for Lambertian surface or frost
      REAL*8 AVET ! hemispheric albedo at each time step using Photometric func.
      REAL*8 HALB ! hemispheric albedo for sloped surface, not frost. ->  ALBJ
      REAL*8 PUS       ! spherical albedo factor  P_s
      REAL*8 PUH       ! hemispheric albedo factor  P_h, depends on i, not frost
      REAL*8 DIRFLAT   ! irradiance onto horizontal surface
      REAL*8 PFAC1,PFAC2,THETA  ! photometric factors, angle in radians
C -------- 
      INTEGER*4 I,IH,IRL,J,JJ,JJH,JHOT,J3P1,KODE,NFFH
      INTEGER*4 KOP             ! photometric function index
      LOGICAL LINT,LQ1,LQ2,LQ3,LTW
      LOGICAL LPH               ! consider planetary heat load

      REAL VLPRES          ! Function names. Default precision
      REAL*8 AVEDAY,AVEYEAR,EPRED8     ! Function names 
      REAL*4 DJU54,SUBS4  ! for *8 to *4
      REAL*4 Z4              ! for *4 to *8
      REAL*8 DUM8
      REAL*8 DHALF /0.5d0/      ! 1/2
      INTEGER*4 JJJ(10)         ! sizes to go to  BINF5
      CHARACTER*2 BUFF
      CHARACTER*80 FUFF         ! name of high-res file.bin5
      CHARACTER*200 HEADTX      ! will go into bin5 header
      INTEGER*4 HEADLEN /200/   ! 60 used
C needed for fff
      INTEGER*4 NHF,NLF
      INTEGER*4 MEMI(8)         ! fff info
      REAL*8 WORK(MAXNH+3)      ! to hold extended hours
      REAL*8 FFELP(10+MAXN4)    ! Information from fff IOD3 KRCCOM
      REAL*8 DLATEST / 0.1D0 /  ! fff latitude match tolerance 
      REAL*8 TENS /0.5D0/       ! spline tension
      REAL*8 FAC5X              ! factor from T^4 to radiance
C      SAVE FAC5X,FFELP,LINT,NFFH,NHF,NLF
   
      LQ1=IDB2.GE.5             ! once per season or latitude
      LQ2=IDB2.GE.9             ! each time of day
      
      IF (LPLANHTAB .OR. LPLANVTAB) THEN
        LPH = .TRUE.
      ELSE
        LPH = PARW(1).GT.0.      ! doing planetary heat loads
      ENDIF

C
      IRET=1          ! set return code to normal
      I=IQ            ! simply to avoid compiler complaint that  IQ is not used

      TWILFAC = 1.            ! twilight not allowed
      TWILIM = 0.             ! " 
      LTW=TWILFAC.LT.1.0        ! Twilight present flag

C     
C============ factors that do not depend upon season ===================
      RANG=2.0D0*PIVAL/N2      ! time step expressed in radians
      F23=2.D0/3.D0
      JHOT=INT(FLOAT(N24)*13.5D0/24.D0) ! index of warmest time of day
      IF (SLOAZI .GT. -360.D0) THEN ! slope, if any, is regional
         SKYFAC = (1.D0+ DCOS(SLOPE/RADC))/2.D0 ! effective isotropic radiation.
         COSZLIM=0.            ! zenith angle default limit is 90 degrees
      ELSE                  ! slope is of conical pit wall
        IF (LASOLTAB .OR. LSOLDIFTAB) THEN   ! Error, never use pits with ASOL or SOLDIF tables.
          IRET=30
          GOTO 9
        ENDIF
         QA=(90.D0-SLOPE)/RADC ! zenith angle of slope, in radians
         QI=DSIN(QA)
         SKYFAC = QI**2          ! effective sky for isotropic radiation.
         COSZLIM=DCOS(QA)      ! tangent to the pit wall 
      ENDIF
      GHF=0.
      IF (IIB.GT.0) GHF=0.001*DBLE(IIB) ! Geothermal Heat-flow  value
      IF (J5.EQ.1 .AND. LOPN3) THEN ! prepare for fff each season 
C arg5 contains FFELP, so must be at least 10+MAXN4
        CALL TFAR8(2,QA,QH,MEMI,  FFELP,DUM8,DUM8) ! Arg2,6,7 not used
        IF (QH .LT. 0.) THEN    ! error happened
          IRET=30+NINT(-QH) ! 31 to 36
          GOTO 9
        ENDIF
        NFFH=MEMI(1)            ! number of hours in fff
        NHF=N2/NFFH             ! number of timesteps / hours in fff
        IF ( MOD(N2,NFFH).NE.0 ) THEN ! fatal error
          IRET=37
          GOTO 9
        ENDIF
        LINT=(NHF.GE.2) ! will need to interpolate hour
        NLF=MEMI(2)        ! number of latitudes  in fff
        FAC5X=(1.-SKYFAC)*EMIS*SIGSB*FFELP(7) ! last is fff surface emissivity
        I=1
        IF (LINT) CALL CUBUTERP8 (I, NHF, TENS,WORK,FARAD) ! set up interpolation
C                                         ^ignore last 3 args
      ENDIF
C============ factors constant over latitude that depend upon season ==========
C precision conversion
      DJU54=SNGL(DJU5)
      SUBS4=SNGL(SUBS)
C     
      SOLR=SOLCON/(DAU*DAU)     ! solar flux at this heliocentric range
      RSDEC=SDEC/RADC           ! current solar declination in radians
      SD=DSIN(RSDEC)
      CD=DCOS(RSDEC)
C  Define all unit vectors in "Day" system.  Here, Sun at midnight
      MXX(1)=CD                 !|  X axis in equitorial plane toward midnight
      MXX(2)=0.                 !|  Y axis in equitorial plane, right-hand
      MXX(3)=SD                 !|  Z axis toward planet north pole
      DIP=SLOPE/RADC            ! dip in radians
      SAZ=SLOAZI/RADC           ! azimuth of dip, east from north
C       set blowup test to a factor larger than perpendicular black surface
      TBLOW = 2.0D0*(SOLR / (EMIS*SIGSB))**0.25D0
C      
C     get current total pressure at 0 elevation
      IF (N4.GT.8) THEN         ! use global integrations
        PCAP = 0
      ELSE
        PCAP=0.
      ENDIF
      PGASM = (1.-FANON)*PTOTAL ! initial partial pres. of condGas at 0 elev
      IF (KPREF.EQ.0) THEN         ! constant 
        PZREF = PTOTAL             ! current total pressure at 0 elevation
        PGASG = PGASM              !  partial pres. of condGas at 0 elev. now
      ELSEIF (KPREF.EQ.1) THEN  ! follows Viking
        KODE=4                     ! average of all years and both landers
        Z4=VLPRES(KODE, DJU54)     ! current normalized pressure
        PZREF = PTOTAL*DBLE(Z4)    ! current total  P at 0 elevation
        PGASG = PGASM+(PZREF-PTOTAL) ! all changes are pure condGas
      ELSEIF (KPREF.EQ.2) THEN  ! based on polar cap balance
        PZREF = PTOTAL - PCAP
        PGASG = PGASM -PCAP        ! all changes are pure condGas
      ENDIF
C     
      IF (LPGLOB) THEN          ! print global properties
         CALL TPRINT8 (8)       ! print page heading
      ENDIF
C
      J4=0
C  ----------------new latitude loop loop loop--------------------------
C
 100  J4=J4+1
      DLAT=ALAT(J4)            ! current latitude, degrees
      PARC(11)=DLAT             ! current latitude ( may need in TDAY


      LQ3=LD19 .AND. (J5.EQ.JDISK) .AND. (J4.EQ.1) ! first recorded season

      IF (LOPN3) THEN           ! find matching latitude, interpolate
        J=0
        DO I=1,NLF
          IF ( ABS(DLAT-FFELP(10+I)) .LE. DLATEST ) J=I ! latitude matches
        ENDDO
        IF (J .LT. 1) THEN       ! no match found
          IRET=38                ! signal an error
          GOTO 9
        ENDIF
        IF (LINT) THEN          ! interpolate fff to each timestep
          CALL MVD( FARTS(1,J,1), WORK(3), NFFH) ! copy Tsurf with space at front
          DO I=3,NFFH+2         ! all those extracted
            WORK(I)=WORK(I)**4 *FAC5X ! convert from temperature to radiance
          ENDDO
          WORK(1)=WORK(NFFH+1)  ! wrap last-1 to front
          WORK(2)=WORK(NFFH+2)  ! wrap last to next
          WORK(NFFH+3)=WORK(3)  ! wrap first to end
          KODE=2
          CALL CUBUTERP8 (KODE,NFFH, TENS,WORK,FARAD) ! Interpolate  Ts radiance
        ELSE
          CALL MVD( FARTS(1,J,1), FARAD, NFFH) ! no time-density increase
        ENDIF

      ENDIF                     !  LOPEN3
      RLAT=DLAT/RADC            ! latitude in radians
      CL=DCOS(RLAT)
      SL=DSIN(RLAT)
      SS=SL*SD          
      CC=DMAX1(1.D-10,CL*CD)
      COSIAM(1)= DMAX1 (1.D-6,AVEDAY(SDEC,DLAT)) ! get average cosine incidence
      COSAM=-SS/CC            !  find length of day and daylight intervals
      IF (DABS(COSAM).LT.1.) THEN
         DAM=RADC*ACOS(COSAM)      ! occurs in  COMMON. Used only in tprint.f
      ELSEIF (COSAM.GE.1.) THEN
         DAM=0.
C;         COSIAM(2) = 1.E-6    ! sun never rises
      ELSE
         DAM=180.D0
      ENDIF
C Vector algebra version 2012may31  !VAv
      FXX(1)=-CL                ! unit vector in Day system toward the 
      FXX(2)=0.                 ! regional zenith at noon
      FXX(3)=SL                 ! 
      IF (SLOPE .EQ. 0.) THEN
        CALL VEQUAL(FXX, TXX)    ! tilt normal is same as flat normal
      ELSE    ! 2016oct02 Two rotations to create
        CALL ROTV (FXX,2,DIP, QXX) ! rotate flat surface normal North by the dip
        CALL VROTV(QXX,FXX,-SAZ, TXX) ! rotate  E around  F by the slope azimuth
      ENDIF

      QA=PARW(7)*(PIVAL/12.) ! omega= polar azimuth of planet heat source, radian
      PXX(1)=COS(QA)            ! | in  Day system.  Unit vector 
      PXX(2)=SIN(QA)            ! | to planet source
      PXX(3)=0.                 ! | assumed to be in equitorial plane
      CALL VDOT(PXX,TXX,COSP)   ! assume synchronous
      COSP=MAX(COSP,0.D0)       ! never negative

C       
      AVEE=EMIS
      AVEA=ALB ! surface albedo; will be frost if frosty at end of prior day
C     

C Photometric function in lat loop as  PHOG can depend upon frost
      IF (PHOG.EQ.0.)  THEN     ! will use Lambert
        KOP=1  
        PUS=1.                  ! spherical albedo /  AH0         
      ELSEIF (PHOG.LE.-1.) THEN ! will use  Lommel-Seeliger
        KOP=2   
        PFAC1= 0.30685282D0        ! uo ln[uo/(1+u0)]+1 when uo=1
        PUS=1.3333333D0
      ELSEIF (PHOG.GT. 0.) THEN ! will use  Keihm-Vasavada Form 
        KOP=4                   ! expect 0<  PHOG  <1
        PFAC1=PHOG*(4.D0/PIVAL)**3 ! x * f3  in Eq. 12
        PFAC2=(0.14D0/0.12D0)*(2.D0/PIVAL)**8 ! f8 in Eq. 12
        PUS=1.05944+0.05944*PHOG  ! Eq. aak = 14
      ELSE                      ! must be -, will use  Minnaert
        KOP=3
        PFAC1=-PHOG             !  exponent nu or k : expect 0< k  <1
        PFAC2=PFAC1-1.D0        ! the exponent on: mu0==cos i
        PUS=2.D0 /(1.D0+PFAC1)
      ENDIF
      SALB=PUS*AVEA              ! spherical albedo, for diffuse irradiance

      ACOSLIM = AMAX1(OPACITY/EXPMIN,0.001D0) ! limit to avoid math checks
C3      IF (LQ3) WRITE(IOSP,701)'LQ3',NCASE,J5,J4,TATMAVE,PRES,OPACITY
C3 701  FORMAT(A4,I3,I5,I3,2F12.6,F12.8)
C     
C  Insolation at mid-step and its average.  Time origin is midnight.
C angle from noon is (j2/n2 * 2.pi) - pi, so cos of this is -cos(j2/n2 *2pi)
C cosi = cosine of solar incidence angle onto horizontal
C cos2 = cosine of solar incidence angle onto tilted surface
C cos3 = cosine of twilight angle onto horizontal
C cosp = cosine of planet heat onto tilted surface
      CALL FILLD(0.D0,PLANH,N2) ! ensure 0 unless  LPH true
      CALL FILLD(0.D0,PLANV,N2)
      AVEI=0.                   ! to sum solar absorbed by slope surface
      SUMH=0.                   ! to sum planetary  IR
      SUMV=0.                   ! to sum planetary  Vis
      IH = 1                    ! saving "hour" count
      AH = DBLE(N2)/DBLE(N24)   ! time steps between saving results
      JJH = NINT(AH+DHALF)       ! round to time step of first saving

Cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      DO JJ =1,N2            ! ------------------time of day loop----------
         ANGLE=(DBLE(JJ)-DHALF)*RANG ! rotation angle at middle of time step
         CALL ROTV(MXX,3,-ANGLE, HXX) ! VAv  Sun progress thru day
         CALL VDOT(HXX,FXX, COSI) ! cos of incidence angle on horizontal
         CALL VDOT(HXX,TXX, COS2) ! cos of incidence angle onto tilted slope
C     Get heating for horizontal surface
         IF (COSI.GT.ACOSLIM) THEN ! Day: Sun is above horizon
C PhotFunc for horizontal surface
          SELECT CASE (KOP)  !vvvvvvvvvvvvvvvvvvvvvvvv
          CASE(1)            !  Lambert
            PUH=1.D0         ! default photometric factor
          CASE(2)            !  Lomell-Seeliger 
            PUH= (1.D0+COSI*DLOG(COSI/(1.D0+COSI)))/PFAC1 !  Lommel-Seeliger
          CASE(3)            !  Minnaert
            PUH= COSI**PFAC2 !  Minnaert
          CASE DEFAULT       ! must be  Keihm or  Vasavada
            THETA=ACOS(COSI) ! incidence angle in radians
            PUH= 1.D0+PFAC1*THETA**3+PFAC2*THETA**8
C     1.+0.25 *(theta/r45)**3+1.17*(theta/r90)**8  Keihm
C     1.+0.375*(theta/r45)**3+1.17*(theta/r90)**8  Vasavada
          END SELECT         !^^^^^^^^^^^^^^^^^^^^^^^^
          AVET=MAX(MIN(ALB*PUH,1.D0),0.D0) ! ensure  1-A cannot be negative
C daytime up/down fluxes
C As opacity goes to zero, COLL->1., topup-> cosi*ALB, botdown->0 atmheat->0
            TOPUP=COSI*AVET         ! upward solar 
            BOTDOWN=0.         ! no atm scattering
            COLL=1.D0          ! no atm attenuation of beam
            DIRFLAT=COSI ! incident intensity on horizontal unit area
          ELSE     ! night: set several values for dark
            DIRFLAT=0.
            TOPUP=0.
            COLL=0.      
        ENDIF

C  ASOL = coll. flux onto (sloped) surface 
C  DSOL = diffuse flux onto ?? surface
C Get diffuse insolation, including twilight and first-order surface reflection 
         IF (COSI.GT.TWILIM) THEN ! in day or twilight zone
           COS3= DCOS(TWILFAC*ACOS(COSI)) ! twilight effective cosine
           BOTDOWN=0.
           DIFFUSE=SKYFAC*BOTDOWN ! diffuse flux onto surface
           IF (SLOAZI .LE. -360.D0) THEN ! bounce in a pit
             AINC=ACOS(COSI)*RADC ! incidence angle in degrees
             G1=MIN (1.D0,(90.D0-AINC)/SLOPE) ! (90-i)/slope
           ELSE
             G1=1.0D0
           ENDIF 
           BOUNCE=(1.D0-SKYFAC)*SALB*(G1*DIRFLAT+DIFFUSE) 
         ELSE
           DIFFUSE=0.
           BOUNCE=0.
         ENDIF
C     
C     Set direct surface insolation
         IF (COS2.GT.COSZLIM .AND. COSI.GT. 0.) THEN ! target directly illumin.
            SELECT CASE (KOP)  !vvvvvvvvvvvvvvvvvvvvvvvv
            CASE(1)            !  Lambert
              PUH=1.D0         ! default photometric factor
            CASE(2)            !  Lomell-Seeliger
              PUH= (1.D0+COS2*DLOG(COS2/(1.D0+COS2)))/PFAC1 ! Lommel-Seeliger
            CASE(3)            !  Minnaert coeff later 
              PUH= COS2**PFAC2 !  Minnaert
            CASE DEFAULT       ! must be  Keihm or  Vasavada
              THETA=ACOS(COS2) ! incidence angle in radians
              PUH= 1.D0+PFAC1*THETA**3+PFAC2*THETA**8
            END SELECT         !^^^^^^^^^^^^^^^^^^^^^^^^
            HALB=ALB*PUH       ! normalized hemispherical albedo
          DIRECT=COS2*COLL     ! slope is in sunlight
         ELSE
           DIRECT=0.            ! Target is in shadow
           HALB=ALB  ! but used only  *DIRECT, so value does not matter 
         ENDIF
C     
        IF (LASOLTAB) THEN ! new vis and ir flux tables
         QI = f_get_jd_lt_asol(J5 - 1, (real(JJ, 8))/N2)
        ELSE 
          QI=DIRECT*SOLR         ! collimated solar onto slope surface
        ENDIF
        
         ASOL(JJ)=QI            ! collimated insolation onto slope surface
        ! write into ASOL(JJ) with value from table
         ALBJ(JJ)=MAX(MIN(HALB,1.D0),0.D0) ! current hemispheric albedo

         IF (LSOLDIFTAB) THEN ! Flux table total solar flux, including bounce and diffuse
           DIFFUSE = f_get_jd_lt_soldif(J5 - 1, (real(JJ, 8))/N2)  ! reusing DIFFUSE
           SOLDIF(JJ) = DIFFUSE*SKYFAC 
         ELSE
          SOLDIF(JJ)=(DIFFUSE+BOUNCE)*SOLR ! all diffuse, = all but the direct.
         ENDIF

         IF (LPH) THEN ! add planetary heat loads
           QA=ANGLE+PIVAL ! add 1/2 rev to convert from Hour to orbital phase
           
           IF (LPLANHTAB) THEN
             PLANH(JJ) = f_get_jd_lt_planh(J5 - 1, (real(JJ, 8))/N2)
           ELSE
             PLANH(JJ)=COSP*(PARW(1)+PARW(2)*COS(QA-PARW(3)/RADC)) ! thermal
           ENDIF

           IF (LPLANVTAB) THEN
             PLANV(JJ) = f_get_jd_lt_planv(J5 - 1, (real(JJ, 8))/N2)
           ELSE
             PLANV(JJ)=COSP*(PARW(4)+PARW(5)*COS(QA-PARW(6)/RADC))
           ENDIF

           SUMH=SUMH+PLANH(JJ)
           SUMV=SUMV+PLANV(JJ)
         ENDIF

         AVEI=AVEI+(1.d0-ALBJ(JJ))*QI+(1.-SALB)*SOLDIF(JJ) ! sum energy into surf
C3         IF (LQ3) WRITE(88,777)JJ,COSI,COLL,HUV,QI,DIRECT,DIFFUSE,BOUNCE
C3     & ,HALB,ALBJ(JJ)
C3 777      FORMAT(I5,2f11.7,2f12.6,3f11.7,2f9.5)
         IF (JJ.EQ.JJH) THEN       !  JJH is next saving hour
           TOFALB(IH,J4)=TOPUP  ! in  HATCOM, but never used
           IH = IH+1            ! increment to next hour
           JJH = NINT(REAL(IH)*AH) ! next time-step to save
         ENDIF
C         IF (J5.EQ.IDB4) WRITE(53,531)J4,JJ,TATM,AVET,HUV ; 2018jun
C 531     FORMAT(I4,I6,F9.4,F8.5,G12.5)                    ; 2018jun
      ENDDO                  !-------------end time-of-day loop-----------
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

C       find equilibrium temperature at current latitude
C     
      AVEI=AVEI/DBLE(N2)        ! average absorbed insolation
      IF (LPH) THEN             ! add in absorbed planetary heating
        SUMH=(EMIS*SUMH + (1.-SALB)*SUMV)/DBLE(N2) ! average value
      ELSE
        SUMH=0.
      ENDIF
      BETA=0.
C       start by using annual average insolation
      QA=1.D0/(DSQRT(1.D0-XECC**2)) ! average orbit insolation factor
      QS=AVEYEAR(RADC*BLIP,DLAT)    ! Ave. fraction.  Args. in degrees
      AVEI=QA*QS*SOLCON/SJA**2      ! average insolation in  W/m^2
      TSEQ4=((1.D0-AVEA)*AVEI+GHF+SUMH)/(SIGSB*AVEE)
      TEQUIL=TSEQ4**0.25D0 ! equilib  T_s
      IF (TEQUIL.GE.TBLOW) THEN !db, separate line so dbg can break
        WRITE(IOSP,*)'Case,J5,J4,TEQ+',NCASE,J5,J4
     & ,TEQUIL,AVEA,AVEI,GHF,SIGSB,AVEE !db
      ENDIF
      IF (J5.LE.1) TEQQ(J4)=TEQUIL ! save initial Tequilib.
C                          WRITE(IOPM,*)'TLATS: J5,J4,TEQUIL',J5,J4,TEQUIL
      JJO=1
C       if at start, use linear profile, else  continuing from prior season
      IF (J5.LE.1) THEN      ! start with linear profile
        IF (N3.GT.3) JJO=NINT(.75*N2)+1 ! start first day just past 3/4 day
        TSUR=TEQUIL             !  isothermal if bottom is insulating
        TBOT=TEQUIL             !  "  "
        IF(IIB.LE.-1) TBOT=TDEEP  ! case for constant bottom  T
        IF(IIB.LT.-1) TSUR=TDEEP  ! case for isothermal initial condition
C       
        DO I=1,N1               ! initial linear profile
          TTJ(I)=TSUR+(TBOT-TSUR)*(XCEN(I)-XCEN(1))/(XCEN(N1)-XCEN(1))
        ENDDO
        TTJ(N1PIB)=TBOT
        TTS(1)=TEQUIL           ! Initiate the midnight values
        TTB(1)=TEQUIL
        CALL MVD(TTJ,TT1,N1) ! temperature profile. N1 layers first day 
      ELSE                      ! start with final value from previous season
        TTS(1)=TTS4(J4)
        TTB(1)=TTB4(J4)
        CALL MVD(TMN4(1,J4),TTJ,N1PIB) !|| layer temperatures for finite-diff.
        CALL MVD(TMN4(1,J4),TT1,N1PIB) !|| copy forecast midnight values
      ENDIF
C        write(iosp,*) asol
      if (idb2 .ge. 6) WRITE(IOPM,*)'l672',j5,j4,tt1(4,1) ! HKX
      IF (LP3) CALL TPRINT8 (3) ! print header for hourly summary
C====== 
C       
      CALL TDAY8 (2,IRL)      ! execute day loop
C       
C====== 
      IF (IRL .GT. 1) THEN      ! if blow-up; stop the model
        IRET=20+IRL
        GOTO 9
      ENDIF
C       save results for current latitude
      IF (I15.EQ.102 .AND. J5.GE.JDISK) CALL TUN8(I15,2,I,SOLDIF) !  I is dummy
C       
      J3P1=J3+1
C       i=n24/2                  ! noonish
C       
      if (idb2 .ge. 6) WRITE(IOPM,*)'post',j5,j4,tt1(4,j3),tt1(4,j3p1) ! HKX
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
      FP=DELJUL/PERIOD-J3      ! undone iterations to end of this season.
C The 5 items predicted are stored for the end of the day prior to their index;
C so they are defined up to J3P1.  DTMJ(1)==-1.
C If 2 or more new days were computed this season, do asymptotic prediction.
C If have 1 new day, do linear predication
C 2018jun27 Revise  EPRED8. Arg1 is the full array, arg3 is index of last valid Y
C  EPRED8 Assumes 3rd item in arg1 is the last valid, can handle any value of arg3 
C  J3P1 here may be less than 3, but EPRED8 will ignore the undefined address space
      DO I=1,N1                 ! predict next season's layer temperatures
        CALL MV21D (TT1(I,1),MAXN1,WORK,J3P1) ! transfer all defined values for layer I
        TMN4(I,J4)= EPRED8(WORK,FP,J3P1, 1,TBLOW) ! forecast to end of season
      ENDDO
        if (idb2 .ge. 6) WRITE(IOPM,*)'l721',j5,j4,fp,j3p1,tblow ! HKX
      TTS4(J4)    = EPRED8(TTS,FP,J3P1, 1,TBLOW) ! surface average
      TTB4(J4)    = EPRED8(TTB,FP,J3P1, 1,TBLOW) ! bottom layer average

      IF (IIB.LE.-1) TMN4(N1PIB,J4)=TTJ(N1PIB) ! reset bottom T
      TEXTRA(J4,1) = TTS4(J4)-TTS(J3P1) ! amount of extrapolation,Top
      TEXTRA(J4,2) = TTB4(J4)-TTB(J3P1) ! " " , bottom
C       TAX(MAXN1,J4)= TEXTRA(J4,1) ! overload into tax
C       TIN(MAXN1,J4)= TEXTRA(J4,2) ! overload 
      HEATMM(J4) = HEAT1M       ! daily average surface heat flow
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
          WRITE(76,762)QH,TSFH(I),QS,TPFH(I)
        ENDDO
      ENDIF

      IF (J4.EQ.NLAD .AND. J5.EQ.N5) THEN ! write hi-res  Tsurf file 
        CALL FILLI (0,JJJ,10)   ! initiate bin5 control 
        JJJ(1) = 1              ! # of dimensions
        JJJ(2) = N2             ! Number times of day
        JJJ(8) = 5              ! set type as  REAL*8
        JJJ(9) = HEADLEN        ! header length
        I=NCASE/10              !| convert case number to string
        J=NCASE-10*I            !| assumes it is 99 or less
        WRITE(BUFF,'(2I1)')I,J  ! 2-digit case number
        I=LEN_TRIM(FRUN)        ! output stem
        J=LEN_TRIM(FTOUT)       ! stem the user defined on change 16
        FUFF=FRUN(1:I)//'tout'//FTOUT(1:J)//'c'//BUFF//'.bin5' ! full name
        HEADTX=VERSIN//' TLATS output of TOUT' ! 
        I=IDB3                  !  +1=report many values   +2 report progress 
        CALL BINF5 ('W',FUFF,HEADTX,JJJ,TOUT,I) ! 
        WRITE (IOSP,*)'TLATS wrote ',FUFF,'  iret= ',I
      ENDIF

      IF (J4.LT.N4) GO TO 100   !^^^^^^^^^^^^^^^^^^^^ end of  Latitude loop

C       
C       ---------------------------------------------------------------------
C       
 9    CONTINUE
      RETURN
      END
