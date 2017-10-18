      SUBROUTINE ECLIPSE (PARR,PARI, JBE, OUTT)
C_Titl  ECLIPSE  Calculate insolation profile through an eclipse
C_Vars
      IMPLICIT NONE
      INTEGER*4 NPARR, NPARI
      PARAMETER (NPARR=10)
      PARAMETER (NPARI=2)
      REAL*8 PARR(NPARR)        ! in. parameters
C 1:  Style: 0=none  1=Daily  1.3+=Rare, and  NINT is the layer factor
C 2:  Distance to sun,  AU (used to get  Sun angular diameter)
C 3:  Occulting body  (OB) radius, km
C 4:  OB to  Eclipsed body  (EB) mutual orbit radius, km
C 5:  Eclipsed body radius km (significant for  Phobos/Mars)
C 6:  Mutual solar synodic period, days
C 7:  Sun-line path closest approach to  OB center, as fraction of  OB radius
C 8:  J2000 date of eclipse (not used here)
C 9:  Eclipse central hour (of 24)
C 10:  Debug code. ne.0 prints constants and >1 prints that point in detail.
C            Negative will return all  OUTT as 1.
C x:  Extinction scale height of  OB's atmosphere, km  NOT implimented
C  NOTATION ctime means coarse time -steps, as used in  TDAY
C           ftime means fine time-steps, as used in  TFINE
C           "time-steps" may be either, depending upon context.
      INTEGER*4 PARI(NPARI)  ! in. parameters
C 1:  Number of coarse time steps per period
C 2:  Logical unit to use for error messages
      INTEGER JBE(2) !both.  in: (1)=0 means do only the indices  =1: and  OUTT
!       out:  indices of the first and last ctime-steps in eclipse,
!        in the system specified by  N2
      REAL*8 OUTT(*)       ! both, insolation multiplier, the fraction of the
!   un-eclipsed insolation. if daily,  N2 steps will be defined, covering the 
!   entire sol.  If rare, time steps are smaller by  K=  round(PARR(1))  squared,
!  K*(JBE(2)-JBE(1)+1)  will be defined, and apply to ctime-steps begining
!   at  JBE(1)-1.  If error, first value will be negative.
C_Desc
C  Decide to keep inputs in physical units and normalize internally.
C  All obscuration calculations are done in units of time steps.
C  Formula from  http://mathworld.wolfram.com/Circle-CircleIntersection.html
C   coded and tested in  IDL as circleoverlap.pro
C
C  For daily, computes insolation factors for the full day on the  KRC time grid.
C  For rare: uses finer time steps for only the  KRC ctime steps covering
C   the eclipse, but ensures they correspond to complete  ctime-steps
C   i.e.,  (JBE(2)-JBE(1)) * layer_factor^2  starting at start of  JBE(1)
C
C  The instant of time-step 1 for insolation is 0-based +1/2 timestep into
C   the sol, or 1-based -1/2 timestep.
C
C_Hist 2017mar11:Apr03  Hugh  Kieffer for  KRC eclipses
C  Comments use 2 spaces before uppercase to support the  FLOWER algorithm
C_End
      REAL*8 ALAP ! area of overlap between two circles
      REAL*8 AU /149.5978707D6/ !  Astronomical unit, km
      REAL*8 SEQR /695700./     !  Sun equitorial radius, km
      REAL*8 DAYSEC /86400.D0/  ! seconds/day
      REAL*8 DHALF /0.5D0/      ! 1/2
      REAL*8 PIVAL /3.1415926536D0/ ! pi
      REAL*8 FRAB
      REAL*8 FRAK ! fraction of insolation that gets to target
      REAL*8 SRAD
      REAL*8 OBR
      REAL*8 RMO
      REAL*8 EBR
      REAL*8 EBPER
      REAL*8 VEL
      REAL*8 BIAS ! sun-line miss-distance from center of ob,

      REAL*8 CN2
      REAL*8 PERIOD
      REAL*8 SUNR
      REAL*8 X
      REAL*8 PHI
      REAL*8 HALFT
      REAL*8 B,R  ! radii of the bigger and smaller intersecting circles,scaled
      REAL*8 B2,R2   ! square of  B and  R after scaling
      REAL*8 BPR,BMR !  B+R and  B-R after scaling
      REAL*8 ASUN
      REAL*8 DANG
      REAL*8 D,D2,TWOD ! circle center separatiion after scaling, squared and *2
      REAL*8 P1,P2,P3,P4,PP  ! parts of sqrt term in overlap, and their product
      REAL*8 ANG1,ANG2, SQP  ! parts of the overlap equation
      REAL*8 ehour
      REAL*8 QSEP ! distance from  OB to surface of  EB, km
      REAL*8 Y2,DN2
      REAL*8 ZENA ! zenith angle of the eclipse, radians
      REAL*8 FMIN, TFAC
      REAL*8 FE7,FE8 ! ctime-steps at the beginning and end of eclipse

      INTEGER IOERR,J,J7,J8,KODE,KFAC,N2,NOUT
      LOGICAL LRARE

C transfer arguments
      N2=PARI(1)                ! number of coarse tie steps in a period
      IOERR=PARI(2)             ! error unit
      LRARE = (PARR(1).GE. 1.3) ! eclipse on single day
      KFAC=NINT(PARR(1))        ! layer factor
      SRAD=SEQR/(PARR(2)*AU)    ! solar radius, radians
      RMO=PARR(4)               !  Mutual orbit radius, km
      OBR=PARR(3)/RMO           !  Occulting body  (OB) radius, scaled
      EBR=PARR(5)/RMO           !  EB radius, scaled
      PERIOD=PARR(6)            !  EB orbit period, days
      FRAB=PARR(7)              ! fractional bias of sunline from center of  OB
      ehour=PARR(9)             ! local time in solar day (24H) of eclipse
      KODE=NINT(PARR(10))       ! debug kode

      BIAS=FRAB*OBR             ! sun line closest approach to  OB center, scaled
      EBPER=PERIOD*DAYSEC      !  EB orbit period, seconds
      KFAC=KFAC*KFAC            ! time factor
      TFAC=DBLE(KFAC)           ! time factor as real
      DN2=dble(n2)
c      VEL=2.*PIVAL*RMO/EBPER    ! orbital velocity, km/sec
      CN2= DN2*ehour/24.0D0  ! real timestep at center of eclipse
c      DTIM=EBPER/DBLE(N2)       ! size of one ctime step, seconds
      ZENA=PIVAL*ABS(ehour/12. -1.) ! zenith angle of eclipse on eq., radian
      QSEP=1.-COS(ZENA)*EBR    ! distance between  OB and  ESP, scaled
      SUNR=QSEP*SRAD      ! apparent radius of sun at  OB from  ESP, scaled

      IF (FRAB.GE.(1.+SUNR/OBR)) THEN
        JBE(1)=INT(CN2+DHALF) ! ctime closest to eclipse
        JBE(2)=JBE(1)-1 ! later is lower, impossible.  To signal no eclipse
        RETURN
      ENDIF

      IF (OBR.GE.SUNR) THEN     ! ensure that  B is the bigger
        B=OBR                   ! radius of bigger object
        R=SUNR                  ! " " of smaller
      ELSE                      !  Sun is bigger, annular eclipse
        B=SUNR
        R=OBR
      ENDIF
      X=SQRT( (B+R)**2-BIAS**2 ) ! projected  X_c dist. at 1st contact
      PHI=ASIN(X/QSEP)           ! half-angle of full eclipse, radian
      HALFT=PHI*DBLE(N2)/(2.*PIVAL) ! ctime-steps for 1/2 full phenomonon

      FE7=CN2-HALFT             ! ctime in steps at start of eclipse
      FE8=CN2+HALFT             ! ctime in steps at  end  of eclipse
C  INT rounds magnitude down
      J7=INT(FE7)               ! index of ctime step containing eclipse start
      J8=INT(FE8)+1             ! index of ctime step containing eclipse end
      JBE(1)=J7                 ! transfer indices to output
      JBE(2)=J8

C---- some meanings change
      Y2=BIAS**2       ! square of closest approach, in
      R2=R**2                    ! 4  terms that appear more than once
      B2=B**2
      BPR=B+R
      BMR=B-R
      IF (OBR.GE.SUNR) THEN     !  Sun is smaller.  Total eclipse
        ASUN=PIVAL*R2           ! area of the  Sun in step^2 units
        FMIN=0.                 ! fraction of sunlight if complete overlap
      ELSE                      !  Sun is larger.  Annular eclipse
        ASUN=PIVAL*B2           ! area of the  Sun in step^2 units
        FMIN=1.D0-R2/B2         ! fraction of sunlight if complete overlap
      ENDIF
      DANG=2.*pival/(dn2*tfac)  ! anomaly change in one time step, radian
 33   FORMAT(A,5G13.5,/,5G13.5)
      IF (KODE .NE. 0) THEN
C        call backtrace    !  GNU compiler fails on this
        WRITE(*,33)'PARR',PARR
        WRITE(*,33)'PERIOD,N2,ZENA,QSEP',PERIOD,N2,ZENA,QSEP
        WRITE(*,33)'LRARE,KFAC,TFAC,FMIN',LRARE,KFAC,TFAC,FMIN
        WRITE(*,33)'SRAD,OBR,RMO,EBPER', SRAD,OBR,RMO,EBPER
        WRITE(*,33)'VEL,BIAS,SUNR',VEL,BIAS,SUNR
        WRITE(*,33)'X,PHI',X,PHI
        WRITE(*,33)'CN2,HALFT,FE7,FE8',CN2,HALFT,FE7,FE8
        WRITE(*,33)'B,R,Y2,',B,R,Y2
        WRITE(*,33)'B2,R2,BPR,BMR',B2,R2,BPR,BMR
        WRITE(*,33)'ASUN,DANG,JBE',ASUN,DANG,JBE
      ENDIF
      IF (LRARE) THEN           ! using fine time steps thru eclipse
        NOUT=(J8+1-J7)*KFAC       ! always corresponds to integral time-steps
        CN2=(CN2-FE7)*TFAC+DHALF ! ftime at eclipse center, relative to j7 time
        J7=1
        J8=NOUT
      ELSE                      ! using  c-time steps all day
        CN2=CN2+DHALF             ! adjustment for itegral truncation later
        NOUT=N2
      ENDIF
      CALL FILLD (1.D0,OUTT,NOUT) ! to handle outside eclipse
      IF (KODE .LT.0) RETURN    ! debug test
      DO J=J7,J8      !  Daily=all,  Rare= only the points in eclipse
        PHI=ABS(DBLE(J)-CN2)*DANG   ! orbital angle from eclipse center. radian
        X=SIN(PHI)*QSEP        ! distance from the eclipse centerline in steps
        D2=X**2+Y2              ! d^2
        D=SQRT(D2)       ! separation of circle centers, in steps
        IF (D.LE.BMR) THEN
          FRAK=FMIN               ! total overlap
        ELSE IF (D.GE.BPR) THEN
          FRAK=1.               ! no overlap
        ELSE
          TWOD = 2.D0*D         ! next: four terms in square root.
          P1=BMR-D              !  -d+r-R =  (B-R)-d
          P2=-BMR-D             !  -d-r+R =  -(B-R) -d
          P3=BPR-D              !  -d+r+R =  (B+R)-d
          P4=BPR+D              !   d+r+r =  (B+R)+D
          PP=P1*P2*P3*P4        ! product of 4 terms in radical

          IF (PP .LT. 0.) THEN
            WRITE(IOERR,*) 'ECLIPSE.F: NEGATIVE SQRT'
            OUTT(1)=-1.D0
            RETURN
          ENDIF

          SQP=SQRT(PP)
          ANG1= ACOS((D2+R2-B2)/(TWOD*R)) ! angles in the overlap diagram
          ANG2= ACOS((D2+B2-R2)/(TWOD*B))
          ALAP= R2*ANG1 +B2*ANG2 -DHALF*SQP ! area of overlap
          FRAK=1.-ALAP/ASUN  ! fraction of  Sun not obscured
          IF (J-J7.EQ.KODE-1) THEN
            WRITE(*,33)'J,PHI,X,D,PP',J,PHI,X,D,PP
            WRITE(*,33)'SQP,ANG1,ANG2,FRAK',SQP,ANG1,ANG2,FRAK
          ENDIF
        ENDIF
        OUTT(J)= FRAK ! proportion of sunlight that reached  target=EB
      ENDDO
      RETURN
      END
