      SUBROUTINE PORBIT (KODE, TMJD,LSUBS, SDEC,DAU )
C_TITLE	 PORBIT  Converts between date and Ls. Also returns DAU and Sdec
C_VARS
      INCLUDE 'porbcm.inc'      ! has IMPLICIT NONE
C_ARGS	
      INTEGER KODE  ! in.  1= T to Ls.  2= Ls to T
      REAL*4 TMJD   ! in/out. request time in days relative to J2000.0		
      REAL*4 LSUBS  ! out/in. planetocentric longitude of the Sun, degree	
      REAL*4 SDEC   ! out. Sub-solar latitude, degree
      REAL*4 DAU    ! out. Heliocentric distance, A.U.
C_DESCR	conic orbit; uses solution to Keplers equation
C Uses rotation matric from orbital to seasons, from common
C Responds to  PBUG  in  PORBCM: bit-encoded
C +1 Show Print output
C_CALLS  ORBIT  ROTVEC  VNEG  COCOCS
C_HIST	circa 1970  Hugh Kieffer ORIGINAL VERSION
C    delete intermediate history
C 2013jul24 HK Revise from PORB to KRC version 2 system
C_END
C
      INTEGER I,K
      REAL*4 T,P,Q,R,TAR           ! scalars
      REAL*4 ANOM,EA,CTA,STA         ! scalars for inverse
      REAL*4 HPFXX(3)           ! vectors

      IF (KODE.EQ.1) THEN        ! T to Ls -----------------------

         T = TMJD-TJP           ! time from periapsis
         CALL ORBIT (SJA,OPERIOD,ECC,T, R,PHFXX) ! get vector from sun to planet
         CALL VNEG (PHFXX, HPFXX) ! get to Sun from planet
         TAR=ATAN2(HPFXX(2),HPFXX(1)) ! true anomaly of Sun, radians
         LSUBS=(TAR-TAV)*R2D      ! planetocentric system longitude of the Sun
         IF (LSUBS.LT.0.) LSUBS=LSUBS+360. ! put on 0:360 range
         IF (LSUBS.GT.360.) LSUBS=LSUBS-360. ! put on 0:360 range

      ELSE                      !  Ls to T  -----------------------

         TAR= (LSUBS-180.)/R2D +TAV    ! true anomaly of Planet; radians
         STA=SIN(TAR)
         CTA=COS(TAR)
         P= sqrt(1.-ECC**2)*STA ! sin E * 1+ecosE
         R= ECC+CTA             ! cos E * 1+ecosE
         EA=ATAN2(P,R)          ! Eccentric anomaly
         ANOM=EA-ECC* SIN(EA)   ! Keplers equation to get mean anomaly
         T=OPERIOD*(R2D*ANOM/360.) ! days from periapsis
         TMJD=T+TJP
         R=SJA*(1.-ECC**2)/(1.+ECC * CTA) ! radial distance
         HPFXX(1)= -R*CTA       ! X part of neg of PHfxx
         HPFXX(2)= -R*STA       ! Y part of neg of PHfxx
         HPFXX(3)= 0.           ! Z part

      ENDIF                     ! -----------------------
 
      CALL ROTVEC (BFRM,HPFXX, HPBXX) ! rotate into Body vernal equinox system
      CALL COCOCS(HPBXX, P,Q,DAU) ! spherical angles in radians. Get DAU
      SDEC=90.-R2D*P            ! sub-solar latitude in degree

C Debug section
      K=IFIX(PBUG)
      I=MOD(K,2)                ! +1
      IF (I.EQ.1) THEN
         print *,'KODE,TMJD,LSUBS=',KODE,TMJD,LSUBS
         print *,'TAR,P,Q,R=',TAR,P,Q,R
         print *,'SDEC,DAU=',SDEC,DAU
      ENDIF

      RETURN
      END
