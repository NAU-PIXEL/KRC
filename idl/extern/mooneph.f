      SUBROUTINE MOONEPH (et, mexx,smxx,zm)
C_Titl  MOONEPH Moon ephemeris using JPL Planetary and Lunar Ephemerides
      implicit none
C_Vars
      INCLUDE 'stacom.inc'      ! /STCOMX/ KM,BARY,PVSUN
      INCLUDE 'ephcom.inc'      ! /EPHHDR/ CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
C_Args
      DOUBLE PRECISION ET       ! In.  Ephemeris time
      DOUBLE PRECISION mexx(3)  ! out.  XYZ vector from earth center to Moon,km
C mexx(1) = 1.1D6 (physically impossible) indicates et out-of-range 
      DOUBLE PRECISION smxx(3)  ! out.  XYZ vector from Moon to Sun, AU
      DOUBLE PRECISION zm(6)    ! out.  Moon libration items ???
C                              "(Euler angles and rates, w.r.t. the ephemeris
C                                   reference frame)   [rads, rads/day] 
C_Desc
C JPL code does a Fortran  STOP within STATE.f if requested time is outside
C the range of currently-loaded tables. This causes IDL to exit.
C To avoid that, test also done here, with return of impossible mexx value.
C_Hist 2000dec07 Hugh Kieffer
C 2002feb21 HK  Add test of ET being in range of tables.
C 2007jan25 HK  Correct documentation, had said smxx was in km
C 2009jun21 HK Fix extra dimesion of 1 in referenced to pvsun
C Currently, loaded tables cover 2436912.50:2458864.50 or 1959dec10 to 2020jan16
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890


      DOUBLE PRECISION  esxx(3) !  XYZ vector to Earth from Sun
C used in call to  CONST 
      CHARACTER*6  NAMS(400)    ! ephemeris constant names
      DOUBLE PRECISION VALS(400) ! ephemeris constant values
      INTEGER  NVS              !  Number of ephemeris constants defined

C Target codes in STATE
      INTEGER moon /10/         ! GEOCENTRIC  MOON 
      INTEGER earth /3/         ! EARTH-MOON BARYCENTER
      INTEGER lib /12/          ! LUNAR LIBRATIONS (IF ON FILE)
       
      INTEGER  List(12)
C     data list /0,0,  1, 0,0,0,0,0,0,  1, 0, 2/ ! desired bodies
C     E/M bary         Moon  Lib
       
       
      INTEGER  I                ! local variables
      LOGICAL FIRST
      DATA FIRST/.TRUE./        ! has not been called yet

      DOUBLE PRECISION ET2(2)   ! time to state
      DOUBLE PRECISION PV(6,12) ! return from state
      DOUBLE PRECISION PNUT(4)  ! used only as dummy
 
      save first

      ET2(1)=ET                 ! construct 2 element time for  STATE
      ET2(2)=0.D0
      IF (FIRST .or. et.eq. 0. ) then
        CALL STATE(et2,list,pv,pnut) ! 
c need to initialize some items in commons
        CALL  Const (NAMS, VALS, SS, NVS) ! Get the ephemeris constants.
        FIRST=.FALSE.
      else IF (ET2(1).LT.SS(1) .OR. ET2(1).GT.SS(2)) then   
        WRITE(*,198)ET2(1),SS(1),SS(2)
 198    format(' ***  Requested ET,',f12.2,
     *       ' not within ephemeris limits,',2f12.2,'  ***')
        mexx(1)= 1.1D6          ! impossible value

      Else

      list(moon)=1              ! want only positions
      list(earth)=1             ! want only positions
      list(lib)=2               ! want ??? 
      bary=.true.               ! FORCE BARYCENTRIC OUTPUT BY  'STATE' (native)
      km=.true.                 ! coordinates in km (native to files)
      CALL STATE(ET2,LIST,PV,pnut) ! get the positions
D      write(*,*) 'emrat,au=',emrat,au
D      write(*,*)'pvsun=',pvsun
D      write(*,*)'pv='
D      write(*,301) pv
D 301  format(6g15.5)
D      write(*,*)'pnut=',pnut

      DO I=1,3                  ! get output coordinates
        mexx(i)=PV(I,moon)       ! coordinates of  Moon relative to  Earth
c                  earth   sun      barycenter correction 
        esxx(i)=PV(I,earth)-pvsun(i)-mexx(I)/(1.D0+EMRAT)!  Earth <-  Sun
        smxx(i)=-(esxx(i)+mexx(i))/AU ! To  Sun from  Moon, in  AU
        zm(i)=pv(i,11)          !  Lunar librations
      ENDDO
D      write(*,*)'mexx=',mexx
D      write(*,*)'esxx=',esxx
D      write(*,*)'smxx=',smxx
D      write(*,*)'zm=',zm

      DO I=4,6 ! rest of librations
        zm(i)=pv(i,11)          !  Lunar librations
      ENDDO

      end if
      return
      END
