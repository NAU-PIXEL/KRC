      SUBROUTINE TPRINT (IQIN) 
C_Titl  TPRINT printed output routine
C_Vars
        INCLUDE 'krccom.inc'
        INCLUDE 'latcom.inc'
        INCLUDE 'daycom.inc'
        INCLUDE 'units.inc'
        INCLUDE 'filcom.inc'

C_Args
        INTEGER IQIN !in. what to print:  1=program description
C  2=all parameters  3=latitude page titles  4=convergence summary {depth table}
C  5=latitude summary  6=layer min and max temperatures  
C  7=blowup conditions  8=page heading  9=one line for "one=point' mode

C_Hist	87sep21  HHK  CHANGE format 142 to  G, and narrow depth table
C	93mar03  ECisneros ported to unix 
C 97feb13  HHK major revision
C 97aug22  HHK correct printed scales to  MKS
C 2002mar07 HHK Add one line output for "one-point" mode
C 2002aug04 HHK Fix errors in interpolation for one-point temperatures
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
        DATA IPG /0/
C
	IQ = IQIN
        IF (IPG.EQ.0) CALL DATIME (DAYTIM)
        GO TO (800,800,800,230,800,800,480,800,900,99), IQ
C
C  PRINT program description  (IQ = 1)
C
100     OPEN (IOD1, FILE='KRCEXP.TXT',STATUS='OLD')
        DO 110 I=1,60		!  SCONVG is used here as a scratch array
                READ  (IOD1,'(30A4)',END=128,ERR=110) SCONVG
110             WRITE (IOSP,'(1X,30A4)') SCONVG
128     CLOSE (IOD1)
        RETURN
C
C  PRINT all parameters  (IQ = 2)
C
135	WRITE (IOSP,136) FINPUT,FOUT,FDISK
136	FORMAT ('0Input file: ',A/' Print file: ',A/' Save file: ',A)

	WRITE (IOSP,142) (FD(I),I=1,NFD)
142     FORMAT ( '0    ALBEDO     EMISS   INERTIA     COND2     DENS2'
     &, '    PERIOD SPEC_HEAT   DENSITY'/1X,8F10.4
     &/'0      CABR    ABRAMP    ABRPHA    PTOTAL     FANON      TATM'
     &, '     TDEEP   SpHeat2'/1X,8F10.4
     &/'0      TAUD     DUSTA    TAURAT     TWILI      ARC2      ARC3'
     &, '     SLOPE    SLOAZI'/1X,3F10.4,3F10.2,2F10.1 
     &/'0    TFROST    CFROST    AFROST     FEMIS       AF1       AF2'
     &, '    FROEXT      ARC6'/1X,F10.4,F10.1,2F10.4,F10.5,3F10.4
     &/'0      RLAY      FLAY     CONVF     DEPTH     DRSET       DDT'
     &, '       GGT     FDOWN'/1X,8F10.4
     &/'0      DJUL    DELJUL SOLAR DEC       DAU       L_S    SOLCON'
     7, '      GRAV    Atm_Cp'
     &/1X,F10.3,2F10.4,F10.5,4F10.4)

        WRITE (IOSP,144) (ID(I),I=1,NID)
144     FORMAT ( '0        N1        N2        N3        N4        N5'
     &, '       N24        IB        IC'/1X,8I10
     &/'0     NRSET      NMHA      NRUN     JDISK     IDOWN       I14'
     &, '       I15     KPREF'/1X,8I10
     &/'0     K4OUT     JBARE     spare     spare',/1X,4I10)

        WRITE (IOSP,146) (LD(I),I=1,NLD)
146     FORMAT('0    LP1    LP2    LP3    LP4    LP5'
     &,         '    LP6 LPGLOB   LVFA   LVFT   LD10'/1X,10L7
     &/        '0  LPORB   LKEY    LSC LNOTIF  LOCAL'
     &,         '   LD16 LPTAVE Prt.78 Prt.79  L_ONE'/1X,10L7)

        WRITE (IOSP,143) 'Latitudes: ',(ALAT(I),I=1,N4)
        WRITE (IOSP,143) 'Elevations:',(ELEV(I),I=1,N4)
 143    FORMAT (/1X,A/(1X,10F7.2))
C
C print layer depth table
C
	IQ = 4	! temporary, to return from header line
	GOTO 800
150     Q2=DENS*SPHT
        WRITE(IOSP,160) COND,Q2,DIFFU,SCALE
160     FORMAT (' Conductiv.=',1PE10.3,'  Dens*Cp=',E10.3,'  Diffu.='
     &,E10.3,'  Scale=',E10.3)
        IF (IC.GT.2 .AND. IC.LT.N1-1) then
          Q2=DENS2*SPHT2
          Q4=SQRT(Q2*COND2)      ! inertia
          WRITE(IOSP,162),IC,Q4
 162      FORMAT(' Beginning at layer ',I3,'   Inertia=',F8.1)
          Q4=COND2/Q2 ! diffusivity
          Q6=SQRT(Q4*PERIOD*86400./PI)
          WRITE(IOSP,160) COND2,Q2,Q4,Q6
        ENDIF
        WRITE(IOSP,165)
 165    FORMAT(
     &'0        ___THICKNESS____   _______CENTER_DEPTH_____ CONVERGENCE'
     &/' LAYER     scale    meter     scale    meter   kg/m^2   factor')
C       123456789 123456789 123456789 123456789 123456789 123456789 123456789
C2345 789 123456789 123456789 123456789 123456789 123456789 123456789 72________
        q6=0.
        DO 170 I=1,N1
                q2 = TLAY(I)/SCALE ! thickness in units of surface scale
                q4 = x(i)/scale    ! center-depth in units of scale
                if (i.gt.1 .and. i.lt.ic) q6=q6+tlay(i)*dens
                if (i.ge.ic) q6=q6+tlay(i)*dens2 ! layer-center columnar mass
170             WRITE(IOSP,180) I,q2,TLAY(I),q4,X(I),q6,SCONVG(I)
180     FORMAT (I5,F10.4,F10.4,F10.4,f10.4,F10.3,F8.3)
        WRITE(IOSP,190) (N1K(K),K=1,KKK)
190     FORMAT ('0Bottom layers for time doubling:  ',10I5)
        RETURN
C
C latitude page titles  (IQ = 3)
C
200     WRITE(IOSP,210) DLAT,SDEC,DAM,TEQUIL
210     FORMAT ('0  LAT=',F6.1,'  SOLAR DEC=',F6.2,'   HALF DAY=',F6.2
     &,' DEGREES   EQUILIBRIUM TEMP=',F7.2/)
        WRITE(IOSP,220) (X(I),I=2,N1M1,NLW),X(N1)
220     FORMAT ('    HOUR  FROST  X = 0.',(11F8.3))
        RETURN
C
C print daily convergence summary  (IQ = 4)
C
230   WRITE(IOSP,240)TTS4(J4),TTB4(J4),DTM4(J4),J3,FROST4(J4)
240     FORMAT ('0SURF AVE =',F6.1,'  BOTM AVE =',F6.1
     &,'  DELTA T =',F7.4,'  NUM DAYS =',I3,'  FROST =',F9.4)
        WRITE(IOSP,250) (I,I=2,N1M1,NLW),N1
250     FORMAT (' AT START OF DAY'
     &/' DAY  DTMJ SUR_AVE BOT_AVE     1',13I8)
        J3P1=J3+1
        DO 260 J=1,J3P1
260     WRITE(IOSP,270) J,DTMJ(J),TTS(J),TTB(J),TT(1,J)
     &,(TT(I,J),I=2,N1M1,NLW), TT(N1,J)
270     FORMAT (1X,I3,F6.2,(14F8.1))
        RETURN
C
C print latitude summary  (IQ = 5)
C
280   WRITE(IOSP,'(A)') '0SURFACE TEMP VS LATITUDE AND LOCAL HOUR ANGLE'
        WRITE(IOSP,'(A,20F6.1)') ' LATITUDE =',(ALAT(I),I=1,N4)
        DO 330 I=1,N24
                IF (MOD(I,6).EQ.1) WRITE(IOSP,'(1X)')
330             WRITE(IOSP,'(5X,I4,A,20F6.1)') I,'HR',(TSF(I,J),J=1,N4)
        WRITE(IOSP,'(A,20F6.1)') '0SURF AVE =',(TTS4(I),I=1,N4)
        WRITE(IOSP,'(A,20F6.1)') ' BOTM AVE =',(TTB4(I),I=1,N4)
        WRITE(IOSP,'(A,20F6.1)') ' GM FROST =',(FROST4(I),I=1,N4)
        WRITE(IOSP,'(A,20F6.3)') ' ALB/dayF =',(AFRO4(I),I=1,N4)
        WRITE(IOSP,'(A,20F6.1)') ' T. EQUIL =',(TST4(I),I=1,N4)
        WRITE(IOSP,'(A,20F6.2)') ' DELTA T. =',(DTM4(I),I=1,N4)
        WRITE(IOSP,'(A,20I6  )') ' NUM DAYS =',(NDJ4(I),I=1,N4)
        WRITE(IOSP,'(A,20F6.2)') ' TS EPRED =',(TAX(MACN1,I),I=1,N4)
        WRITE(IOSP,'(A,20F6.2)') ' TB EPRED =',(TIN(MACN1,I),I=1,N4)

        RETURN
C
C print max and min layer temperature summaries  (IQ = 6)
C
410     WRITE(IOSP,'(A)') '0MAXIMUM TEMPERATURE'
        WRITE(IOSP,'(A,19F6.1)') ' LAYER LAT',(ALAT(I),I=1,N4)
        DO 440 J=1,N1
440             WRITE(IOSP,'(1X,I5,4X,20F6.1)') J,(TAX(J,I),I=1,N4)
        WRITE(IOSP,'(A)') '0MINIMUM TEMPERATURE'
        WRITE(IOSP,'(A,19F6.1)') ' LAYER LAT', (ALAT(I),I=1,N4)
        DO 470 J=1,N1
470             WRITE(IOSP,'(1X,I5,4X,20F6.1)') J,(TIN(J,I),I=1,N4)
        RETURN
C
C print blowup  (IQ = 7)
C
480     WRITE(IOSP,490) J2,J3,J4,J5
490     FORMAT ('0 TEMP BLOWING UP. J2,J3,J4,J5=',4I10/)
        WRITE(IOSP,'(1X,10E12.4)') (T(I),I=1,N1PIB)
        RETURN
C
C print page heading  (IQ =8)  & for 2,3,5,6
C
800     IPG=IPG+1
        WRITE(IOSP,810) TITLE,NRUN,NCASE,DAYTIM,IPG
810     FORMAT ('1',20A4,' RUN-CASE',I4,'-',I2
     &,3X,5A4,'  PAGE=',I3)
        GOTO (100,135,850,150,850,850,99,99,99) ,IQ
C
C print model constants for  IQ = 3,5,6
C
850     WRITE(IOSP,860) ALB,EMIS,SKRC,SDEC,DAU,SUBS,DJU5,J5
     &,FD(I14),FD(I15)
860     FORMAT ('     ALBEDO     EMISS   INERTIA SOLAR DEC'
     &,'       DAU       L_S JULIAN DAY    J5   two from KRCCOM'
     &/1X,3F10.4,F10.2, F10.5,2F10.2,I7,2X,2G10.4)
        GO TO (99,99,200,99,280,410,99,99,99) ,IQ

C
C Print one line of "one-point" results
C
C Model should have been run for the precise season and latitude of request.
C Need to interpolate temperatures to the "hour" requested.
 900    q2=houro*Real(n2)/24.   ! real index of requested time in time steps
        i=q2                    ! integer index of prior point
        j=i+1                   ! index of next point
        if (i.lt.1) i=n2        ! wrap around midnight
        if (j.gt.n2)j=1         ! wrap around midnight
        touto=tout(i)+(q2-real(int(q2)))*(tout(j)-tout(i)) ! linear interp
C planetary temperature available only on the hour, so interpolate Tp-Ts
C  and add this to Ts at the exact requested time
        q2=houro*Real(n24)/24.  ! real index of requested time in "hours"
        i=q2                    ! integer index of prior point
        j=i+1                   ! index of next point
        if (i.lt.1) i=n24
        if (j.gt.n24)j=1
        q4=(tpfh(i)-tsfh(i))    ! planetary-kinetic
        q4=touto+ q4+(q2-real(int(q2)))*((tpfh(j)-tsfh(j))-q4)
        write(iosp,903) SUBS,ALAT(1),HOURO,ELEV(1) 
     &       ,ALB,SKRC,TAUD,SLOPE,SLOAZI,touto,q4
 903   format (' 11',2(F6.1),F6.2,F5.1,F5.2,F7.1,F5.2,F5.1,F5.0,2f11.5)

99      RETURN
C
        END
