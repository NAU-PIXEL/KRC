        SUBROUTINE TDISK8 (KODE,KREC)
C_Titl  TDISK8  save/read results at the end of a season.  DP version
C_Vars
        INCLUDE 'krcc8m.f'      ! has IMPLICIT NONE
        INCLUDE 'latc8m.f'
        INCLUDE 'dayc8m.f'
        INCLUDE 'hatc8m.f'
        INCLUDE 'units.inc'
        INCLUDE 'filcom.inc'
C_Lims
        INTEGER*4 MAXDIM
        PARAMETER (MAXDIM=6)    ! max number of dimensions needed
        REAL*8 FFF (KOMMON)     !   the largest of the 5n types
        INTEGER*4 MMM(MAXDIM)   ! to hold cumulative size of each dimension
        INTEGER*4 JJJ(10)       ! sizes to go to  BINF5
        REAL*8 FRONT(4)         ! Leading size integers converted to real
        INTEGER*4 KASE,KODED    ! words/case, file type
        COMMON /BINCOM/ JJJ,KODED,KASE,FFF ! ensure these are remembered
C_Args
        INTEGER*4 KODE          ! in. control
C  1 = Open file or restart bin5 accumulation.  Then see  KREC
C        will set ITOP to the file type
C        will set  LOPN2=.true. if direct-access file opened
C  2 = Write a season. appended after  JREC.  KREC ignored
C  3 = Read a season.  input  KREC as record number,  IERR returned as iostat
C       also, sets record position to append on next write
C  4 = Close the file.  KREC ignored
C  5 = If first record, Write KRCCOM to IOD2 .  KREC ignored
        INTEGER*4 KREC  !in (when  KODE=1): file status: 0=new  1=old
C_Desc
C Can write several styles of binary files; controled by K4OUT. See helplist.txt
C The two basic families are Direct access files containing combinations of
C the COMMON arrays, and bin5 files of specific variables and arrays.
C The bin5 files have K4OUT=50+; the parameter statements above set the 
C maximum allowed sizes, but they are written dense to the requested array
C sizes, and BINF5 writes only the utilized part of the storage allocated.
C 2008oct22 All bin5 files have 4 indices and KRCCOM [and other vectors) in the 
C first few latitudes or seasons. The indices are always: NWKRC, 
C     1-based index of the dimension that has the extra leading values, 
C     how many extra are used, and spare
C_Hist ~1975 Hugh Kieffer Original version; evolved over 20 years
C 93mar03 ecisneros ported code to unix platform
C                         converted include filenames to lowercase.
C 97feb11  HK  Get file name only from common
C 97aug03  HK  Add short form output  97sep08 accomodate  TPF,ALAT,ELEV
C 98may26  HK  NRECL in bytes for  Linux
C 98aug31  HK  Include custom  bin5 file capablity 
C                   51=(30 layers, 2 min/max,  5 lat, 40 seasons,  5 cases)
C  The last 2 layers for min are:  TpMin,L_sub_s; for max are:  TpMax,DJU5
C 1998nov16  HK  Add  52=(24 hours,   Tsurf/Tp, 10 lat, 80 seasons, 10 cases)
C 1999nov24  HK  Move setting  LOPN2 false at 400
C 2002aug04  HK  Add 53=(combo  at 1 lat, 2+80 seasons, 10 cases). Recode logic
C 2002aug15  HK  Increase season size
C 2003may25  HK  Increase  MD23 from 10 to 19. Use NDx consistently for  K4OUT=52
C 2003jun10  HK  Fix indexing for seasons for type 53
C 2003aug27  HK  Fix indexing error
C 2004jul06  HK  Add file style 54.
C 2004Oct05  HK  Revise style 52; add Down fluxes and spare
C 2005aug04  HK  Add first version of type 55
C 2005nov16  HK  Set ncase=0 after a file close so next would start at 1
C 2006apr12  HK  Change file style 54 to have both 1am and 1pm surface  Temp..
C 2006apr22  HK  Allow flexible number of cases for  output file type 52 and 54
C 2006may04  HK  Remove the parameter statements for each type. Add  BINCOM
C 2006Oct16  HK  Add two convergence items into output file type 52
C 2008mar31  HK  Ensure all INTEGER are INTEGER before successful 63-bit compile
C 2008apr13  HK  Add spaced TMIN and TMAX to Type 52
C 2008sep30-oct15  HK Add type 56, Revise type 51 and 52, remove type 53
C 2009feb23  HK  Recode all bin5 outputs
C 2010jan12  HK  Use  IMPLICIT  NONE
C 2010apr21  HK  Write notice of writing record only if count <=  IDISK2
C 2012feb26  HK  Remove unused variables  apr04 minor cleanup
C 2013feb19  HK  Include KRC Version number and file type in bin5 header
C 2013aug17  HK  Fix value of MM4 for Type 52
C 2013aug30  HK  Fix logical flaw if changing some file types by use of ITOP 
C 2014mar10  HK Make  REAL*8  version. Delete all Type 5x but 52.  R2R arg3 all *2
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C JJJ are the IDL SIZE sent to BIN5 files;  JJJ[1]= dimensionality
C MMj are the number of words in the j'th dimension
C ie MM1 =JJJ[2], MM2=MM1*JJJ[3] etc   OR MM4 used as saved other values
C Direct write will zero-fill unused part of record
C Make arrays that overlay each of the major   Commons
        REAL*8 COMKRC(NWKRC),COMLAT(NWLAT),COMDAY(NWDAY)
        EQUIVALENCE (COMKRC,ALB),(COMLAT,DTM4),(COMDAY,XCEN) ! first word of each
        REAL*4 RASE
        REAL*4 qd,qe ! TEST ONLY
        INTEGER*4 JREC          ! the 1-based output record number
        INTEGER*4 I,I1,I2,I4,IDX,IOS,IRET,ITOP,J,JOUT,K,NWTOT,NRECL
        INTEGER*4 KHEAD,NDX,ND4,MASE,MM1,MM2,MM3,MM4 ! multiple array dimensions
        INTEGER*4 NSOUT         ! number of seasons expected to be output
        CHARACTER*3 CSTAT       ! file status, and reuse for file type
CD      CHARACTER*8 BUFF        ! conversion from integer to string
        CHARACTER*25 HEADTX     ! will go into bin5 header
        INTEGER*4 HEADLEN /25/
        SAVE IDX,NDX,JREC,MASE,MMM ! insure these 
        SAVE FRONT,NSOUT,ITOP   ! remain defined
C
 31     FORMAT(A,5I7)           !<dbug
        IF (IDB3.GE.3) WRITE(IOSP,*)'TDISKa ',KODE,KREC,NCASE,J5,K4OUT
        IF (IDB3.GE.4) WRITE(IOSP,31)'TDISKa N3,N4+',N3,N4,N5,J5,MASE !<dbug
        IF (IDB3.GE.5) WRITE(*   ,31)'TDISKa N3,N4+',N3,N4,N5,J5,MASE !<dbug
        GOTO (100,200,300,400,500),KODE
C
C request file name and open file                       1  1  1  1  1  1  1  1
C Note. Case and KRCCOM are completely defined at this point
100     IF (LOPN2) CLOSE (IOD2)
        LOPN2=.FALSE.
        IF (KREC.EQ.0) THEN
          CSTAT='NEW'
CC        WRITE (IOPM,*) ' File for saving results?'
        ELSE
          CSTAT='OLD'
          WRITE (IOPM,*) ' File to start (and continue) from ?'
        ENDIF
        ITOP=K4OUT              ! remember what kind of file is open
C     recl may be bytes or longwords, depending upon  OS and compiler options.
C Solaris: is longwords, since file is unformatted (default for direct access)
        IF (K4OUT.LT.50) THEN   ! Open direct-access output file
          IF (K4OUT.LT.0) THEN  !  K4OUT is negative  .tm1
            NWTOT=2*MAXNH*MAXN4 !  TSF  &  TPF only
          ELSEIF (K4OUT.EQ.0) THEN                  ! .t0
            NWTOT=NWKRC+NWLAT   !  KRCCOM & LATCOM
          ELSE                  !  K4OUT is 1:49
            NWTOT=NWKRC+NWDAY   !  KRCCOM & DAYCOM
          ENDIF
          NRECL=8*NWTOT    ! bytes: or  NRECL=NWTOT  ! depends upon compiler <<<<
          OPEN (UNIT=IOD2,FILE=FDISK,ACCESS='DIRECT',STATUS=CSTAT
     &      ,RECL=NRECL,ERR=191,IOSTAT=IOS)
          LOPN2=.TRUE.
          JREC=0                ! no records written yet
          WRITE(IOSP,110)CSTAT,FDISK,ITOP,NWTOT, NRECL,DAYTIM
          WRITE(IOERR,110)CSTAT,FDISK,ITOP,NWTOT, NRECL,DAYTIM
 110      FORMAT (/'0TDISK:  Opened ',A,' direct access file = ',A
     &      /' Type=',I4,' Record length in Real & NRECL=',2I6,2X
     &      ,'NOW=',5A4)
        ELSE                    ! setup for BIN5 file
          JJJ(4) = 0            !
          JJJ(5) = 0            !
          JJJ(6) = 0            !
          JJJ(7) = 0            !
          JJJ(8) = 5            ! set type as  REAL*8
          JJJ(9) = HEADLEN      ! header length
          NSOUT=N5-JDISK+1 ! number of seasons to output
          IF (K4OUT.EQ.52) THEN 
            ND4=N4              ! remember how many latitudes in first case
            JJJ(1) = 5          ! # of dimensions
            JJJ(2) = N24        ! # hours output
            JJJ(3) = 7          ! 3 temperature, 2 fluxes, 2 layers
            JJJ(4) = N4         ! dimen of latitudes
            JJJ(5) = NSOUT      !  number of seasons to come.
            KHEAD   =5*NSOUT    !  Extra words in case header
          ELSE
             WRITE (IOERR,*)' TDISK Invalid K4OUT, fatal: ',K4OUT
             LOPN2=.FALSE.
             GOTO 9
          ENDIF
D         WRITE(IOSP,13)K4OUT,JJJ
D 13      FORMAT ('Initiated custom output: K4OUT=',I3,/'JJJ=',10I6)
C at this point, jbb(2:n) contain the dimension needed for data.
C need to update next to last for NDX, and compute the number of cases possible
          IDX=JJJ(1)-1          ! dimension that has extra size
          WRITE(IOPM,*)'IDX=',IDX
CC        IF (IDX.LT.2) THEN STOP ! ensure enough dimensions for this scheme
          MMM(1)=JJJ(2)         ! compute number of words for each dimension
          DO J=2,IDX            ! up to the largest used
             MMM(J)=MMM(J-1)*JJJ(J+1) !
          ENDDO
          K=4+NWKRC+KHEAD       ! Prefix size (case header) required
          NDX=(K-1)/MMM(IDX-1)+1 ! extra Dim N-1 needed to hold case header
          WRITE(IOPM,*)'N4,NDX,K=',N4,NDX,K
          JJJ(IDX+1)=JJJ(IDX+1)+NDX ! revised this dimension
          WRITE(IOPM,113)IDX,JJJ
 113      FORMAT (' IDX=',I2,'  JJJ=',10I4)
          WRITE(IOPM,*)'MMM=',MMM
          WRITE(IOSP,113)IDX,JJJ
          WRITE(IOSP,*)'MMM=',MMM
          KASE=MMM(IDX-1)*JJJ(IDX+1) ! # words in a case
          RASE=FLOAT(KOMMON)/FLOAT(KASE) ! # cases that could be accomodated
          MASE=IFIX(RASE)       ! # cases that could be accomodated
          KODED=K4OUT           ! transfer into Common
          FRONT(1)=DFLOAT(NWKRC) ! first 4 words are sizes used
          FRONT(2)=DFLOAT(IDX)  ! 1-based index of dimension with extra values
          FRONT(3)=DFLOAT(NDX)  ! Number of those extra
          FRONT(4)=DFLOAT(NSOUT) ! number of output seasons
          print *,'KOMMON,KASE=',KOMMON,KASE
          WRITE(IOSP,*),'KOMMON,KASE=',KOMMON,KASE
          WRITE(IOSP,*),'RASE,MASE,MTOT=',RASE,MASE,KASE*MASE
          JREC=0                ! records written thus far = none
          LOPN2=.TRUE.
          IF (MASE.LT.1) LOPN2=.FALSE. ! KASE larger than KOMMON
        ENDIF
        GOTO 9
C
191     WRITE (IOERR,*) ' TDISK:1, ERROR OPENING FILE. IOSTAT=',IOS
        WRITE (IOERR,*) '  IOD2=',IOD2,'  status=',cstat,'  recl=',nrecl
        WRITE (IOERR,*) '  file=  ',FDISK
        LOPN2=.FALSE.
        GOTO 9
C
C write next record (internal record count)             2  2  2  2  2  2  2  2
 200    IF (ITOP.LT.50) THEN !---------------------------------------------..
          IF (LOPN2) THEN       ! save current values
            JREC=JREC+1
            I=JREC              ! may be incremented by the  WRITE command
            IF (ITOP.LT.0) THEN ! K4OUT negative  .tm1
              WRITE(IOD2,REC=I)TSF,TPF
            ELSEIF (ITOP.EQ.0) THEN      ! .tm0
              WRITE(IOD2,REC=I)COMKRC,COMLAT
            ELSE                ! K4OUT=1:49
              WRITE(IOD2,REC=I)COMKRC,COMDAY
            ENDIF
            I=LNBLNK(FDISK)     ! last non-blank character in file name
            IF (JREC.LT.IDISK2) WRITE(IOSP,210)J5,JREC,SUBS,FDISK(1:I)
 210        FORMAT(' TDISK wrote: J5 rec Ls File ',2I4,F7.2,1X,A)
          ELSE
            WRITE (IOERR,*) ' TDISK:2, WRITE, BUT NO FILE OPEN'
          ENDIF 
          GOTO 9
        ENDIF

C get here if type 5x  !---------------------------------------
D         WRITE(IOSP,*)'TDISKb ',kode,krec,ncase,j5,' jd,jjj',jdisk,JJJ
        IF (NCASE.GT.MASE) GOTO 9 ! no room left for storage
        JOUT=J5-JDISK           ! 0-based season count after start to disk
        if (jout.lt.0) then ! should not happen
          print *,'JOUT NEG',jout,j5,jdisk,ncase
          goto 9
        endif
        JREC=JOUT+1             ! 1-based count of this record
        IF (JREC.GT.NSOUT) GOTO 9 ! beyond allocated season range
        I1=(NCASE-1)*KASE+1     ! first word for this case
        IF (JOUT.EQ.0) then     ! first season of this case
           CALL R2R (0.,FFF(I1),-KASE*2) ! zero the case
           CALL R2R (FRONT,FFF(I1),4*2) ! first 4 words are sizes used
           CALL R2R (ALB,FFF(I1+4),NWKRC*2) ! followed by KRCCOM
        ENDIF
        I4=I1+4+NWKRC           ! first word in prefix after KRCCOM
        MM1=MMM(1)              ! may be needed several times in 5x sections
        MM2=MMM(2)
        MM3=MMM(3)
        MM4=MMM(4)
        IF (ITOP.EQ.52) THEN !------------------------------------
C For hourly conditions:  revised 2004jul22  and 2004Oct06 and 2008oct15
C [N24,7,Nlat,x+nseas,ncase]   First x "seasons" of each case contains: 
C   Float of Front+KRCCOM, DJU5(nseas),SUBS(nseas)
C   ,PZREF(nseas), TAUD(nseas), SUMF(nseas)
C True seasons contain for every hour: TSF,TPF,TAF,DOWNVIS,DOWNIR
C  and float(NDJ4)+ DTM4 + TTA4+ Tmin(Nlay-) Omitting virtual first layer
C  and    FROST4+ AFRO4+ HEATMM+ Tmax(Nlay-) " " "
C the Nlay- are as many layers as fit within the number of Hours
           MM4 = MIN(N1-1,MM1-3) ! compute # layers to store
           IF (JOUT.EQ.0) THEN  ! insurance done only once per case 
             J=N5-JDISK+1+NDX   ! season dimension required 
             IF (N24.GT.JJJ(2) .OR. N4.GT.JJJ(4) .OR. J.NE.JJJ(5)) THEN
               WRITE (IOERR,*)
     &   'Invalid Change of N24, NLAT, or #to Disk for K4OUT=52'
               WRITE (IOERR,*)N24,MM1, N4,JJJ(4) ,J,JJJ(5)
            WRITE (IOSP,*) 'Bad size change, CLOSE file. See Error File'
               GOTO 400         ! Force early closing of file
             ENDIF
           ENDIF
           J=I4+JOUT            ! loc of this seasons DJU5
           if (j.lt.1) then !<<<<<<<<<<< debug test
             print *, 'tdisk: J lt 1',I4,Jout,j,jdisk,j5
           J4=I1+MM3*(NDX+JOUT) ! first word of this case, season
             print *, 'i1,mm3,ndx, j_soon=',i1,mm3,ndx, j4
             qe =-1.
             qd=alog(qe) ! should force math error
           endif

           FFF(J) =DJU5         ! date for this case and season
           FFF(J+NSOUT) =SUBS
C          write(*,*)'t1', subs
           FFF(J+NSOUT+NSOUT) =PZREF
           FFF(J+3*NSOUT) =TAUD
           FFF(J+4*NSOUT) =SUMF
           J=I1+MM3*(NDX+JOUT)  ! first word of this case, season
           DO J4=1,N4           ! do each latitude
             I=J+(J4-1)*MM2     ! first word of this case, season, latitude
             CALL R2R (TSF(1,J4),FFF(I)          ,MM1*2) ! copy one day
             CALL R2R (TPF(1,J4),FFF(I+MM1)      ,MM1*2)
             CALL R2R (TAF(1,J4),FFF(I+MM1+MM1)  ,MM1*2)
             CALL R2R (DOWNVIS(1,J4),FFF(I+3*MM1),MM1*2)
             CALL R2R (DOWNIR (1,J4),FFF(I+4*MM1),MM1*2) ! item 5
             K=I+5*MM1          ! first index of item 6
             FFF(K)=DFLOAT(NDJ4(J4)) ! # days to compute solution 
             FFF(K+1)=DTM4(J4)  ! rms temperature change on last day
             FFF(K+2)=TTA4(J4) 
             CALL R2R (TIN(2,J4),FFF(K+3),MM4*2)
             K=K+MM1            ! first index of item 7
             FFF(K)=FROST4(J4)  ! Frost at midnight
             FFF(K+1)=AFRO4(J4) ! frost albedo
             FFF(K+2)=HEATMM(J4)  
             CALL R2R (TAX(2,J4),FFF(K+3),MM4*2)
           ENDDO
D          WRITE(IOSP,*)'JOUT,J,I,K',JOUT,J,I,K
        ENDIF                   !---------------------------------------------
        GOTO 9
C
C read requested record (external record control)       3  3  3  3  3  3  3  3
300     IF (LOPN2) THEN
                I=KREC
                READ(IOD2,REC=I,IOSTAT=IOS)COMKRC,COMLAT
                WRITE(IOSP,310)FDISK,KREC
310             FORMAT(' TDISK:  READ FILE= ',A,' RECORD=',I3)
                IERR=IOS
                JREC=KREC
            ELSE
                WRITE (IOERR,*)' TDISK:3, READ, BUT NO FILE OPEN'
            ENDIF
        GOTO 9
C
C close the file                                        4  4  4  4  4  4  4  4
 400    IF (LOPN2) then
          IF (ITOP.GE.50) THEN ! save custom array
CD           print *,'TDISK: K4OUT=',k4out
            WRITE(CSTAT,403)ITOP ! convert file type integer to string
 403        FORMAT(I3)
CD          print *,'CSTAT>',cstat,'<'
            HEADTX=VERSIN//' file type'//CSTAT ! 12+10+3
CD          print *,'headtx=',headtx
            JJJ(IDX+2) = NCASE
            WRITE (IOSP,*)'JJJ=',JJJ
            IRET=IDB3 !  +1=report many values   +2 report progress 
            CALL BINF5 ('W',FDISK,HEADTX,JJJ,FFF,IRET) ! others equivalenced
            WRITE (IOSP,*)'Wrote bin5 file: type and iret= ',ITOP,IRET
            WRITE (IOSP,*)'  File name=',FDISK
          ELSE                  ! close direct access file, or message
            CLOSE (IOD2)
            WRITE (IOSP,*)'Closed direct access file. Type=',ITOP
          ENDIF
          LOPN2=.FALSE.
        ELSE ! not open
          IF (K4OUT.LT.50) WRITE (IOERR,*)' TDISK:4, no file was open'
        ENDIF
        NCASE=0                 ! ensure next file indexing start at 1 
        GOTO 9
C       
C       output parameters once                          5  5  5  5  5  5  5  5
 500    IF (ITOP.LT.0 .AND. JREC.EQ.0) THEN
           JREC=1
           I=JREC               ! need room for NWKRC items
           IF (IDB3.GE.3) WRITE(IOSP,*)'TDISKc KREC=',KREC,LOPN2,IOD2,I
           WRITE (IOD2,REC=I) COMKRC
        ELSE 
        WRITE (IOERR,*) 'TDISK:5,wrong conditions: ITOP,jrec=',ITOP,JREC
        ENDIF
        
 9      CONTINUE
C       print *,'exit TDISK8  KREC=',KREC
        IF (IDB3.GE.7) WRITE(IOSP,*)'TDISKx  KREC=',KREC
        RETURN
        END
