	SUBROUTINE BINF5 (ACTION,FILENAME, HEADER,ID,DDATA, RET)
C_titl  BINF5:  read/write binary file with IDL-style header
	IMPLICIT NONE
C****************************************************************************
C_Args  Type          name          i/o  Description
        CHARACTER*1   ACTION       ! i   action -  R=read or  W=write
C				   ! can be either uppercase or lowercase
	CHARACTER*(*) FILENAME     ! i   input/output filename
	CHARACTER*(*) HEADER       ! b   header text data
	INTEGER*4 ID(10)	   ! b   array type and dimension
C (1) = number of dimensions
C (2:7) = dimensions 1:6
C (8) = array type:  1=byte  2=I*2  3=I*4  4=R*4  5=R*8
C (9) = header text length (in. Actual for  W, max for  R)
C (10) = total header size in bytes (multiples of 512). set here
	BYTE DDATA(*)	           ! b   data array;
C
        INTEGER*4     RET	   ! o   return code
C                0 = normal return
C               -1 = action invalid
C               -2 = error opening filename
C               -3 = error reading or writing header
C               -4 = error reading or writing data
C               -5 = error closing filename
C               -6 =  ID was invalid
C	        -7 = cpu architecture not recognized
C	        -8 = file architecture unknown
C	        -9 = word type in file unknown
C              -10 = File size integers invalid
C               >0 = file was this much larger than room provided by caller
C_Descr
C  Does all file operations during a single call;
C   open, read or write, close
C  WARNING: will overwrite existing file
C  Note: the  ID values must be set for each  W call;
C   this routine does not remember array sizes.
C  For the read option, the dimensions will be considered for the maximum
C   room allowed for the array to be returned.  RET >0 indicates how much
C   bigger was the array on file.

C  This routine meant to be usable on a number of platforms, and to detect if
C byte swap is needed after reading a file.
C  The initial integer items in the file header are meant to be compatible
C with the idl routines BIN5W & BIN5R.
C  Does not include the file creation data in the returned header
C  First block of header can hold 455 characters of user text.
C  Returned header may have up to 501 trailing blanks
C
C  !ISIS  indicate ISIS versions of code,  versus  !HK
C_Lim
	INTEGER*4  MAXT,MAXD,NBH,MAXARC
	PARAMETER (MAXT=5) ! number of defined types
	PARAMETER (MAXARC=6) ! number of machine architectures
	PARAMETER (MAXD=6) ! max number of dimensions
	PARAMETER (NBH=512) ! total length of header buffer
C   on read, if  Header is too long, last part will be trimmed.
C_Bugs
C this version does byte-swapping after a read, but assumes  R*4 word type
C_Call  PIO_ system  IDARCH  WHITE1  CATIME  B2B
C_Hist	98may12  HHKieffer  Adopted from  binfil.f
C 98sep07  HHK replace  B2B use for character varable
C 98dec29  HHK disable byteswap on read ???
C 99nov05  HHK allow longer headers
C  and include as ckb real differences from the version at
C    /home/kbecker/isis/apps/rolo2/libs/librolo/binf5.f
C2004jul12 HK add arch-type for Mac.
C 2008mar31 HK Replace  SWAPBYTER4 with SWAPBYTE4
C           and  Replace C## with D for conditional compile
C           and temporary firmcode machine architecture   << BEWARE
C_End

	INTEGER*4 IERR		! return code from pio routines
        INTEGER*4 NBD
	INTEGER*4 ND1		! first byte after header
        INTEGER*4 NBYTES

	BYTE HH(NBH)
	CHARACTER*512 CC	!  MUST be same size as  HH
	EQUIVALENCE (CC,HH)	! align character and byte buffers

	INTEGER*4 FID,I,IARCH,J,JCPU,K,KBLOCK,LL,LOC,NBY,NBYF,NIT,PLAN
	INTEGER*4 I1 /1/

	INTEGER*4 NBYPW(MAXT) /1,2,4,4,8/
	INTEGER*4 IDL(10)		! array to contain idl size values
	CHARACTER*120 BUF
	CHARACTER*6 ARCH(MAXARC) /'sparc ','alpha ','ppc   ','x86   '
      1 ,'x86_64','unknow'/		!hardware architecture
C  IDARCH index =                   1      2       3=mac
	INTEGER*4 BARCH(MAXARC) / 1,2,2 ,2 ,2,0 / ! byte-order type (arbitrary)
	CHARACTER*6 ARCHCPU

	INTEGER*4  BT2,BA1,BA2,BC1 ! fixed byte locations
	PARAMETER (BC1=NBH-4)	! first of  C_END
	PARAMETER (BA2=BC1-1)	! last byte of architecture
	PARAMETER (BA1=BC1-8)	! first " " 2008 compatibility with bin5.pro
	PARAMETER (BT2=BA1-1)	! last room for text
	INTEGER*4  WHITE1,IDARCH		! functions
C****************************************************************************

	RET = 0

	IF (ID(8).LT.1 .OR. ID(8).GT.MAXT) GOTO 86 ! invalid type
	IF (ID(1).LT.1 .OR. ID(1).GT.MAXD) GOTO 86 ! invalid # dimensions
	NIT=1			! # items
	DO I=1,ID(1)		! compute total array size in bytes
	  J=ID(I+1)
	  IF (J.LT.1) GOTO 86	! invalid size
	  NIT=NIT*J
	ENDDO
	NBY=NIT*NBYPW(ID(8))	! caller array bytes total
	NBYTES = NBH+NBY        ! total # bytes in file

	IARCH=IDARCH(BUF)	! get current machine type as integer and string
	ARCHCPU=BUF(1:6)
C-	ARCHCPU=ARCH(5)    ! BEWARE  BEWARE BEWARE BEWARE BEWARE
	JCPU=0
	DO I=1,MAXARC
	  IF (ARCHCPU.EQ.ARCH(I)) JCPU=BARCH(I) ! save archit. type
	ENDDO
        WRITE (*,*) 'BINF5: ARCH S= ',IARCH,' >', ARCHCPU,'<',jcpu
	WRITE (*,*) 'BINF5 called with ID= ',ID,'   2008apr01'
	IF (JCPU.EQ.0) GOTO 87

	IF (ACTION .EQ. 'W' .OR. ACTION .EQ. 'w') THEN !=======  WRITE ========

	   CALL PIO_IN (FID, FILENAME, NBYTES, 3, IERR) !  OPEN even if exists
	   IF (IERR .NE. 0) GOTO 92
C construct header
C don't know precisely how many 512-byte blocks will be needed because the
C length to contain the initial integers can depend upon the number of blocks;
C and, must allow for the rare event of one more character in  NBH requireing
C an additional block. so, make initial estimate, then iterate on loop that
C computes the number of blocks required until it will fit.
	   K=9	! minimum number of characters to represent the size integers
	   PLAN=(ID(9)+(K+28+10)-1)/NBH +1 ! estimate number of blocks required
C  K for sizes, 28 for date and its delimiters, 10 for ending
 100	   WRITE (BUF,*) (ID(I),I=1,ID(1)+1),ID(8),NIT,PLAN*NBH ! sizes
	   CC=' '		! initiate header as all blank
	   K=WHITE1 (BUF,CC)	! remove excess spacing; put into header
C  K is last non-blank character
	   LL =(ID(9)+(K+28+10)-1)/NBH +1 ! number of blocks required
D	   write(*,*)'ID(9),K,LL,plan=',ID(9),K,LL,plan !###
	   IF (PLAN.LT.LL) THEN
	     PLAN=PLAN+1
	     GOTO 100
	   ENDIF

C  PLAN is now the number of header blocks that will be written
	   ND1=plan*NBH+1	! first location after header
D	WRITE (*,*) 'BINF5: NBH,ND1,NIT,NBY,NBYTES='
D    &                     ,NBH,ND1,NIT,NBY,NBYTES

	   CC(K+1:K+3)= '<< '	! end of dimensions
	   K=K+3		! current end of stuff

	   CALL CATIME (BUF)	!HK returned string is 22 long
Ckb           call u_datime(buf) !ISIS

	   CC(K+1:K+22)=BUF(1:22) ! move date into header
	   K=K+22		! current end of stuff
	   CC(K+1:K+3)= ' >>'	! end of date
	   K=K+3		! current end of stuff
	   KBLOCK=0
	   J=0			! number of user text bytes alredy written
	   LOC=1		! location to begin writting

 200	   KBLOCK=KBLOCK+1	! current block count
	   IF (KBLOCK.EQ.PLAN) THEN ! this is the last block
	     CC(BA1:BA2) =ARCHCPU ! place byte architecture
	     CC(BC1:NBH) ='C_END' !   and ending
	     I=BT2-K		! available space for text
	   ELSE
	     I=NBH-K		! space available to the end of the block
	   ENDIF

	   LL=ID(9)-J		! number of user text bytes still to write
D	   write(*,*)'kblock,i,j,ll=',kblock,i,j,ll !##
	   IF (LL.GT.0) THEN
	     I=MIN(I,LL)	! amount to write, even if 0
	     CC(K+1:K+I) = HEADER(J+1:J+I) ! move text to header
	   ENDIF
D	   write(*,*)'LOC=',LOC	!###
	   CALL PIO_WT (FID, LOC, NBH, HH, IERR) ! write one block
	   IF (IERR .NE. 0) GOTO 93
	   LOC=LOC+NBH

	   IF (KBLOCK.LT.PLAN) THEN
	     CC=' '		! initiate header as all blank
	     J=J+I		! increment # text bytes written
	     K = 0		! Header text will be moved into output
C				!   header record starting at beginning
	     GOTO 200
	   ENDIF

D	   write(*,*)'LOC,ND1=',LOC,ND1	!###
	   CALL PIO_WT (FID, ND1,NBY, DDATA, IERR) ! write data to file
	   IF (IERR .NE. 0) GOTO 94


	ELSEIF (ACTION .EQ. 'R' .OR. ACTION .EQ. 'r') THEN !======  READ =======

	   CALL PIO_IN (FID, FILENAME, NBYTES, 0, IERR) !  OPEN existing file
	   IF (IERR .NE. 0) GOTO 82
	   LOC=1
	   CALL PIO_RD (FID, LOC, NBH, HH, IERR) ! read first block of header
	   IF (IERR .NE. 0) GOTO 83

C decode sizes in first block of header (now equivalenced into  CC)
	   READ (CC,*,err=330) IDL(1),(IDL(I),I=2,IDL(1)+1)
	1	, IDL(8),NIT, IDL(10) ! sizes
	   goto 340
 330	   write(*,*)'Num_Bytes_in_Header not found: Using 512'
	   READ (CC,*,err=810) IDL(1),(IDL(I),I=2,IDL(1)+1), IDL(8),NIT
	   IDL(10)=512 ! early bin5 version header length
 340	   WRITE (*,*) 'Sizes from file=',IDL	! ??? debug
	   NIT=1		! # items
	   DO I=1,IDL(1)	! compute total array size in bytes
	      J=IDL(I+1)
	      IF (J.LT.1) GOTO 86 ! invalid size
	      NIT=NIT*J
	   ENDDO
	   IF (IDL(8).LT.1 .OR. IDL(8).GT.5) GOTO 89
	   NBYF=NIT*NBYPW(IDL(8)) ! bytes in one item
	   IF (NBYF.GT.NBY) THEN ! file bigger than array space
	      NBD=NBY		! set read size to array space
	      RET=NBYF-NBY	! set number of unread bytes
	   ELSE
	      NBD=NBYF		! set read size to file array size
	      RET=0
	   ENDIF

C read rest of header, transfering to user up to their room limit
	   J=0			   ! number of text bytes transferred
	   K = INDEX (CC,' >>') +2 ! end of  SIZE array
	   LL=NBH		   ! last byte of a block
	   PLAN=(IDL(10)-1)/NBH +1   ! number of blocks in file header
	   DO KBLOCK=1,PLAN	   ! one block at a time into local buffer
	     IF (KBLOCK.GT.1) THEN
	       LOC=LOC+NBH
D	       write(*,*)'LOC=',LOC ! ###
	       CALL PIO_RD (FID,LOC, NBH, HH, IERR) ! read a block
D	       write(*,*)'CC=',cc ! ###
	       IF (IERR .NE. 0) GOTO 83
	     ENDIF
	     IF (ID(9).GT.0) THEN ! user asked for header, extract it
	       IF (KBLOCK.EQ.PLAN) LL=BT2 ! last available byte
	       I = MIN(LL-K,ID(9)-J) ! end of text, or callers room to store
	       IF (I.GT.0) HEADER(J+1:J+I)=CC(K+1:K+I) ! add to header
	       J=J+I		! increment # text bytes written
D	       write(*,*)'Last byt into header  J=',j !###
	       K=0	! text starts at beginning of all but first block
	     ENDIF
	   ENDDO
	   idl(9)=j		! bytes moved to caller

C	   CALL R2R (IDL,ID,10) !HK return file array dimensions
           call u_move4 (10,idl,id) ! return file array dimensions

	   IF (NBD.GT.0) THEN	! read data from file
	       LOC=LOC+NBH
	      ND1= IDL(10)+1
D	      write(*,*)'Read: LOC,ND1,NBD=',LOC,ND1,NBD !###
	      CALL PIO_RD (FID, ND1,NBD, DDATA, IERR)
	      IF (IERR .NE. 0) GOTO 84

	      J=0
	      DO I=1,3
		IF (CC(BA1:BA2).EQ.ARCH(I)) J=BARCH(I) ! get file archit. type
	      ENDDO
	   WRITE (*,*)'BINF5 Read: file arch & J= >',CC(BA1:BA2),'<',J
D	      IF(ID(9).GT.0) WRITE (*,*)'BINF5 Read: header= ',HEADER
C       write (*,'(5a10)')(arch(i),i=1,3)
	      IF (J.EQ.0) GOTO 88 ! unknown type

	      IF (JCPU.NE.J) THEN ! swap bytes 
!ISIS the # lines below
#if defined(SWAP_RESTRICTIONS)
		IF (IDL(8).EQ.4) THEN
		  CALL SWAPBYTE4 (DDATA,NIT,DDATA) ! assumes real*4
		  WRITE(*,*) 'BINF5 swapping bytes. # R*4 words=',NIT
		ELSEIF (IDL(8).GT.1) THEN
		  WRITE(*,*) 'BINF5 fails to swap bytes'
		  RET=-10
		ENDIF
#else
               if (nbypw(idl(8)) .gt. 1) then
                  call u_swapn(nit, nbypw(idl(8)), ddata, ddata) ! ISIS routine
                endif !ISIS
#endif
	      ENDIF
	    ENDIF

        ELSE                    !==========  ACTION not valid ============
	   RET = -1
	   RETURN
	END IF			!=========================================

C successfull read or write: close the binary file and return
        CALL PIO_CL (FID, 0, IERR)
	IF (IERR .NE. 0) RET = -5
	RETURN

C  Error section: set return codes, and delete file if error writing it
 810	RET = RET-1
 89     RET = RET-1
 88     RET = RET-1
 87     RET = RET-1
 86     RET = RET-2
 84     RET = RET-1
 83     RET = RET-1
 82     RET = RET-2
        RETURN

 94     RET = RET-1
 93     RET = RET-1
 92     RET = RET-2
        CALL PIO_CL (FID, 1, IERR)	! delete the binary file
	RETURN

	END
