	SUBROUTINE BINF5 (ACTION,FILENAME, HEADER,ID,DDATA, IRET)
C_Titl  BINF5  read/write binary file with self-describing header, sim.  IDL
	IMPLICIT NONE
C****************************************************************************
C_Args  Type          name          i/o  Description
        CHARACTER*1   ACTION       ! i   action - W=write,  else=Read  with:
				   !     F=Force read even if byte order unknown
				   !   can be either uppercase or lowercase
	CHARACTER*(*) FILENAME     ! i   input/output filename
	CHARACTER*(*) HEADER       ! b   header text data
	INTEGER*4 ID(10)	   ! b   array type and dimension
C (1) = number of dimensions
C (2:7) = dimensions 1:6
C (8) = array type:  1=byte  2=I*2  3=I*4  4=R*4  5=R*8
C (9) = header text length (in: actual for  W, max for  R)
C (10) = total header size in bytes (multiples of 512). set here
	BYTE DDATA(*)	           ! b   data array;
        INTEGER*4     IRET	   ! b   debug control & return code
C input:  0=normal +1=report many values   +2 report progress  +3 both
C output:        0 = normal return
C               >0 = file was this much larger than room provided by caller
C               -1 = Requested action not valid
C                other negative are errors, see the last part of code
C_Descr
C  Does all file operations during a single call;
C   open, read or write, close
C  WARNING: will overwrite existing file
C  Note: the  ID values must be set for each  W call;
C   this routine does not remember array sizes.
C  For the read option, the dimensions will be considered for the maximum
C   room allowed for the array to be returned.  IRET >0 indicates how much
C   bigger was the array on file.

C  This routine meant to be usable on a number of platforms, and to detect if
C byte swap is needed after reading a file.
C  The initial integer items in the file header are meant to be compatible
C with the  IDL routine bin5.
C  Does not include the file creation data in the returned header
C  First block of header can hold 455 characters of user text.
C  Returned header may have up to 501 trailing blanks
C
C  !ISIS  indicate isis versions of code,  versus  !HK
C_Lim
	INTEGER*4  MAXT,MAXD,NBH,MAXARC
	PARAMETER (MAXT=5)	! number of defined word types
	PARAMETER (MAXARC=8)	! number of machine architectures
	PARAMETER (MAXD=6)	! max number of dimensions
	PARAMETER (NBH=512)	! total length of header buffer
C   on read, if  Header is too long, last part will be trimmed.
C_Lien
C  Machine architecture firmcoded as 64-bit
C_Bugs
C this version does byte-swapping after a read, but assumes  R*4 word type
C_Call  PIO_ system  B2B  BIGEND  CATIME WHITE1 
C_Hist	98may12  HHKieffer  Adopted from  binfil.f
C 98sep07  HHK replace  B2B use for character varable
C 98dec29  HHK disable byteswap on read ???
C 99nov05  HHK allow longer headers
C  and include as ckb real differences from the version at
C    /home/kbecker/isis/apps/rolo2/libs/librolo/binf5.f
C 2004jul12 HK Add arch-type for mac.
C 2008mar31 HK Replace  SWAPBYTER4 with swapbyte4
C           and  Replace c## with d for conditional compile
C           and temporary firmcode machine architecture   << beware
C 2010apr06-may22  HK derive this debug version from bin5f.f
C 2012mar21 HK Minor cleanup, seperate error 86 into 85 and 86. Add error -11
C 2013jan02 HK Fix cpu endian detection and automatic byteswap, Allow F action
C_End

	INTEGER*4  BT2,BA1,BA2,BC1 ! fixed byte locations
	PARAMETER (BC1=NBH-4)	! first of  C_END
	PARAMETER (BA2=BC1-1)	! last byte of architecture
	PARAMETER (BA1=BC1-8)	! first " " 2008 compatibility with bin5.pro
	PARAMETER (BT2=BA1-1)	! last room for text

	BYTE HH(NBH)		! one block of header
	CHARACTER*512 CC	!  MUST be same size as  HH
	EQUIVALENCE (CC,HH)	! align character and byte buffers

	INTEGER*4 IERR		! return code from  PIO routines
        INTEGER*4 NBD
	INTEGER*4 ND1		! first byte after header
        INTEGER*4 NBYTES

	INTEGER*4 FID,I,IARCH,J,K,KBLOCK,LL,LOC,NBY,NBYF,NIT,PLAN

	INTEGER*4 NBYPW(MAXT) /1,2,4,4,8/ ! Num bytes per word for IDL types
	INTEGER*4 IDL(10)	! array to contain idl size values
	CHARACTER*120 BUF	! used for time and id array
	CHARACTER*6 ARCH(MAXARC) /'sparc ','bigend','litend','alpha '
     &  ,'ppc   ','x86   ','x86_64','unknow'/ ! hardware architecture
	INTEGER*4 BARCH(MAXARC) / 1,1,2,2,2,2,2,0 / ! byte-order type 
	INTEGER*4 JCPU		! endian type: 1=big 2=little 0=unknown
	CHARACTER*6 BUF6
	CHARACTER*8 BUF8

	INTEGER*4 WHITE1, BIGEND	! functions
	LOGICAL*4 DBG1		! set to report sizes
	LOGICAL*4 DBG2		! set to write as code locations are reached
C****************************************************************************
D	print *,'BINF5: input IRET=',IRET
	DBG1=MOD(IRET,2).EQ.1	! set if  IRET odd   Report values
	DBG2=IRET.GE.2		! set if  IRET >= 2  Report progress
	IF (DBG1.OR.DBG2) WRITE(*,*) 'BINF5 debug flags=',DBG1,DBG2
	IRET = 0			! set return flag for no error
	IF (DBG1) WRITE(*,*)'BINF5: INPUTS: Action=',ACTION
	IF (DBG1) WRITE(*,*)'FILENAME=',FILENAME
	IF (DBG1) WRITE(*,*)'ID=',ID
	IF (DBG1) WRITE(*,*)'Header=>>',HEADER,'<<'

	IF (ID(8).LT.1 .OR. ID(8).GT.MAXT) GOTO 85 ! invalid type
	IF (ID(1).LT.1 .OR. ID(1).GT.MAXD) GOTO 85 ! invalid # dimensions
	NIT=1			! # items in dimensions thus far
	DO I=1,ID(1)		! compute total array size in items
	  J=ID(I+1)
	  IF (J.LT.1) GOTO 85	! invalid size
	  NIT=NIT*J
	ENDDO
	NBY=NIT*NBYPW(ID(8))	! caller array bytes total
	NBYTES = NBH+NBY        ! total # bytes in header
	IF (DBG1) WRITE(*,*) 'NBY,NBYTES=',NBY,NBYTES

	J=BIGEND()		! returns 1 if big, 0 if little. lse is error
	JCPU=2-J		! 1 if big, 2 if little

        WRITE (*,*) 'BINF5: ARCH S= ', ARCH(JCPU+1),' <',JCPU
	WRITE (*,*) 'BINF5 2013jan02 called with ID= ',ID !<<  Version

	IF (ACTION .EQ. 'W' .OR. ACTION .EQ. 'w') THEN !=======  WRITE ========
	   CALL PIO_IN (FID, FILENAME, NBYTES, 3, IERR) !  OPEN even if exists
	   IF (DBG2) WRITE(*,*)'@2 ',IERR
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
C            K is last non-blank character
	   LL =(ID(9)+(K+28+10)-1)/NBH +1 ! number of blocks required
 	   IF (DBG1) WRITE(*,*)'ID(9),K,LL,plan=',ID(9),K,LL,PLAN !###
	   IF (PLAN.LT.LL) THEN
	     PLAN=PLAN+1
	     GOTO 100
	   ENDIF

C  PLAN is now the number of header blocks that will be written
	   ND1=PLAN*NBH+1	! first location after header
	   IF (DBG1) WRITE (*,*) 'BINF5: NBH,ND1,NIT,NBY,NBYTES='
     &                     ,NBH,ND1,NIT,NBY,NBYTES

	   CC(K+1:K+3)= '<< '	! end of dimensions
	   K=K+3		! current end of stuff

	   IF (DBG2) WRITE(*,*) '@3'
	   CALL PIO_IN (FID, FILENAME, NBYTES, 3, IERR) !  OPEN even if exists
	   IF (DBG2) WRITE(*,*) '@4'
	   CALL CATIME (BUF)	!hk returned string is 22 long
	   IF (DBG1.OR.DBG2) WRITE(*,*) '@5 Time=',BUF
Ckb           call u_datime(buf) !isis

	   CC(K+1:K+22)=BUF(1:22) ! move date into header
	   K=K+22		! current end of stuff
	   CC(K+1:K+3)= ' >>'	! end of date
	   K=K+3		! current end of stuff
	   KBLOCK=0
	   J=0			! number of user text bytes alredy written
	   LOC=1		! location to begin writting

 200	   KBLOCK=KBLOCK+1	! current block count
	   IF (KBLOCK.EQ.PLAN) THEN ! this is the last block
	     CC(BA1:BA2) =ARCH(JCPU+1) ! place byte architecture
	     CC(BC1:NBH) ='C_END' !   and ending
	     I=BT2-K		! available space for text
	   ELSE
	     I=NBH-K		! space available to the end of the block
	   ENDIF

	   LL=ID(9)-J		! number of user text bytes still to write
	   IF (DBG1) WRITE(*,*)'plan,kblock,i,j,ll=',PLAN,KBLOCK,I,J,LL !##
	   IF (LL.GT.0) THEN
	      I=MIN(I,LL)	! amount to write, even if 0
	      CC(K+1:K+I) = HEADER(J+1:J+I) ! move text to header <<<<<
	   ENDIF
	   IF (DBG1.OR.DBG2) WRITE(*,*)'@6 LOC=',LOC !###
	   CALL PIO_WT (FID, LOC, NBH, HH, IERR) ! write one block
	   IF (DBG2) WRITE(*,*) '@7'
	   IF (IERR .NE. 0) GOTO 93
	   LOC=LOC+NBH

	   IF (KBLOCK.LT.PLAN) THEN
	     CC=' '		! initiate header as all blank
	     J=J+I		! increment # text bytes written
	     K = 0		! header text will be moved into output
C				!   header record starting at beginning
	     GOTO 200
	   ENDIF

	   IF (DBG1.OR.DBG2) WRITE(*,*)'@8 LOC,ND1,NBY=',LOC,ND1,NBY !###
C   FID, starting byte, Number bytes, buffer to write, return code    
	   CALL PIO_WT (FID, ND1,NBY, DDATA, IERR) ! write data to file
	   IF (IERR .NE. 0) GOTO 94

	ELSE !=========================== READ ============================
	   
C	   IF (ACTION .EQ. 'R' .OR. ACTION .EQ. 'r') THEN
	   DO I=1,10
	      IDL(I)=0 ! initiate all values
	      ENDDO
C here, nbytes should be output as the file size in bytes
	   CALL PIO_IN (FID, FILENAME, NBYTES, 0, IERR) !  OPEN existing file
	   IF (DBG2) WRITE(*,*)'@2 NBYTES,ierr=',NBYTES,IERR
	   IF (IERR .NE. 0) GOTO 82
	   LOC=1
	   IF (DBG1) WRITE(*,*)'NBH=',NBH
C arg2 is starting byte, arg3 is # bytes to read
	   CALL PIO_RD (FID, LOC, NBH, HH, IERR) ! read first block of header
	   IF (DBG2) WRITE(*,*)'@2b HH==CC=',CC
	   IF (IERR .NE. 0) GOTO 83

C decode sizes in first block of header (now equivalenced into  CC)
	   READ (CC,*,ERR=330) IDL(1),(IDL(I),I=2,IDL(1)+1)
     &     , IDL(8),NIT, IDL(10) ! sizes
	   GOTO 340
 330	   WRITE(*,*)'Num_Bytes_in_Header not found: Using 512'
	   READ (CC,*,ERR=810) IDL(1),(IDL(I),I=2,IDL(1)+1), IDL(8),NIT
	   IF (DBG2) WRITE(*,*)'IDL=',IDL
	   IDL(10)=512 ! early bin5 version header length
 340	   WRITE (*,*) 'Sizes from file=',(IDL(I),I=1,IDL(1)+1),IDL(8)	! ??? debug
	   NIT=1		! # items
	   DO I=1,IDL(1)	! compute total array size in items
	      J=IDL(I+1)
	      IF (J.LT.1) GOTO 86 ! invalid size
	      NIT=NIT*J
	   ENDDO
	   IF (DBG2) WRITE(*,*)'NIT=',NIT
	   IF (IDL(8).LT.1 .OR. IDL(8).GT.5) GOTO 89
	   NBYF=NIT*NBYPW(IDL(8)) ! array size in bytes
	   IF (NBYF.GT.NBY) THEN ! file bigger than array space
	      NBD=NBY		! set read size to array space
	      IRET=NBYF-NBY	! set number of unread bytes
	   ELSE
	      NBD=NBYF		! set read size to file array size
	      IRET=0
	   ENDIF
	   IF (DBG2) WRITE(*,*)'NBD,IRET=',NBD,IRET
C read rest of header, transfering to user up to their room limit
	   J=0			   ! number of text bytes transferred
	   K = INDEX (CC,' >>') +2 ! end of  SIZE array
	   LL=NBH		   ! last byte of a block
	   PLAN=(IDL(10)-1)/NBH +1   ! number of blocks in file header
	   DO KBLOCK=1,PLAN	   ! one block at a time into local buffer
	     IF (KBLOCK.GT.1) THEN
	       LOC=LOC+NBH
	        IF (DBG1) WRITE(*,*)'LOC=',LOC ! ###
	       CALL PIO_RD (FID,LOC, NBH, HH, IERR) ! read a block
	       IF (DBG1)  WRITE(*,*)'CC=',CC ! ###
	       IF (IERR .NE. 0) GOTO 83
	     ENDIF
	     IF (ID(9).GT.0) THEN ! user asked for header, extract it
	       IF (KBLOCK.EQ.PLAN) LL=BT2 ! last available byte
	       I = MIN(LL-K,ID(9)-J) ! end of text, or callers room to store
	       IF (I.GT.0) HEADER(J+1:J+I)=CC(K+1:K+I) ! add to header
	       J=J+I		! increment # text bytes written
	       IF (DBG1) WRITE(*,*)'Last byt into header  J=',J !###
	       K=0	! text starts at beginning of all but first block
	     ENDIF
	   ENDDO
	   IDL(9)=J		! bytes moved to caller

C	   call r2r (idl,id,10) !hk return file array dimensions
           CALL U_MOVE4 (10,IDL,ID) ! return file array dimensions

	   LOC=LOC+NBH		! read data from file
	   ND1= IDL(10)+1
	   IF (DBG1) WRITE (*,*)'Read: LOC,ND1,NBD=',LOC,ND1,NBD !###
	   CALL PIO_RD (FID, ND1,NBD, DDATA, IERR) ! read data array
	   IF (IERR .NE. 0) GOTO 84

	   J=0
	   BUF8=CC(BA1:BA2)	! extract the word architecture
C	   buf8= adjustl(buf8) !not in gfortran	! left adjust ( should not be needed)
	   BUF6=BUF8(1:6)	! only the first 6 non-blank
	   IF (DBG1) WRITE (*,*)'BA1:BA2=',BA1,BA2
	   IF (DBG1) WRITE (*,*)'BUF8,6 >',BUF8,'<>',BUF6,'<'
	   DO I=1,MAXARC
	      IF (BUF6.EQ.ARCH(I)) J=BARCH(I) ! get file archit. type
	   ENDDO
	   WRITE (*,*)'BINF5 Read: file arch, I,J= >',BUF8,'<',I,J
	   IF (DBG1 .OR.ID(9).GT.0) 
     &        WRITE (*,*)'BINF5 Read: header= ',HEADER
C       write (*,'(5a10)')(arch(i),i=1,3)
	   IF (J.EQ.0) THEN 	! unknown type
	      WRITE (*,*) 'BINF5 read: file byte order unknown'
	      IF (ACTION .NE. 'F' .AND. ACTION .NE. 'f') GOTO 88 ! error exit
	      J=JCPU		! force no-swap
	   ENDIF
	   IF (J.NE.JCPU) THEN	! swap bytes
!isis the # lines below
#if defined(SWAP_RESTRICTIONS)
	      IF (IDL(8).EQ.4) THEN
		 CALL SWAPBYTE4 (DDATA,NIT,DDATA) ! assumes real*4
		 WRITE(*,*) 'BINF5 swapping bytes. # R*4 words=',NIT
	      ELSEIF (IDL(8).GT.1) THEN
		 WRITE(*,*) 'BINF5 fails to swap bytes'
		 IRET=-10
	      ENDIF
#else
	      IF (NBYPW(IDL(8)) .GT. 1) THEN
		 CALL U_SWAPN(NIT, NBYPW(IDL(8)), DDATA, DDATA) !isis routine
	      ENDIF		!isis
#endif
	   ENDIF
	ENDIF			!=========================================

C successfull read or write: close the binary file and return
	IF (DBG2) WRITE(*,*) '@10'
        CALL PIO_CL (FID, 0, IERR)
	IF (DBG2) WRITE(*,*) '@11'
	IF (IERR .NE. 0) IRET = -11 ! Error closing file
	RETURN

C  Error section: set return codes, and delete file if error writing it
 810	IRET = IRET-1  ! -10 error converting Header prefix to integer
 89     IRET = IRET-1  ! -9 invalid word type found in header:  IDL(8) 
 88     IRET = IRET-2  ! -8 unknown word type upon read
 86     IRET = IRET-1  ! -6 a dimension read in header was non-positive 
 85     IRET = IRET-1  ! -5 invalid item in input  ID
 84     IRET = IRET-1  ! -4 error reading data
 83     IRET = IRET-1  ! -3 error reading some block of header
 82     IRET = IRET-2  ! -2 error opening file to read
        RETURN

C Error during WRITE of file
 94     IRET = IRET-1  ! -4 error writing data to file
 93     IRET = IRET-1  ! -3 error writing a block of header
 92     IRET = IRET-2  ! -2 error opening file for write
	IF (DBG2) WRITE(*,*) '@12'
        CALL PIO_CL (FID, 1, IERR)	! close and delete the binary file
	IF (DBG2) WRITE(*,*) '@13'
	RETURN

	END
