	PROGRAM test
C_Titl  KRC planet/comet surface thermal model
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'units.inc'
	INCLUDE 'filcom.inc'

CQ1	COMMON /PORBCM/ PC(60)
C_End
	integer time
	real*4 pp(4)

C			zero out commons
CQ1	CALL R2R (0.,ALB, -NWKRC)		!  KRCCOM
CQ1	CALL R2R (0.,ALAT,-NWLAT)		!  LATCOM
CQ1	CALL R2R (0.,TTS, -NWDAY)		!  DAYCOM
C			set logical units
	IOKEY = 5
	IOD1 = 1
	IOD2 = 2
	IOIN = 3
	IOPM = 6
	IOSP = 7
	IOERR = IOPM
	LOPN2 = .FALSE.
		itim0 = TIME ()
c check  COMMON size
	xxx=77.777
	rad = xxx
	do i=1,5000
	  if (fd(i).eq.xxx) goto 21
	enddo
 21	write(iopm,*)'RAD is at FD',i
	
	xxx=77.788
 	daytim(5)=xxx
	j=i
	do i=j+1,5000
	  if (fd(i).eq.xxx) goto 23
	enddo
 23	write(iopm,*)'DAYTIME(5) is at FD',i
	
	ixxx=7777
	J5=ixxx
	do i=1,5000
	  if (id(i).eq.ixxx) goto 31
	enddo
 31	write(iopm,*)'J5 is at ID_',i
	
	xxx=77.799
	tpf(MAXNH,maxn4)= xxx
	do i=1,5000
	  if (alat(i).eq.xxx) goto 41
	enddo
 41	write(iopm,*)'TPF_end is at ALAT',i
	
	CALL R2R (0.,x, -NWDAY)		!  DAYCOM
	xxx=77.999
	tpfh(MAXNH)= xxx
	do i=1,5000
	  if (x(i).eq.xxx) goto 51
	enddo
 51	write(iopm,*)'TPFH_end is at X',i,' add',maxbot

C			set constants
	PI = 3.14159265
	RAD = 180./PI		! degrees/radian
	SIGSB = 1.354518E-12 ! stephan-boltzman constant: cal:cm:sec:k
C			open files: input, print, save
	do d=10000.,12000, 100.
	  do i=1,4
	    pp(i)=vlpres(i,d)
	  enddo
	  write(iopm,'(f10.1,4f10.3)')d,pp
	enddo

C	CALL CHKMATH(IOSP)
	STOP
	END
