C_Titl  UNITS.INC common  /UNITS/ for logical units and errors  KRC
        INTEGER*4 IOPM,IOKEY,IOIN,IOSP,IOERR,IOD1,IOD2,IOD3,IRTN,IERR
        LOGICAL*4 LOPN1,LOPN2,LOPN3,LOPN4
	COMMON /UNITS/
     & IOPM	! prompt, usually terminal screen
     &,IOKEY	! interactive input, usually terminal keyboard
     &,IOIN	! input; input disk file or terminal
     &,IOSP	! printer (spooled)
     &,IOERR	! error messages, commonly =  IOSP
     &,IOD1	! disk unit for explanation file (temporarily open)
     &,IOD2	! disk output files
     &,IOD3	! spare
     &,IRTN	! subroutine # in which error occured
     &,IERR	! error return code
     &,LOPN1,LOPN2	! status of logical units
     &,LOPN3,LOPN4	!  .TRUE. means that one is currently open
C_Hist	1985----  Hugh_H_Kieffer
C 2004jul06 HK Explicit type statements
C_End____________________________________________________________________________
