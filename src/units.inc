C_Titl  UNITS.INC common  /UNITS/ for logicIO units and errors  KRC
        INTEGER IOPM,IOKEY,IOIN,IOSP,IOERR,IOD1,IOD2,IOD3,IRTN,IERR
        INTEGER IDB1,IDB2,IDB3,IDB4,IDB5,IDB6   ! debug control
        LOGICAL LOPN1,LOPN2,LOPN3,LOPN4
        COMMON /UNITS/
     & IOPM     ! prompt, usually terminal screen
     &,IOKEY    ! interactive input, usually terminal keyboard
     &,IOIN     ! input file
     &,IOSP     ! printer (spooled)
     &,IOERR    ! error messages, commonly =  IOSP
     &,IOD1     ! disk unit for explanation file (temporarily open)
     &,IOD2     ! disk output files
     &,IOD3     ! spare
     &,IRTN     ! subroutine # in which error occured
     &,IERR     ! error return code
     &,IDB1,IDB2,IDB3,IDB4,IDB5,IDB6 ! debug control
     &,LOPN1,LOPN2      ! status of logical IO units
     &,LOPN3,LOPN4      !  .TRUE. means that one is currently open
C_Desc
C IDB tests all write to IOSP
C IDB1 1=mseas_enter 5=tcard_enter
C IDB2 1=tlats_enter 5=tday_enter  6=tday_exit
C IDB3 1=tdisk_enter 3=tdisk_exit
C IDB4 n=TDAY  
C_Hist  1985----  Hugh_H_Kieffer
C 2004jul06  HK  Explicit type statements
C 2010jan12  HK  Remove '*4' from type statements
C 2010apr21  HK  Add debug control
C_End____________________________________________________________________________
