      PROGRAM IDUMMY
C_Titl  IDUMMY  Dummy top routine in place of IDL call

      INCLUDE 'filcom.inc'
      
      finput='idldummy.inp'
      fout='idummy.prt'
      fdisk='idummy.tdi'

      kode=0
      iret=idlkrc(kode)
      write(*,*) iret
      kode=33
      iret=idlkrc(kode)
      write(*,*) iret
      stop
      end
