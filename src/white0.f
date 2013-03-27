      integer function white0(ss, ww)
C_Titl  WHITE0: remove all white space
      implicit none
C_Args
      character*(*) SS  ! in. string to be treated
      character*(*) WW  ! out. string with no blanks or tabs
C      integer white0   ! f. out. length of output string
C_Desc
C Transfers all characters except blank or tab
C Stops processing when either input exhausted or output full
C_Bugs
C no warning if input was too long to fit in output
C_Hist 2011aug11  Hugh Kieffer  Original version. Derive from white1.f
C_end

      integer i,j,lss,lww
      character*1 c             ! current character

      lss=len(ss) ! get defined length of input and output strings
      lww=len(ww)
      
      j=0                       ! # characters output thus far
      do i=1,lss
         c=ss(i:i)              ! current character
         if (c.ne.' ' .and. c.ne.CHAR(9)) then ! was not blank or TAB
            j=j+1               ! increment output byte
            ww(j:j)=c           ! tranfer character to output
         endif
         if (j.eq.lww) goto 9   ! output area full, quit
      enddo
 9    white0=j
      return
      end
      
