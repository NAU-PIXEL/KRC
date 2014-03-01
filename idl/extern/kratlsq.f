      SUBROUTINE KRATLSQ(KODE,NF,FNX,FNY,MM,KK, COF,DEVV)
C_Titl  KRATLSQ  Version of ratlsq.  Internal spline fit, Double precision
      IMPLICIT NONE
      INTEGER MAXIT             ! max number of iterations
      PARAMETER (MAXIT=5)
      INTEGER KODE     !both. Control 
C         1= Asking for the NPT X values . These will be returned in FNX 
C          KODE will be returned as  NPT=NPFAC*(MM+KK+1)
C            NF must be at least this big. If not, KODE returned as -1
C          Caller must fill NFY(1:KODE) with the Y values 
C         2= Values of NFY have been set for the FNX returned above
C         3= FNX and FNY must be set for NF points spanning the range to be fit.
C            NF can be any value larger than MM+KK+1+1, expected about 2* this.
C          Set Negative to print debug information
      INTEGER NF      !in. Number of elements in fnx and fny
      REAL*8 FNX(*)   !in. Monotonic increasing independant values of function
C          for KODE=1 or 3, (1) and (NF) must contain the X range desired
      REAL*8 FNY(*)   !in. Dependant values of function
      INTEGER MM      !in. Desired order of numerator
      INTEGER KK      !in. Desired order of denominator. Negative set debug
      DOUBLE PRECISION COF(MM+KK+1) !out. Coefficients of the rational function
      DOUBLE PRECISION DEVV(MAXIT)  !out. Max. abs. deviation each iteration
C_Desc  Two modes, set by KODE. Can use cubic splines to interpolate between 
C points, in which case FNX and FNY should be dense around anything that looks 
C like a pole. Or, can call with KODE=1 then 2 to evaluate function at just
C  the points needed.
C  Approximation to function is :
C 1-based: [cof(1)+cof(2)x+...cof(m+1)x^m] / [1.+cof(m+2)x +... cof(m+k+1)x^k]
C 0-based [cof[0]+cof[1]x+...cof[m]x^m] / [1.+cof[m+1]x +... cof[m+k]x^k] 
C See Press: p200, p109
C_Calls: NumRec  dsvbksb  dsvdcmp[pythag]  ratval  
C     and my Real*8 versions: dspline  dsplint
C_Hist 2013jul14,Aug27 Hugh Kieffer    Derive from NumRec ratlsq
C_End 
      INTEGER NPFAC,MAXC,MAXP
      DOUBLE PRECISION a,b
      DOUBLE PRECISION  dev,PIO2,BIG
      PARAMETER (NPFAC=8,MAXC=20,MAXP=NPFAC*MAXC+1,
     *   PIO2=3.141592653589793D0/2.D0,BIG=1.D30)

      INTEGER i,it,j,ncof,npt
      DOUBLE PRECISION devmax,e,hth,pow,sum,bb(MAXP),coff(MAXC),
     *ee(MAXP),fs(MAXP),u(MAXP,MAXC),v(MAXC,MAXC),w(MAXC),wt(MAXP),
     *xs(MAXP),ratval

      real*4 sep /-77.77/  ! separator
      REAL*8 YPN, YOUT, Y2(MAXP)
      LOGICAL DBUG

      DBUG=.FALSE.
      IF (KODE.LT.0) DBUG=.TRUE.
      KODE=abs(KODE)
      ncof=mm+kk+1
      npt=NPFAC*ncof
      dev=BIG

      if (kode.ne.2) then ! calc the X values
         A=FNX(1)               ! low end of fit range
         B=FNX(NF)              ! high " " 
         if (b.le.a) then
            print *,'nf,a,b=',nf,a,b
            print *, (fnx(i),i=0,50)
            goto 82
         endif
         do 11 i=1,npt
            if (i.lt.npt/2) then
               hth=PIO2*(i-1)/(npt-1.d0)
               xs(i)=a+(b-a)*sin(hth)**2
            else
               hth=PIO2*(npt-i)/(npt-1.d0)
               xs(i)=b-(b-a)*sin(hth)**2
            endif
 11      continue
         
         if (kode.eq.1) then    ! return the X values
            if (nf.lt.npt) goto 81 ! error
            do i=1,npt
               fnx(i)=xs(i)     ! reset the X values
            enddo
            KODE=npt            ! tell how many point needed
            goto 9              ! return
         endif
      endif
         
      if (kode.eq.2) then        !  transfer Y values
         if (nf.lt.npt) goto 81 ! error
         do i=1,npt             !
            xs(i)=fnx(i) ! recover the X values
            FS(I)=fny(i) ! transfer the Y values
         enddo
      else                      ! find cubic-spline interpolates
         YPN=2.*BIG             ! signal for natural spline
         CALL DSPLINE(FNX,FNY,NF,YPN,YPN,Y2) ! get 2nd derivatives
         IF (DBUG) THEN 
            WRITE(45,'(5f15.8)')(y2(i),i=1,nf)
            print *,'Done spline, do splint  NF,NPT=',NF,NPT
         ENDIF
         do i=1,npt
            CALL DSPLINT(FNX,FNY,Y2,NF,XS(I),YOUT) ! get function value
            FS(I)=YOUT          !h
         enddo
      endif

C Kode is now either 2 or larger, and the FS are all set
      do i=1,npt ! initialize the weights
        wt(i)=1.d0
        ee(i)=1.d0
      enddo

      IF (DBUG) THEN 
         write(46,344)(xs(i),i=1,npt)
         write(46,344)(fs(i),i=1,npt)
         write(46,344)sep
      ENDIF
 344  format(8f11.5)

      e=0.d0
      do 17 it=1,MAXIT
        do 14 i=1,npt
          pow=wt(i)
          bb(i)=pow*(fs(i)+sign(e,ee(i)))
          do 12 j=1,mm+1
            u(i,j)=pow
            pow=pow*xs(i)
12        continue
          pow=-bb(i)
          do 13 j=mm+2,ncof
            pow=pow*xs(i)
            u(i,j)=pow
13        continue
14      continue
        call dsvdcmp(u,npt,ncof,MAXP,MAXC,w,v)
        call dsvbksb(u,w,v,npt,ncof,MAXP,MAXC,bb,coff)
        devmax=0.d0
        sum=0.d0
        do 15 j=1,npt
          ee(j)=ratval(xs(j),coff,mm,kk)-fs(j)
          wt(j)=abs(ee(j))
          sum=sum+wt(j)
          if(wt(j).gt.devmax)devmax=wt(j)
15      continue
        e=sum/npt
        if (devmax.le.dev) then
          do 16 j=1,ncof
            cof(j)=coff(j)
16        continue
          dev=devmax
        endif
CH        write (*,10) it,devmax
        DEVV(IT)=DEVMAX         ! evolution of max deviation
17    continue
 9    return

C Error section ---------------------------------------------
 82   print *,'b lt a'
 81   print *,'KRATLSQ NF must be at least',npt
      KODE=-1
      goto 9
CH 10    FORMAT (1x,'ratlsq iteration=',i2,' max error=',1pe10.3)
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 53'3.
