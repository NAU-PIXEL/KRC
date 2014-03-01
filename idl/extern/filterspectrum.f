      INTEGER FUNCTION FILTERSPECTRUM(NFREQ,FREQ,RAD, IOP, RADFILT,
     +        RMSRINGBEFORE,RMSRINGAFTER)
C_Titl  FILTERSPECTRUM remove high-freq. ringing from a  TES radiance spectrum
      IMPLICIT NONE
C_Args
      INTEGER NFREQ             !in. number of frequencies in the spectrum
      REAL*8 FREQ(NFREQ)        !in array of frequencies for spectrum
      REAL*8 RAD(NFREQ)         !in array of radiances for spectrum
      INTEGER IOP               !in. debug print unit; 0=none
c
c  OUTPUT:
      REAL*8 RADFILT(NFREQ)!out. array of radiances for filtered spectrum
      REAL*8 RMSRINGBEFORE !out. quality measure showing 1e8*rms difference 
c                  between neighboring frequencies in original spectrum.
c                  a value of -99. is returned if any radiance
c                  between  RMSFREQMIN and  RMSFREQMAX is less than 1e-7.
      REAL*8 RMSRINGAFTER ! out. same "quality measure" as  RMSRINGBEFORE, but
c                  computed after the filtering has been performed.
      INTEGER STATUS ! out. as the function value
C                =1 no error
c                =11  IMODE out of range  NFREQ must be <0 or >286
c                =12 error in calculation of  RMSRING. need at least
c                    two frequencies in interval to do it.
c                =13  NFREQ too large. must be <=1024.
c
C_Desc
c  Subroutine to filter (remove high-frequency ringing from)
c     a radiance spectrum. the subroutine also returns an the rms
c     difference between neighboring frequencies in the original
c     spectrum as a measure of quality of the spectrum.
c
C_Hist
c  Revision 2.7  1997/11/07 16:48:01
C  97nov19 received as email from  Mike_Smith of  GSFC
C (301) 286-7495  or   michael@chryse.gsfc.nasa.gov.
C  97nov19  Hugh_Kieffer editted for documentation style and case only
C  97nov19  HHK made debug print control an argument
C_End
      INTEGER IMODE,DEBUG,NRING,I,J,IMIN,IMAX,NFFT
      INTEGER NFFTREMOVE,NFREQ2,NFREQCHOP,APOD0,APOD1
      REAL*8 SUMRING
      REAL*8 RMSFREQMIN,RMSFREQMAX,Y(2048),VREF,AVETEMP,SUMTEMP
      REAL*8 DBDT,BTEMP,BTEMP1,TEMP,BB,BT,V,RADIANCE,FACTOR
c
c  Functions for brightness temperature and planck function
c
      BT(V,RADIANCE)=1.438769*V/LOG(1.+1.1910439E-12*V*V*V/RADIANCE)
      BB(V,TEMP)=1.1910439E-12*V*V*V/(EXP(1.438769*V/TEMP)-1.)
c
c  Initialize
c
      DEBUG=IOP ! 0 means no debug write statements
      STATUS=1

      if (debug.ne.0) then
        write(iop,*)'NFREQ,IOP=',NFREQ,IOP
        write(iop,*)'Wavenumbers as input'
        do i=1,nfreq,5
          write(iop,'(i5,5d15.5)')i,(freq(j),j=i,i+4)
        enddo
        write(iop,*)'Radiances as input'
        do i=1,nfreq,5
          write(iop,'(i5,5d15.5)')i,(rad(j),j=i,i+4)
        enddo
      endif
      
c
      DO I=1,NFREQ
         RADFILT(I)=-99.
      END DO
      RMSRINGBEFORE=-99.
      RMSRINGAFTER=-99.
c
c  Parameters
c
      IF (NFREQ.GT.143.AND.NFREQ.LE.286) THEN
         IMODE = 1
      ELSE IF (NFREQ.GT.0.AND.NFREQ.LE.143) THEN
         IMODE = 2
      END IF
c      
      RMSFREQMIN=340.
      RMSFREQMAX=500.
c
      IF (IMODE.EQ.1) THEN
         NFREQCHOP=16
         NFFTREMOVE=17
         APOD0=50
         APOD1=100
      ELSE IF (IMODE.EQ.2) THEN
         NFREQCHOP=8
         NFFTREMOVE=9
         APOD0=25
         APOD1=50
      ELSE
         IF (DEBUG.NE.0) WRITE(IOP,*) 'imode out of range'
         STATUS=11
         GOTO 999
      END IF

c
c  Find ringing amplitude (before filtering)
c
      NRING=0
      SUMRING=0.
      SUMTEMP=0.
      BTEMP=-1.
      IF (DEBUG.NE.0) WRITE(IOP,*)
     +     'finding ringing amplitude. i,btemp(i),btemp(i+1)'
      DO I=1,NFREQ-1
         IF ((FREQ(I).GE.RMSFREQMIN).AND.(FREQ(I+1).LE.RMSFREQMAX)) THEN
            IF ((RAD(I).LT.1.E-7).OR.(RAD(I+1).LT.1.E-7)) THEN
               IF (DEBUG.NE.0)
     +              WRITE(IOP,*) 'Found a radiance <1.e-7:',I,RAD(I)
               RMSRINGBEFORE=-99.
               GOTO 100
            END IF
            NRING=NRING+1
            BTEMP1=BT(FREQ(I+1),RAD(I+1))
            IF (BTEMP.LE.0.) BTEMP=BT(FREQ(I),RAD(I))
            SUMRING=SUMRING+(BTEMP1-BTEMP)*(BTEMP1-BTEMP)
            IF (DEBUG.NE.0) WRITE(IOP,*) I,BTEMP,BTEMP1
            SUMTEMP=SUMTEMP+BTEMP
            BTEMP=BTEMP1
         END IF
      END DO
      IF (DEBUG.NE.0) WRITE(IOP,*) 'nring, sumring=',NRING,SUMRING
      IF (NRING.EQ.0) THEN
       IF (DEBUG.NE.0) WRITE(IOP,*) 'Error - less than two points in'//
     +         ' range for rmsring calculation'
         STATUS=12
         GOTO 999
      ELSE
         VREF=.5*(RMSFREQMIN+RMSFREQMAX)
         AVETEMP=SUMTEMP/NRING
         DBDT=BB(VREF,AVETEMP+.5)-BB(VREF,AVETEMP-.5)
         RMSRINGBEFORE=1.E8*DBDT*SQRT(SUMRING/NRING)
         IF (DEBUG.NE.0)
     +     WRITE(IOP,*) 'avetemp,dbdt,<dt>,rmsringbefore=',
     +     AVETEMP,DBDT,SQRT(SUMRING/NRING),RMSRINGBEFORE/1.E8
      END IF
c
c  Filter spectrum (to remove "ringing")
c
c  First, find the right size.
c
 100  NFREQ2=NFREQ-NFREQCHOP
      IF ((NFREQ2.GE.128).AND.(NFREQ2.LE.255)) THEN
         NFFT=256
      ELSE IF ((NFREQ2.GE.256).AND.(NFREQ2.LE.511)) THEN
         NFFT=512
      ELSE
         NFFT=2**INT((LOG(NFREQ2+0.5)/LOG(2.)))
         IF (NFFT.LT.NFREQ2) NFFT=NFFT*2
      END IF
      IF (DEBUG.NE.0) WRITE(IOP,*) 'nfreq, nfreqchop, nfreq2, nfft=',
     +        NFREQ,NFREQCHOP,NFREQ2,NFFT
      IF (NFFT.GT.1024) THEN
         IF (DEBUG.NE.0) WRITE(IOP,*) 'nfft too large. Must be <=1024'
         STATUS=13
         GOTO 999
      END IF
c
c  Fill the array to be fourier transformed. leave off the first
c      nfreqchop points
c
      DO I=0,NFREQ-NFREQCHOP-1
         Y(2*I+1)=RAD(I+NFREQCHOP+1)
         Y(2*I+2)=0.
      END DO
c
c  Pad array with zeros
c
      DO I=NFREQ-NFREQCHOP,NFFT-1
         Y(2*I+1)=0.
         Y(2*I+2)=0.
      END DO
      IF (DEBUG.NE.0) THEN
         WRITE(IOP,*) 'orig truncated spect'
         DO I=0,NFFT-1
           WRITE(IOP,*) I,Y(2*I+1)
         END DO
      END IF
c
c  Do the forward fft. we don't care about getting the correct
c     fft frequency array back (f).
c
      CALL FFT(Y,NFFT,1)
c
c  Remove the high frequency spike by replacing the nfftremove
c      highest frequencies with interpolated values.
c
      IF (DEBUG.NE.0) THEN
         WRITE(IOP,*) 'transformed spectrum'
         DO I=0,NFFT-1
           WRITE(IOP,*) I,Y(2*I+1),Y(2*I+2)
         END DO
      END IF
      IMIN=NFFT/2-NFFTREMOVE/2
      IMAX=IMIN+NFFTREMOVE-1
      IF (DEBUG.NE.0) WRITE(IOP,*)'for fftremove:imin,imax=',IMIN,IMAX
      DO I=IMIN,IMAX
         Y(2*I+1)=Y(2*IMIN-1)+(I-IMIN+1)*(Y(2*IMAX+3)-Y(2*IMIN-1))/
     +                                     (IMAX-IMIN+2)
         Y(2*I+2)=Y(2*IMIN)+(I-IMIN+1)*(Y(2*IMAX+4)-Y(2*IMIN))/
     +                                     (IMAX-IMIN+2)
      END DO
c
c  Apodize the transformed spectrum
c
      IF (APOD1.GT.0) THEN
         IF (DEBUG.NE.0) WRITE(IOP,*) 'apodization factors'
         DO I=NFFT/2-APOD1,NFFT/2+APOD1
            IF (ABS(I-NFFT/2).LT.APOD0) THEN   ! in region to just zero out
               Y(2*I+1)=0.
               Y(2*I+2)=0.
               IF (DEBUG.NE.0) WRITE(IOP,*) I,0.
            ELSE              ! in region to multiply by factor x, 0<x<1
               FACTOR=FLOAT(ABS(I-NFFT/2)-APOD0)/FLOAT(APOD1-APOD0)
               Y(2*I+1)=Y(2*I+1)*FACTOR
               Y(2*I+2)=Y(2*I+2)*FACTOR
               IF (DEBUG.NE.0) WRITE(IOP,*) I,FACTOR
            END IF
         END DO
         IF (DEBUG.NE.0) THEN
            WRITE(IOP,*) 'apodized transformed spectrum'
            DO I=0,NFFT-1
              WRITE(IOP,*) I,Y(2*I+1),Y(2*I+2)
            END DO
         END IF
      END IF
c
c  Do the reverse fft to get the filtered spectrum
c
      CALL FFT(Y,NFFT,-1)
c
c  Reconstruct filtered spectrum
c
      DO I=1,NFREQCHOP
         RADFILT(I)=0.
      END DO
      DO I=0,NFREQ-NFREQCHOP-1
         RADFILT(I+NFREQCHOP+1)=Y(2*I+1)
      END DO
      IF (DEBUG.NE.0) THEN
         WRITE(IOP,*) ' i, filtered spectrum'
         DO I=0,NFFT-1
            WRITE(IOP,*) I,Y(2*I+1),Y(2*I+2)
         END DO
      END IF
c
c  Find ringing amplitude (after filtering)
c
      NRING=0
      SUMRING=0.
      SUMTEMP=0.
      BTEMP=-1.
      IF (DEBUG.NE.0) WRITE(IOP,*)
     +     'finding ringing amplitude. i,btemp(i),btemp(i+1)'
      DO I=1,NFREQ-1
         IF ((FREQ(I).GE.RMSFREQMIN).AND.(FREQ(I+1).LE.RMSFREQMAX)) THEN
            IF ((RADFILT(I).LT.1.E-7).OR.(RADFILT(I+1).LT.1.E-7)) THEN
               IF (DEBUG.NE.0)
     +              WRITE(IOP,*) 'Found a radiance <1.e7:',I,RADFILT(I)
               RMSRINGAFTER=-99.
               GOTO 999
            END IF
            NRING=NRING+1
            BTEMP1=BT(FREQ(I+1),RADFILT(I+1))
            IF (BTEMP.LE.0.) BTEMP=BT(FREQ(I),RADFILT(I))
            SUMRING=SUMRING+(BTEMP1-BTEMP)*(BTEMP1-BTEMP)
            IF (DEBUG.NE.0) WRITE(IOP,*) I,BTEMP,BTEMP1
            SUMTEMP=SUMTEMP+BTEMP
            BTEMP=BTEMP1
         END IF
      END DO
      IF (DEBUG.NE.0) WRITE(IOP,*) 'nring, sumring=',NRING,SUMRING
      IF (NRING.EQ.0) THEN
       IF (DEBUG.NE.0) WRITE(IOP,*) 'Error - less than two points in'//
     +         ' range for rmsring calculation'
         STATUS=12
         GOTO 999
      ELSE
         VREF=.5*(RMSFREQMIN+RMSFREQMAX)
         AVETEMP=SUMTEMP/NRING
         DBDT=BB(VREF,AVETEMP+.5)-BB(VREF,AVETEMP-.5)
         RMSRINGAFTER=1.E8*DBDT*SQRT(SUMRING/NRING)
         IF (DEBUG.NE.0)
     +     WRITE(IOP,*) 'avetemp,dbdt,<dt>,rmsringafter=',
     +     AVETEMP,DBDT,SQRT(SUMRING/NRING),RMSRINGAFTER/1.E8
      END IF
c
c  Done
c
 999  FILTERSPECTRUM=STATUS
      RETURN
      END

c*************************************************************************
      SUBROUTINE FFT(DATA,NN,ISIGN)
      IMPLICIT NONE
      INTEGER ISIGN,NN,I,ISTEP,J,M,MMAX,N
      REAL*8 DATA(2048),TEMPI,TEMPR,THETA,WI,WPI,WPR,WR,WTEMP
c
c  Subroutine to find fast fourier transform. based on numerical
c    recipes subroutine "four1" (section 12.2). to perform forward
c    transform set isign=1, for reverse transform set isign=-1.
c    data is in an array of length 2*nn and is stored real_1,
c    imag_1, real_2, imag_2, ... , real_nn, imag_nn.
c
      N=2*NN
      J=1
      DO I=1,N,2
         IF (J.GT.I) THEN
            TEMPR=DATA(J)
            TEMPI=DATA(J+1)
            DATA(J)=DATA(I)
            DATA(J+1)=DATA(I+1)
            DATA(I)=TEMPR
            DATA(I+1)=TEMPI
         END IF
         M=N/2
 10      IF ((M.GE.2).AND.(J.GT.M)) THEN
            J=J-M
            M=M/2
            GOTO 10
         END IF
         J=J+M
      END DO
      MMAX=2
 20   IF (N.GT.MMAX) THEN
         ISTEP=2*MMAX
         THETA=6.28318530717959/(ISIGN*MMAX)
         WPR=-2.*SIN(0.5*THETA)**2
         WPI=SIN(THETA)
         WR=1.
         WI=0.
         DO M=1,MMAX,2
            DO I=M,N,ISTEP
               J=I+MMAX
               TEMPR=WR*DATA(J)-WI*DATA(J+1)
               TEMPI=WR*DATA(J+1)+WI*DATA(J)
               DATA(J)=DATA(I)-TEMPR
               DATA(J+1)=DATA(I+1)-TEMPI
               DATA(I)=DATA(I)+TEMPR
               DATA(I+1)=DATA(I+1)+TEMPI
            END DO
            WTEMP=WR
            WR=WR*WPR-WI*WPI+WR
            WI=WI*WPR+WTEMP*WPI+WI
         END DO
         MMAX=ISTEP
         GOTO 20
      END IF
      IF (ISIGN.EQ.-1) THEN
         DO I=1,N
            DATA(I)=DATA(I)/NN
         END DO
      END IF
      RETURN
      END

