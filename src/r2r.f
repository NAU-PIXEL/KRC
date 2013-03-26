	SUBROUTINE R2R(RA,RB,N)
C_TITL  R2R   Real-to-real array move or fill
C_ARGS
	REAL RA(1)  ! [In]  Source array or constant
	REAL RB(1)  ! [Out] Destination array
	INTEGER N   ! [In]  Number of items to move:
C			     If  N>0, copies the source array	
C			     If  N<0, replicates the first item in  RA
C			     Into  RB  |N| times.  I.e., fill  RB
C			      with a constant value.
C_DESC  Note:  Can be used to move single value or to move
C	binary words between variables of different type.	
C	Macro version checks absolute location of arrays and
C	goes forward or backward through arrays as required
C	to insure source values are not overwritten before
C	being read.
C	*****  fortran version does not have overlap protection  *****
C_HIST  70--  Hugh_H_Kieffer at  UCLA
C	78--  Eric_Eliason  Macro version into system library
C	81Oct18  HHK  Fortran version
C_PAUS
	IF(N) 2,9,5
2	III=-N
	RA1=RA(1)
	DO 4 I=1,III
4	RB(I)=RA1
	GOTO 9
5	III=N
	DO 7 I=1,III
7	RB(I)=RA(I)
9	RETURN
	END
