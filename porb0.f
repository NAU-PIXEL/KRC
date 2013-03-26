	SUBROUTINE  PORB0		! READ-ONLY VERSION FOR KRC

C_HIST	97jan30  HHK revised from  PORB1
C_END
	INCLUDE 'units.inc'
C	INCLUDE '/home/hkieffer/krc/porb/porbcm.inc'
C       SA - Sep 8, 03 - Uncommented porbcm.inc
        INCLUDE 'porbcm.inc'

	Real*4 P(60)
	EQUIVALENCE (P,PLANUM)

	PI=3.141592654
	READ(IOIN,33)	! skip the line of PORB runtime
	READ(IOIN,33) P
33	FORMAT (5G15.7)
	RETURN
	END

	SUBROUTINE ROTATE (B,V, U)
	DIMENSION U(3), V(3), B(9)
C FORMS U=B*V  U AND V MUST NOT BE SAME ARRAY
	DO 100 I=1,3
100	  U(I)=B(I)*V(1)+B(I+3)*V(2)+B(I+6)*V(3)
	RETURN
	END
