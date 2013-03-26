	SUBROUTINE  PORB0
C_Titl PORB0 Planetary orbit. Read pre-computed matrices and do rotation; minimal for KRC
C_HIST	97jan30  HHK revised from  PORB1
C_END
	INCLUDE 'units.inc'
C commented the statement referencing H.Kieffer's home directory and referenced the same file which was included in the tarball - 09.24.04, r.fergason
C	INCLUDE '/home/hkieffer/krc/porb/porbcm.inc'
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
