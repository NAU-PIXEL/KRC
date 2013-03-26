	SUBROUTINE  PORB0
C_Titl PORB0 Planetary orbit. Read pre-computed matrices and do rotation; minimal for KRC
C_HIST	97jan30  HHK revised from  PORB1
C_END
	INCLUDE 'units.inc'
	INCLUDE 'porbcm.inc'

	Real*4 P(60)
	EQUIVALENCE (P,PLANUM)

	PI=3.141592654
	READ(IOIN,33)	! skip the line of PORB runtime
	READ(IOIN,33) P
33	FORMAT (5G15.7)
	RETURN
	END
