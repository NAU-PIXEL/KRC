	SUBROUTINE ALBVAR (Q1, ALBV)
C_Titl ALBVAR Compute frost albedo as linear function of insolation
C_Vars
	INCLUDE 'krccom.inc'
C_Args:
      REAL Q1    ! IN. average (over a day) solar flux ( direct and diffuse) 
C   incident on a flat surface
      REAL ALBV  ! OUT.  effective hemispheric albedo of frost deposit
C	?? should phase function be here??
C 2005dec28 HK Change to use of IMPLICIT NONE. But here no change needed
C_END

	ALBV = AF1 + AF2*Q1
	RETURN
	END
