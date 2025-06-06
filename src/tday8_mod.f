      module tday8_mod
        implicit none
        private

        public :: update_frost_layer

      contains 

        subroutine update_surface_frost(ATMRAD, FAC9, FROEX, SHEATF, FAC7, TSUR, POWER, JJ, FAC6F, FEMIT, LPH, FEFAC, DTIM, LFROST, FAC8, EMTIR, LOPN3, FARAD, TTJ, ASOL, SOLDIF, PLANH, PLANV, TATMJ, AFNOW, ALB, EFROST, EMIS, CFROST)
          real(kind=8), intent(out) :: ATMRAD, POWER, FAC8
          real(kind=8), intent(in) :: FAC9, FROEX, FAC7, TSUR, FAC6F, FEMIT, FEFAC, DTIM, EMTIR, TATMJ, AFNOW, ALB, EMIS, CFROST
          real(kind=8), intent(inout) :: SHEATF, EFROST

          real(kind=8), intent(in) :: FARAD(:), TTJ(:), ASOL(:)
          real(kind=8), intent(in) :: SOLDIF(:), PLANH(:), PLANV(:)

          integer, intent(in) :: JJ
          
          logical, intent(out) :: LFROST
          logical, intent(in) :: LPH, LOPN3

          real(kind=8) :: Q4, DFROST
          ATMRAD= FAC9*TATMJ**4 ! hemispheric downwelling  IR flux
          Q4 = AFNOW + (ALB-AFNOW)*DEXP(-EFROST/FROEX) ! albedo for frost layer
          
          SHEATF= FAC7*(TTJ(2)-TSUR) ! upward heatflow into the surface
          !   unbalanced flux into surface
          ! FEMIT=FAC6F*SIGSB*TFNOW**4 is [[skyfac]]*Femis*sig*Tf^4
          POWER= (1.D0-Q4)*ASOL(JJ) +(1.D0-Q4)*SOLDIF(JJ) + FAC6F*ATMRAD + SHEATF - FEMIT
          IF (LPH) POWER=POWER+EMIS*PLANH(JJ)+(1.D0-Q4)*PLANV(JJ) ! planetary
          ! If fff, add back-radiation=(1-skyfac)*femis*emis_x*sig*Tfar^4
          IF (LOPN3) POWER=POWER+ FEFAC*FARAD(JJ)
          DFROST = -POWER/CFROST ! rate of frost formation or sublimation
          EFROST=EFROST + DFROST*DTIM ! amount on ground; kg*m**-2
          IF (EFROST.LE.0.) THEN ! reset to bare ground
            LFROST = .FALSE.
            EFROST = 0.
            FAC8=EMTIR*EMIS
          ENDIF
        end subroutine update_surface_frost

      end module tday8_mod  