# Solar penetration in the ground -- validation (RADGND, card 17)

- `Validation_solarpenetration.dv` -- main Davinci validation script.
- `LL26_reference/` -- analytical/reference solution the KRC output is checked
  against.
- `Test_effectTIvssolarpene/` -- effect of the absorption e-folding length on
  best-fit thermal inertia, for several absorption depths and albedo/TI
  perturbations.
- `Test_adaptativeFLAY/` -- checks the interface-side adaptive first-layer
  thickness (see `Davinci_interface/krc_davinciinterface.dvrc`, Section 4 of
  `Interface_modifications.pdf`): fixed vs. adaptive FLAY, at two thermal
  inertias.
- `Test_constantabslength_vs_cosilength/` -- checks scaling of the absorption
  length with incidence angle (constant vs. 1/cos(i)).
