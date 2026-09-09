# Temperature-dependent thermal properties of water ice

Feeds KRC's T-dependent conductivity/specific-heat card (12) -- and, jointly,
the T-dependent emissivity card (18, see
`../../KRC_repo_additions/docs/validation/emissivity_temperature/`) -- with
physically based coefficients for water ice instead of generic defaults.

- `Compute_Fit_PropIcyRegolith_4KRC.dvrc` -- the current fitting tool
  (`Compute_Fit_ThermophysicalProperties_IcyRegolith`, L. Lange): a single,
  self-consistent 3rd-order polynomial fit of emissivity, specific heat and
  conductivity vs. temperature for icy regolith, parametrized by grain
  radius, porosity, conductivity mixing model (`GJKRBB`/`GB`/`WBB`/`WGB`) and
  ice form (amorphous/crystalline). This is the improvement referred to in
  `../Interface_modifications.pdf`, Section 5: it supersedes fitting
  conductivity and emissivity separately, since the conductivity fit itself
  depends on the (possibly T-dependent) emissivity.
- `emis_amorphous_ice.txt`, `emis_crystalline_ice.txt`,
  `Syntheticspectrum_wavenumber.txt`, `Radius_emissivity.txt` -- its input
  spectra.
- `joint_fit_validation/` -- effect of the joint fit on KRC surface
  temperature (with/without each T-dependence switched on).
- `Script_Fiticeproperties4KRC/`, `Scripts_reproduceFerrarriandLucas/`,
  `Script_Genericcodetocomputeconductivity/`, `Test_newcondinKRC/` -- earlier,
  conductivity-only version of this work (fit, reproduction of the published
  model it is based on, and end-to-end KRC sensitivity tests). Kept for
  reference/traceability; superseded for new runs by the joint fit above.
