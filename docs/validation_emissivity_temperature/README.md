# Temperature-dependent emissivity -- validation (EmisT, card 18)

- `Compute_Fit_Emissivity_4KRC.dvrc` / `CheckFunctionFit.sh` / `plotfit.py` --
  the fitting script itself: derives the Emis0-3 polynomial coefficients from a
  Mie/Hapke emissivity model for water ice.
- `inputs/` -- spectra and reference files the fit is built from.
- `Figures/` -- fit-quality plots and tables, one per material/grain-size case.
- `refractive_index_and_w0/` -- upstream derivation (refractive index,
  single-scattering albedo) that the emissivity model itself relies on.

The combined effect of EmisT together with T-dependent conductivity, and the
newer self-consistent joint fit of both, moved to
`Davinci_interface/Improve_IceProperties/` (interface-side tooling) --
see `joint_fit_validation/` there.
