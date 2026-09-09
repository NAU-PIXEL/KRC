# Refractive index and single-scattering albedo (w0)

Upstream steps feeding the emissivity fit above:

- `ConstructeRefractiveIndex/` -- builds synthetic real/imaginary refractive
  index spectra for amorphous and crystalline ice from lab data (Curtis 2005,
  Hudgins 1993) and cross-checks against Ferrari's published values.
- `Computew0_emissivity/` -- computes the single-scattering albedo and
  emissivity from those indices and checks them against Ferrari's model
  (`Comp_Ferrari_*.png`).
