# Coupling with the Mars PCM / MCD

- `Interface_KRC_MCD_PCM.dv` -- the coupling script.
- `KRC_MCD_PCM_DDD.pdf` -- design document (database construction, coupling
  interface, validation, user tutorial).
- `KRC_MCD_PCM_tutorial.pdf` -- two-page quick-start.
- `validation/` -- TES cross-validation of KRC runs driven by prescribed
  MCD fluxes (see its own README).

Unchanged by the four modifications documented in
`../Interface_modifications.pdf`; kept here because it shares the same
interface file (`UserPrescribed_Fluxes` argument of `krc()`).
