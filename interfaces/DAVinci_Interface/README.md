# Davinci interface (not part of the KRC git repository)

- `krc_davinciinterface.dvrc` -- the modified interface (function `krc()`,
  `krc_evalN1`, `Mat_Prop`, etc.).
- `Interface_modifications.tex` / `.pdf` -- write-up of the four modifications
  in this bundle (RADGND, EmisT, adaptive FLAY, ice thermal properties).
- `CouplingwithMCD_PCM/` -- prescribing MCD/PCM atmospheric fluxes in KRC
  (`UserPrescribed_Fluxes`, card 8/26): design doc, tutorial, and TES
  validation.
- `Improve_IceProperties/` -- temperature-dependent thermal conductivity,
  specific heat and (optionally, jointly) emissivity for water ice, feeding
  KRC's T-dependent conductivity card (12).
