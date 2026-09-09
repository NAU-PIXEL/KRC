# MCD-prescribed fluxes vs. TES

KRC runs driven by UserPrescribed_Fluxes (MCD-derived VIS/IR fluxes) compared
against TES-derived surface temperature and albedo/thermal-inertia maps, split
by local time.

- `day/`, `night/` -- one Davinci driver (`maincheck.dv`) and analysis
  (`analyse.py`, `process.py`, `main.sh`) each, against `Tsurf_day.txt` /
  `Tsurf_night.txt` and the same `MCS_TI_regrid_2x10.hdf` /
  `TES_Alb_regrid_2x10.hdf` input maps.
