# Day-side TES validation

`maincheck.dv` runs KRC with prescribed MCD fluxes at each grid point;
`process.py` then `analyse.py` compare the result to `Tsurf_day.txt` using the
TES albedo/thermal-inertia maps as input. Driven by `main.sh`.
