# Joint fit validation: effect on KRC surface temperature

Compares KRC surface temperature with the T-dependent emissivity and/or
T-dependent conductivity coefficients (from `Compute_Fit_PropIcyRegolith_4KRC.dvrc`)
switched on/off independently (`tsurf_*_noTdep/_noemissT/_emissT/_allTdep.tab`),
for two grain-size cases ("a" and "c"). Plots: `effect_emissT_onTsurf.png`,
`GJKRBB_*.png`, `WBB_*.png` (two conductivity mixing models compared).
`Effect_Tdependant_Temperature.sh` reproduces the runs;
`CheckFitMerge_effectoemissT on K.py` checks the fit.
