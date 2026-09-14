#!/usr/bin/env python
"""
process_GCM_4_krc.py

Merged pipeline: process.py (LTST reprojection + seasonal Ls projection)
followed directly, in memory, by convert.py's variable filter/rename and
HDF5 write for KRC. No intermediate .nc file is written to disk.

Usage:
    py process_GCM_4_krc.py input.nc output.hdf
    py process_GCM_4_krc.py input.nc output.hdf --grid1deg   # regrid to 1x1 deg

Pipeline:
  1. Local time: shift lon/15 h into fractional indices, PCHIP,
     periodic wrap. SW points whose interpolation straddles the
     terminator are flagged.
  2. Gap-filling of flagged points via periodic zonal interpolation
     from the proper longitudes at the same (time step, latitude).
  3. Alignment: roll so that index t <-> exact local time.
  4. Ls projection: Time_out = integer_Ls + local_hour/24 (360*npd pts),
     averaging sols within [Ls-0.5, Ls+0.5) at fixed local hour.
  5. (optional) Bilinear regrid onto a 1x1 deg lat/lon grid.
  6. Filter to the KRC-relevant variables, rename to KRC field names
     (fir, fvis, sensible), and write as HDF5 (h5netcdf engine, NetCDF4
     container, same layout convert.py used to produce).
"""

import sys
import numpy as np
import xarray as xr
from scipy.interpolate import PchipInterpolator

NPAD = 4
SW_NAME = "fluxsurf_dn_sw"
ZERO_TOL = 1e-10
FRAC_TOL = 1e-6

# GCM name -> KRC name (same renaming convert.py applied)
RENAME_MAP = {
    "fluxsurf_dn_sw": "fvis",
    "fluxsurf_lw": "fir",
    "sensibFlux": "sensible",
}
VARS_TO_KEEP = ["tsurf", "fluxsurf_lw", "sensibFlux", "fluxsurf_dn_sw",
                "Time", "latitude", "longitude"]


# ============================================================ step 1
def to_local_time(ds):
    time = ds["Time"].values.astype("float64")
    lon = ds["longitude"].values.astype("float64")
    n = time.size

    dt = float(np.median(np.diff(time)))
    npd = int(round(1.0 / dt))
    n_sol = n // npd
    offset = int(round((time[0] % 1.0) / dt))
    if n_sol * npd != n or abs(1.0 / dt - npd) > 1e-3:
        raise ValueError("Inconsistent Time axis (non-uniform sampling?)")
    print(f"[1] {npd} outputs/sol, {n_sol} sols, offset time[0] = "
          f"{offset} sample(s)")

    idx = np.arange(n, dtype="float64")
    idx_pad = np.arange(-NPAD, n + NPAD, dtype="float64")

    fields, sw_mask = {}, None
    for name, da in ds.data_vars.items():
        if da.dims != ("Time", "latitude", "longitude"):
            continue
        f = da.values
        new = np.empty_like(f)
        if name == SW_NAME:
            sw_mask = np.zeros(f.shape, dtype=bool)
        for ilon in range(lon.size):
            shift = lon[ilon] / 360.0 * npd
            q = (idx - shift) % n
            col = f[:, :, ilon]
            col_pad = np.concatenate([col[-NPAD:], col, col[:NPAD]], axis=0)
            new[:, :, ilon] = PchipInterpolator(idx_pad, col_pad, axis=0)(q)
            if name == SW_NAME:
                i0 = np.floor(q).astype(int) % n
                i1 = (i0 + 1) % n
                frac = q - np.floor(q)
                strad = (frac > FRAC_TOL) & (frac < 1.0 - FRAC_TOL)
                night0 = col[i0] <= ZERO_TOL
                night1 = col[i1] <= ZERO_TOL
                sw_mask[:, :, ilon] = strad[:, None] & (night0 ^ night1)
        fields[name] = new
        print(f"    {name} -> local time (pchip)")
    return fields, sw_mask, npd, n_sol, offset


# ============================================================ step 2
def fill_straddled(sw, mask, lon):
    rows = np.argwhere(mask.any(axis=2))
    for t, j in rows:
        m = mask[t, j]
        c = ~m
        xc, yc = lon[c], sw[t, j, c]
        x_ext = np.concatenate([xc - 360.0, xc, xc + 360.0])
        y_ext = np.tile(yc, 3)
        sw[t, j, m] = np.interp(lon[m], x_ext, y_ext)
    nz = int(mask.sum())
    print(f"[2] terminator: {nz} straddling points filled "
          f"({100.0 * nz / mask.size:.2f} %, {len(rows)} rows)")


# ============================================================ step 4
def project_to_ls(fields, ls_col_per_hour, npd, n_sol, n_lat, n_lon):
    ls_int = np.arange(360, dtype="float64")

    ls_unw = ls_col_per_hour.copy()
    for h in range(npd):
        col = ls_unw[:, h]
        col[1:] += 360.0 * np.cumsum(np.diff(col) < 0.0)
        ls_unw[:, h] = col

    def targets(ls_col):
        t = ls_int.copy()
        k = np.floor((ls_col[0] - t) / 360.0) + 1.0
        t = t + 360.0 * np.maximum(k, 0.0)
        t[t > ls_col[-1]] -= 360.0
        return t

    out = {name: np.empty((360, npd, n_lat, n_lon), dtype=f.dtype)
           for name, f in fields.items()}

    for h in range(npd):
        ls_col = ls_unw[:, h]
        tg = targets(ls_col)
        sels = [(ls_col >= tg[i] - 0.5) & (ls_col < tg[i] + 0.5)
                for i in range(360)]
        nn = np.clip(np.searchsorted(ls_col, tg), 0, n_sol - 1)
        for name, f in fields.items():
            fh = f.reshape(n_sol, npd, n_lat, n_lon)[:, h]
            oh = out[name][:, h]
            for i in range(360):
                oh[i] = fh[sels[i]].mean(axis=0) if sels[i].any() else fh[nn[i]]

    time_out = (ls_int[:, None] + np.arange(npd)[None, :] / npd).ravel()
    for name in out:
        out[name] = out[name].reshape(360 * npd, n_lat, n_lon)
    print(f"[4] Ls projection: {360 * npd} points")
    return out, time_out


# ============================================================ step 5 (optional regrid)
def lin_weights(src, dst):
    """1D linear interpolation weights (src in any order, sorted internally)."""
    order = np.argsort(src)
    s = src[order]
    j = np.clip(np.searchsorted(s, dst), 1, s.size - 1)
    w = np.clip((dst - s[j - 1]) / (s[j] - s[j - 1]), 0.0, 1.0)
    return order[j - 1], order[j], w.astype("float64")


def regrid_1deg(out_fields, lat, lon):
    lat_out = np.arange(-90.0, 90.0 + 0.5, 1.0)     # 181
    lon_out = np.arange(-180.0, 180.0 + 0.5, 1.0)   # 361
    ia, ib, wa = lin_weights(lat, lat_out)
    ja, jb, wb = lin_weights(lon, lon_out)
    print(f"[5] bilinear regrid -> {lat_out.size} x {lon_out.size}")

    regridded = {}
    for name, arr in out_fields.items():          # (T, nlat, nlon)
        blk = arr.astype("float64")
        blk = (blk[:, ia, :] * (1 - wa)[None, :, None]
               + blk[:, ib, :] * wa[None, :, None])
        blk = (blk[:, :, ja] * (1 - wb)[None, None, :]
               + blk[:, :, jb] * wb[None, None, :])
        regridded[name] = blk.astype(arr.dtype)
    return regridded, lat_out, lon_out


# ============================================================ step 6 (former convert.py)
def filter_rename_and_write_hdf(ds, output_path):
    """Keep the KRC-relevant variables, rename to KRC field names, write HDF5."""
    print(f"Writing output file: {output_path}...")

    present_vars = [v for v in VARS_TO_KEEP if v in ds.variables]
    ds_filtered = ds[present_vars]

    rename_map = {k: v for k, v in RENAME_MAP.items() if k in ds_filtered}
    ds_renamed = ds_filtered.rename(rename_map)

    ds_renamed.to_netcdf(
        output_path, engine="h5netcdf", invalid_netcdf=True, format="NETCDF4"
    )
    print("Conversion completed successfully! \U0001F389")


# ============================================================ main
def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    grid1deg = "--grid1deg" in sys.argv
    if len(args) != 2:
        sys.exit(f"Usage: py {sys.argv[0]} input.nc output.hdf [--grid1deg]")
    infile, outfile = args

    try:
        ds = xr.open_dataset(infile, decode_times=False)
    except FileNotFoundError:
        print(f"Error: Input file '{infile}' not found.")
        sys.exit(1)

    print(f"Reading file: {infile}...")
    lat = ds["latitude"].values.astype("float64")
    lon = ds["longitude"].values.astype("float64")

    # --- process.py stage ---
    fields, sw_mask, npd, n_sol, offset = to_local_time(ds)
    if SW_NAME in fields and sw_mask is not None:
        fill_straddled(fields[SW_NAME], sw_mask, lon)

    for name in fields:
        fields[name] = np.roll(fields[name], offset, axis=0)
    ls_rolled = np.roll(ds["Ls"].values.astype("float64") % 360.0, offset)
    print(f"[3] axis aligned (roll +{offset})")

    out_fields, time_out = project_to_ls(
        fields, ls_rolled.reshape(n_sol, npd), npd, n_sol, lat.size, lon.size)

    if grid1deg:
        out_fields, lat, lon = regrid_1deg(out_fields, lat, lon)

    # Build an in-memory Dataset with the same variable names process.py's
    # write_netcdf used to produce, so the convert step below can filter/
    # rename them exactly as it did when reading its own .nc input.
    ds_out = xr.Dataset(
        data_vars={name: (("Time", "latitude", "longitude"), arr)
                   for name, arr in out_fields.items()},
        coords={"Time": time_out, "latitude": lat, "longitude": lon},
    )
    ds_out["Time"].attrs.update(
        long_name="Ls + local_true_solar_time/24",
        units="Ls in degrees (integer part) ; local time = frac*24 (h)")

    # --- convert.py stage (in memory, no intermediate file) ---
    try:
        filter_rename_and_write_hdf(ds_out, outfile)
    except Exception as e:
        print(f"An error occurred during conversion: {e}")
        sys.exit(1)
    finally:
        ds.close()


if __name__ == "__main__":
    main()
