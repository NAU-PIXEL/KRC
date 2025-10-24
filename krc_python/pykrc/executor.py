"""Execution engine for running KRC Fortran binary."""

import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import Dict, Any, Optional, List
import platform
import numpy as np

from .config import get_krc_executable, get_paths
from .defaults import (
    MASTER_INP_HEADER_DEFAULTS as MASTER_INP_DEFAULTS,
    PORB_TOUCHED_PARAMS
)
from .materials import calculate_material_properties
from .numerical import calculate_numerical_parameters


# Formatted strings from master.inp - preserves exact Fortran formatting
# These are used when writing header values that match master.inp defaults
MASTER_INP_FORMATS = {
    "ALBEDO": "       .25", "EMISS": "      1.00", "INERTIA": "     200.0", "COND2": "      2.77",
    "DENS2": "     928.0", "PERIOD": "    1.0275", "SPEC_HEAT": "      647.", "DENSITY": "     1600.",
    "CABR": "      0.11", "AMW": "      43.5", "SatPrA": "   27.9546", "PTOTAL": "     546.0",
    "FANON": "      .055", "TATM": "      200.", "TDEEP": "     180.0", "SpHeat2": "     1711.",
    "TAUD": "       0.3", "DUSTA": "       .90", "TAURAT": "      0.25", "TWILI": "       0.0",
    "ARC2_G0": "       0.5", "ARC3_Safe": "     0.801", "SLOPE": "       0.0", "SLOAZI": "       90.",
    "TFROST": "     146.0", "CFROST": "   589944.", "AFROST": "       .65", "FEMIS": "      0.95",
    "AF1": "      0.54", "AF2": "    0.0009", "FROEXT": "       50.", "SatPrB": "   3182.48",
    "RLAY": "    1.1500", "FLAY": "      .100", "CONVF": "       3.0", "DEPTH": "       0.0",
    "DRSET": "       0.0", "PhotoFunc": "       0.0", "GGT": "       0.1", "DTMAX": "       0.1",
    "DJUL": "  -1222.69", "DELJUL": " 17.174822", "SOLARDEC": "      00.0", "DAU": "     1.465",
    "LsubS": "        .0", "SOLCON": "     1368.", "GRAV": "     3.727", "Atm_Cp": "     735.9",
    "ConUp0": "  0.038640", "ConUp1": " -0.002145", "ConUp2": "  0.002347", "ConUp3": " -0.000750",
    "ConLo0": "  2.766722", "ConLo1": " -1.298966", "ConLo2": "  0.629224", "ConLo3": " -0.527291",
    "SphUp0": "  646.6275", "SphUp1": "  246.6678", "SphUp2": "  -49.8216", "SphUp3": "    7.9520",
    "SphLo0": "  1710.648", "SphLo1": "  721.8740", "SphLo2": "  57.44873", "SphLo3": "  24.37532",
}


class KRCExecutor:
    """Manages execution of KRC Fortran binary."""

    def __init__(self, krc_home: Optional[Path] = None):
        """
        Initialize KRC executor.

        Parameters
        ----------
        krc_home : Path, optional
            Path to KRC installation directory (uses config if None)
        """
        if krc_home is not None:
            self.krc_home = Path(krc_home)
        else:
            from .config import get_krc_home
            self.krc_home = get_krc_home()

        self.paths = get_paths()
        self.krc_exe = get_krc_executable()

    def setup_working_directory(self, workdir: Path) -> None:
        """
        Copy required data files to working directory.

        Parameters
        ----------
        workdir : Path
            Working directory to set up

        Raises
        ------
        FileNotFoundError
            If required files are missing
        """
        # Required files when LPORB=T
        required_files = ["standish.tab", "spinaxis.tab", "PORBCM.mat"]

        for filename in required_files:
            src_file = self.paths.support_dir / filename
            dest_file = workdir / filename

            if not src_file.exists():
                raise FileNotFoundError(
                    f"Required file not found: {src_file}\n"
                    f"KRC needs this file when LPORB=T (orbital calculations enabled)"
                )

            shutil.copy(src_file, dest_file)

    def create_input_file(
        self,
        workdir: Path,
        params: Dict[str, Any],
        filename: str = "krc.inp",
        user_params: Optional[Dict[str, Any]] = None
    ) -> Path:
        """
        Create KRC input file from parameters.

        Parameters
        ----------
        workdir : Path
            Working directory for the run
        params : dict
            Dictionary of KRC parameters
        user_params : dict, optional
            Parameters explicitly set by user (for changecard filtering)
        filename : str
            Input filename

        Returns
        -------
        Path
            Path to created input file
        """
        input_path = workdir / filename

        with open(input_path, 'w') as f:
            # Write KOLD and KEEP header
            # Per Davinci krc.dvrc line 231, always write 0 0 regardless of actual values
            # Actual KEEP value is handled via changecards
            f.write("0 0 / KOLD: season to start with;  KEEP: continue saving data in same disk file\n")
            f.write("Version 356 default values.  19 latitudes with mean Mars zonal elevations\n")

            # Write parameter blocks (8 values per line) - ALWAYS use master.inp defaults
            # The actual values will be in changecards if they differ
            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "ALBEDO", "EMISS", "INERTIA", "COND2", "DENS2",
                "PERIOD", "SPEC_HEAT", "DENSITY"
            ])

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "CABR", "AMW", "SatPrA", "PTOTAL", "FANON",
                "TATM", "TDEEP", "SpHeat2"
            ])

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "TAUD", "DUSTA", "TAURAT", "TWILI", "ARC2_G0",
                "ARC3_Safe", "SLOPE", "SLOAZI"
            ])

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "TFROST", "CFROST", "AFROST", "FEMIS", "AF1",
                "AF2", "FROEXT", "SatPrB"
            ])

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "RLAY", "FLAY", "CONVF", "DEPTH", "DRSET",
                "PhotoFunc", "GGT", "DTMAX"
            ])

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "DJUL", "DELJUL", "SOLARDEC", "DAU", "LsubS",
                "SOLCON", "GRAV", "Atm_Cp"
            ])

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "ConUp0", "ConUp1", "ConUp2", "ConUp3",
                "ConLo0", "ConLo1", "ConLo2", "ConLo3"
            ])

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "SphUp0", "SphUp1", "SphUp2", "SphUp3",
                "SphLo0", "SphLo1", "SphLo2", "SphLo3"
            ])

            # Integer parameters - SPECIAL HANDLING FOR N4
            # Per Davinci krc.dvrc lines 1020-1022, N4 must be written directly to header
            # (not via changecard) because KRC checks N4 before processing changecards
            n4_actual = len(params.get('Latitudes', [0.0]))

            # Create modified defaults with actual N4 value
            int_params_with_n4 = MASTER_INP_DEFAULTS.copy()
            int_params_with_n4['N4'] = n4_actual

            self._write_param_line(f, int_params_with_n4, [
                "N1", "N2", "N3", "N4", "N5", "N24", "IIB", "IC2"
            ], is_int=True)

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "NRSET", "NMHA", "NRUN", "JDISK", "IDOWN",
                "FlxP14", "TUN_Flx15", "KPREF"
            ], is_int=True)

            self._write_param_line(f, MASTER_INP_DEFAULTS, [
                "K4OUT", "JBARE", "Notif", "IDISK2"
            ], is_int=True, end_marker=True)

            # Logical flags - use master.inp defaults for header
            self._write_logical_flags(f, MASTER_INP_DEFAULTS)

            # Latitudes and elevations
            self._write_latitudes(f, params)
            self._write_elevations(f, params)

            # Orbital parameters (if LPORB=T)
            if params.get('LPORB', True):
                # Write PORB header line
                # Per Davinci krc.dvrc, format: <timestamp> IPLAN AND TC= <iplan> <tc> <body>:<body>
                porb_header = params.get('PORB_HEADER')
                if not porb_header:
                    # Generate dynamic header
                    import datetime
                    now = datetime.datetime.now()
                    timestamp = now.strftime("%Y %b %d %H:%M:%S")
                    # Extract IPLAN and TC from PORB_PARAMS if available
                    porb_params_list = params.get('PORB_PARAMS')
                    if porb_params_list and len(porb_params_list) >= 2:
                        iplan = porb_params_list[0]
                        tc = porb_params_list[1]
                    else:
                        iplan = 104.0  # Default Mars
                        tc = 0.1
                    body = params.get('body', 'Mars')
                    porb_header = f" {timestamp}=RUNTIME.  IPLAN AND TC= {iplan:.1f} {tc:.5f} {body}:{body}"
                f.write(porb_header + '\n')

                # Write PORB parameters
                # Prefer pre-formatted text from HDF (exact davinci parity)
                # Fallback to G15.7 formatting for generic/custom bodies
                porb_text_lines = params.get('PORB_TEXT_LINES')
                if porb_text_lines is not None:
                    # Use pre-formatted text from HDF (guaranteed exact match with davinci)
                    for line in porb_text_lines:
                        f.write(line + '\n')
                else:
                    # Fallback: format numeric values using G15.7 (for generic bodies)
                    porb_params = params.get('PORB_PARAMS')
                    if porb_params is not None:
                        for i in range(0, len(porb_params), 5):
                            chunk = porb_params[i:i+5]
                            formatted_vals = []
                            for val in chunk:
                                formatted_vals.append(self._format_g15_7(val))
                            line = ''.join(formatted_vals) + '    '  # 4 trailing spaces
                            f.write(line + '\n')

            # Type 8, index 25: Zone file (if LZONE enabled) - must come right after orbital parameters
            # Per Davinci krc.dvrc lines 899-905
            if params.get('LZONE') == True or params.get('LZONE') == 'T':
                # Check if we have an external zone file (Mode 4) or generated zone data (Mode 3)
                if '_external_zonefile' in params:
                    # Mode 4: Copy user's external zone file to working directory
                    import shutil
                    from pathlib import Path
                    external_path = Path(params['_external_zonefile'])
                    zone_file = workdir / 'zonefile.tab'
                    shutil.copy(external_path, zone_file)
                    # Write Type 8 changecard pointing to zone file
                    f.write(f"8 25 0 '{zone_file.name}' /\n")
                elif '_zone_data' in params:
                    # Mode 3: Generate zone file from calculated exponential profile
                    zone_file = self._write_zone_file(workdir, params['_zone_data'])
                    # Write Type 8 changecard pointing to zone file
                    f.write(f"8 25 0 '{zone_file.name}' /\n")

            # Check if in point mode (T→TI inversion)
            point_mode = params.get('_point_mode', False)

            if point_mode:
                # ========== POINT MODE: MULTI-CASE CHANGECARDS ==========
                # Per Davinci krc.dvrc lines 1232-1342, generate one case per TI value
                ti_table = params.get('_ti_lookup_table', np.array([]))

                # First, write base changecards for parameters that don't vary per case
                # This includes all the standard parameters except those that will
                # be written in the per-case section
                base_params = params.copy()

                # Remove parameters that will be written per-case
                # These get written in _write_point_mode_changecards
                per_case_params = {'INERTIA', 'DENSITY', 'N2', 'N1', 'IC2',
                                  'FLAY', 'RLAY', 'ConUp0', 'ConUp1', 'ConUp2', 'ConUp3'}
                for param in per_case_params:
                    base_params.pop(param, None)

                # Write base changecards
                self._write_changecards(f, base_params, workdir, user_params=user_params)

                # Disk file specification for multi-case run
                # This comes AFTER base changecards but BEFORE the cases
                k4out = params.get('K4OUT', 52)
                output_filename = f"./{filename.replace('.inp', f'.t{k4out}')}"
                f.write(f"8 5 0 '{output_filename}' /\n")

                # Calculate all material properties for all TI values at once (vectorized)
                # This is much more efficient than recalculating for each TI
                ti_properties = self._calculate_point_mode_properties(ti_table, params)

                # For each TI value, write a complete set of changecards
                for ti_idx, ti_value in enumerate(ti_table):
                    # Create modified params for this TI value
                    ti_params = params.copy()
                    ti_params['INERTIA'] = ti_value

                    # Per Davinci krc.dvrc line 1313: Force IC2=999 for one-point mode
                    ti_params['IC2'] = 999

                    # Update with vectorized material properties for this TI
                    # Extract the values for this specific TI index
                    ti_params['DENSITY'] = ti_properties['DENSITY'][ti_idx]
                    ti_params['SPEC_HEAT'] = ti_properties['SPEC_HEAT'][ti_idx]
                    ti_params['N1'] = ti_properties['N1'][ti_idx]
                    ti_params['N2'] = ti_properties['N2'][ti_idx]
                    ti_params['ConUp0'] = ti_properties['ConUp0'][ti_idx]
                    ti_params['ConUp1'] = ti_properties['ConUp1'][ti_idx]
                    ti_params['ConUp2'] = ti_properties['ConUp2'][ti_idx]
                    ti_params['ConUp3'] = ti_properties['ConUp3'][ti_idx]
                    ti_params['SphUp0'] = ti_properties['SphUp0'][ti_idx]
                    ti_params['SphUp1'] = ti_properties['SphUp1'][ti_idx]
                    ti_params['SphUp2'] = ti_properties['SphUp2'][ti_idx]
                    ti_params['SphUp3'] = ti_properties['SphUp3'][ti_idx]
                    ti_params['FLAY'] = ti_properties['FLAY'][ti_idx]
                    ti_params['RLAY'] = ti_properties['RLAY'][ti_idx]

                    # LKofT determines whether to use T-dependent conductivity
                    # Per Davinci lines 1319-1324
                    ti_params['LKofT'] = params.get('LKofT', True)

                    # Write ONLY the changecards that vary per case in point mode
                    # Per Davinci krc.dvrc lines 1319-1340
                    self._write_point_mode_changecards(f, ti_params, ti_idx)

                    # End this case with 0/
                    f.write("0/\n")

                # Final 0/ to end all cases
                f.write("0/\n")
            else:
                # ========== NORMAL MODE: SINGLE-CASE CHANGECARDS ==========
                # Write changecards for parameter overrides
                # This must come BEFORE the disk file specification (see krc.dvrc line 1092)
                self._write_changecards(f, params, workdir, user_params=user_params)

                # Disk file specification (change card type 8) - must come AFTER changecards
                k4out = params.get('K4OUT', 52)
                output_filename = f"./{filename.replace('.inp', f'.t{k4out}')}"
                f.write(f"8 5 0 '{output_filename}' /\n")

                # Type 15: Planetary flux (if PFlux enabled)
                if params.get('PFlux', False) or params.get('PFlux', 'F') == 'T':
                    self._write_planetary_flux_changecard(f, params)

                # Type 14: Eclipse (if Eclipse enabled)
                if params.get('Eclipse', 'F') == 'T':
                    self._write_eclipse_changecard(f, params)

                f.write("0/\n")
                f.write("0/\n")

        return input_path

    def _write_param_line(
        self,
        f,
        params: Dict[str, Any],
        param_names: List[str],
        is_int: bool = False,
        end_marker: bool = False
    ):
        """Write a parameter line to input file."""
        # Write header - 10 character fields, right-aligned
        header = "".join(f"{name:>10}" for name in param_names)
        if end_marker:
            header += "                                     end"
        f.write(header + "\n")

        # Write values - 10 character fields, right-aligned
        values = []
        for name in param_names:
            val = params.get(name, 0)
            if is_int:
                values.append(f"{int(val):>10}")
            else:
                # Check if this matches master.inp default and use pre-formatted string
                fval = float(val)
                default_val = MASTER_INP_DEFAULTS.get(name)

                if (default_val is not None and abs(fval - default_val) < 1e-10 and
                    name in MASTER_INP_FORMATS):
                    # Use exact master.inp formatting
                    formatted = MASTER_INP_FORMATS[name]
                else:
                    # Use g format for non-default values
                    formatted = f"{fval:>10.4g}"
                    # For values < 1, drop leading zero to match Fortran (.25 not 0.25)
                    if 0 < abs(fval) < 1:
                        formatted = formatted.replace(' 0.', '  .')

                values.append(formatted)

        value_line = "".join(values)
        if end_marker:
            value_line += "                                       0"
        f.write(value_line + "\n")

    def _format_g15_7(self, val: float) -> str:
        """
        Format a value using Fortran G15.7 format.

        Only used for generic/custom bodies without pre-formatted PORB text.
        For cached bodies, pre-formatted text is used directly from HDF files.

        G15.7 format:
        - 15 character field width
        - 7 significant figures
        - Auto-switches between F (fixed) and E (exponential) notation

        Parameters
        ----------
        val : float
            Value to format

        Returns
        -------
        str
            15-character formatted string matching Fortran G15.7 output
        """
        abs_val = abs(val)

        # E format: |val| < 0.1 (excluding zero) or |val| >= 1e8
        if (abs_val < 0.1 and abs_val > 0) or abs_val >= 1e8:
            return f"{val:15.7E}"

        # F format: 0.1 <= |val| < 1e8 or val == 0
        # Determine decimal places to maintain ~7 significant figures
        if abs_val < 1:
            # 0 <= val < 1 (including zero)
            decimals = 7
        elif abs_val < 10:
            decimals = 6
        elif abs_val < 100:
            decimals = 5
        elif abs_val < 1000:
            decimals = 4
        elif abs_val < 10000:
            decimals = 3
        elif abs_val < 100000:
            decimals = 2
        elif abs_val < 1000000:
            decimals = 1
        else:  # 1e6 <= abs_val < 1e8
            decimals = 0

        return f"{val:15.{decimals}f}"

    def _write_logical_flags(self, f, params: Dict[str, Any]):
        """Write logical flag lines."""
        lp_names = ["LP1", "LP2", "LP3", "LP4", "LP5", "LP6",
                    "LPGLOB", "LVFA", "LVFT", "LKofT"]
        lporb_names = ["LPORB", "LKEY", "LSC", "LZONE", "LOCAL",
                       "Prt76", "LPTAVE", "Prt78", "Prt79", "L_ONE"]

        # Write headers for LP flags (7 characters each, right-aligned)
        f.write("".join(f"{name:>7}" for name in lp_names) + "\n")
        # Write values for LP flags (7 characters each, right-aligned)
        lp_vals = "".join(
            f"{'T' if params.get(name, False) else 'F':>7}" for name in lp_names
        )
        f.write(lp_vals + "\n")

        # Write headers for LPORB flags (7 characters each, right-aligned)
        f.write("".join(f"{name:>7}" for name in lporb_names) + "\n")
        # Write values for LPORB flags (7 characters each, right-aligned)
        lporb_vals = "".join(
            f"{'T' if params.get(name, False) else 'F':>7}" for name in lporb_names
        )
        f.write(lporb_vals + "\n")

    def _write_latitudes(self, f, params: Dict[str, Any]):
        """Write latitude values."""
        lats = params.get("Latitudes", [0.0])
        # Match Davinci format: 7 placeholders on first line
        f.write("Latitudes: in 10F7.2  _____7 _____7 _____7 _____7 _____7 _____7 _____7\n")

        # Write in groups of 10
        for i in range(0, len(lats), 10):
            chunk = lats[i:i+10]
            lat_line = "".join(f"{lat:7.2f}" for lat in chunk)
            f.write(lat_line + "\n")

    def _write_elevations(self, f, params: Dict[str, Any]):
        """Write elevation values."""
        elevs = params.get("Elevations", [0.0])
        # Match Davinci format: 3 placeholders before, then label, then 4 after
        f.write(" _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7\n")

        # Write in groups of 10
        for i in range(0, len(elevs), 10):
            chunk = elevs[i:i+10]
            elev_line = "".join(f"{elev:7.2f}" for elev in chunk)
            f.write(elev_line + "\n")

    def _calculate_point_mode_properties(self, ti_array: np.ndarray, params: Dict[str, Any]) -> Dict[str, np.ndarray]:
        """
        Calculate material properties for all TI values in point mode.

        Uses NumPy broadcasting to efficiently compute properties for all
        thermal inertia values in the T→TI lookup table at once.

        Parameters
        ----------
        ti_array : np.ndarray
            Array of thermal inertia values to test
        params : dict
            Base parameters including Mat1, T_user, k_style, etc.

        Returns
        -------
        dict
            Dictionary with arrays of calculated properties:
            - DENSITY: array of densities
            - N1: array of layer counts
            - N2: array of timesteps
            - ConUp0-3: conductivity polynomial coefficients
            - SphUp0-3: heat capacity polynomial coefficients

        Notes
        -----
        Per Davinci krc.dvrc lines 1236-1290, recalculates:
        1. Porosity from INERTIA
        2. DENSITY from porosity and material properties
        3. COND from INERTIA²/(DENSITY×SPEC_HEAT)
        4. Conductivity k(T) polynomial coefficients
        5. N1 and N2 numerical parameters
        """
        # Get material and calculation parameters
        Mat1 = params.get('Mat1', 'basalt')
        T_user = params.get('T_user', 220.0)
        k_style = params.get('k_style', 'Mars')
        PERIOD = params.get('PERIOD', 1.026)
        DELJUL = params.get('DELJUL', 15.2665)
        N24 = params.get('N24', 48)
        N5 = params.get('N5', 135)
        JDISK = params.get('JDISK', 91)
        FLAY = params.get('FLAY', 0.1)
        RLAY = params.get('RLAY', 1.15)
        MAXN1 = params.get('MAXN1', 100)
        MAXN2 = params.get('MAXN2', 1000)

        # Vectorized calculations for all TI values
        # Per Davinci line 1244: Por1 = 0.60 * (2200. - INERTIA)/2200.
        porosity = np.maximum(0.0, np.minimum(1.0, 0.60 * (2200.0 - ti_array) / 2200.0))

        # Get material properties from database
        from .materials import get_material_properties

        try:
            mat_props = get_material_properties(Mat1)
        except ValueError:
            # Default to basalt if material not found
            mat_props = get_material_properties('basalt')

        # Extract density coefficients
        # Per Davinci line 1245:
        # DENSITY = (1 - Por1) * Mat_Prop.Dens.Dens0 + Mat_Prop.Dens.Dens1*X_user + ...
        Dens0 = mat_props.Dens0
        Dens1 = mat_props.Dens1
        Dens2 = mat_props.Dens2
        Dens3 = mat_props.Dens3

        # Extract specific heat coefficients
        SphUp0 = mat_props.Sph0
        SphUp1 = mat_props.Sph1
        SphUp2 = mat_props.Sph2
        SphUp3 = mat_props.Sph3

        # Calculate X_user for temperature-dependent properties
        X_user = (T_user - 220.0) * 0.01

        # Per Davinci line 1243: SPEC_HEAT at T_user
        SPEC_HEAT = SphUp0 + SphUp1*X_user + SphUp2*X_user**2 + SphUp3*X_user**3

        # Per Davinci line 1245: DENSITY with porosity correction and temperature polynomial
        # DENSITY = (1 - Por1) * Mat_Prop.Dens.Dens0 + Mat_Prop.Dens.Dens1*X_user +
        #           Mat_Prop.Dens.Dens2*X_user^2 + Mat_Prop.Dens.Dens3*X_user^3
        # Broadcasting: porosity is an array, others are scalars
        DENSITY = ((1 - porosity) * Dens0 +
                   Dens1 * X_user +
                   Dens2 * X_user**2 +
                   Dens3 * X_user**3)

        # Per Davinci line 1246: COND = INERTIA²/(DENSITY×SPEC_HEAT)
        COND = ti_array**2 / (DENSITY * SPEC_HEAT)

        # For k(T) polynomial fitting, we need to generate temperature table
        # Per Davinci, T_Tab from 80K to 478K
        T_Tab = np.arange(80, 480, 2)  # 80K to 478K in 2K steps
        X = (T_Tab - 220.0) * 0.01

        # Calculate conductivity for each TI value across temperature range
        # Result will be (n_ti, n_temps) array
        if k_style == 'Moon':
            # Per Davinci line 1249: Hayne et al. 2017
            # k_Table = COND*(1+2.7*((T_Tab-T_user)/350.)^3)
            # Broadcasting: COND is (n_ti,), T_Tab is (n_temps,)
            k_Table = COND[:, np.newaxis] * (1 + 2.7 * ((T_Tab - T_user) / 350.0)**3)
        elif k_style == 'Mars':
            # Per Davinci line 1252: Morgan et al. 2018
            # k_Table = COND*sqrt(T_Tab/T_user)
            k_Table = COND[:, np.newaxis] * np.sqrt(T_Tab / T_user)
        else:  # 'Bulk'
            # Would need material database for bulk conductivity polynomial
            # For now, use Mars style as default
            k_Table = COND[:, np.newaxis] * np.sqrt(T_Tab / T_user)

        # Fit cubic polynomial to k(T) for each TI value
        # We'll do this in a loop for now (polynomial fitting isn't easily vectorized)
        n_ti = len(ti_array)
        ConUp0 = np.zeros(n_ti)
        ConUp1 = np.zeros(n_ti)
        ConUp2 = np.zeros(n_ti)
        ConUp3 = np.zeros(n_ti)

        for i in range(n_ti):
            # Fit cubic polynomial: k = c0 + c1*X + c2*X² + c3*X³
            coeffs = np.polyfit(X, k_Table[i, :], 3)
            ConUp3[i] = coeffs[0]
            ConUp2[i] = coeffs[1]
            ConUp1[i] = coeffs[2]
            ConUp0[i] = coeffs[3]

        # Calculate N1 and N2 for each TI value
        # Import the functions from numerical module
        from .numerical import krc_evalN1, krc_evalN2

        N1 = np.zeros(n_ti, dtype=int)
        N2 = np.zeros(n_ti, dtype=int)
        FLAY_array = np.zeros(n_ti)
        IC2_array = np.zeros(n_ti, dtype=int)

        # Get parameters needed for N1/N2 calculations
        RLAY_param = params.get('RLAY', 1.15)
        FLAY_param = params.get('FLAY', 0.1)
        MAXN1 = params.get('MAXN1', 100)
        MAXN2 = params.get('MAXN2', 1000)

        # For point mode, always use IC2=999 (single layer)
        # Per Davinci krc.dvrc line 1313
        thick = 0.0  # Force single layer in point mode

        for i in range(n_ti):
            # Calculate N1 for this TI value
            # Per Davinci lines 1309-1311
            n1_result = krc_evalN1(
                RLAY=RLAY_param,
                FLAY=FLAY_param,
                INERTIA=ti_array[i],
                SPEC_HEAT=SPEC_HEAT,
                DENSITY=DENSITY[i],
                DELJUL=DELJUL,
                N5=N5,
                JDISK=JDISK,
                MAXN1=MAXN1,
                PERIOD=PERIOD,
                THICK=thick,  # Always 0 for point mode
                verbose=False
            )

            N1[i] = n1_result['N1']
            FLAY_array[i] = n1_result['FLAY']
            IC2_array[i] = 999  # Force single layer for point mode

            # Calculate N2 for this TI value
            # Per Davinci line 1318
            N2[i] = krc_evalN2(
                FLAY=FLAY_array[i],
                INERTIA=ti_array[i],
                DENSITY=DENSITY[i],
                SPEC_HEAT=SPEC_HEAT,
                PERIOD=PERIOD,
                N24=N24,
                MAXN2=MAXN2,
                verbose=False
            )

        return {
            'DENSITY': DENSITY,
            'SPEC_HEAT': np.full(n_ti, SPEC_HEAT),  # Same for all TI (depends on T_user, not TI)
            'N1': N1,
            'N2': N2,
            'ConUp0': ConUp0,
            'ConUp1': ConUp1,
            'ConUp2': ConUp2,
            'ConUp3': ConUp3,
            'SphUp0': np.full(n_ti, SphUp0),
            'SphUp1': np.full(n_ti, SphUp1),
            'SphUp2': np.full(n_ti, SphUp2),
            'SphUp3': np.full(n_ti, SphUp3),
            'FLAY': FLAY_array,  # May vary per TI due to N1 calculations
            'RLAY': np.full(n_ti, RLAY_param),
            'IC2': IC2_array,  # Always 999 for point mode
        }

    def _write_point_mode_changecards(self, f, ti_params: Dict[str, Any], ti_idx: int):
        """
        Write changecards for a single TI case in point mode.

        Per Davinci krc.dvrc lines 1319-1340, only write parameters that
        change per TI iteration:
        - INERTIA (varies per case)
        - DENSITY (varies with porosity)
        - N2 (varies with stability)
        - N1 (layer count)
        - IC2 (always 999 for point mode)
        - FLAY, RLAY (layer spacing)
        - ConUp0-3 (conductivity coefficients, zeroed if LKofT=F)

        Parameters
        ----------
        f : file handle
            Input file being written
        ti_params : dict
            Parameters for this specific TI value
        ti_idx : int
            Index of this TI case (0-based)
        """
        # Per Davinci lines 1321-1324, write parameters in specific order

        # 1. INERTIA - always written (Type 1, index 3)
        val = ti_params['INERTIA']
        if -1 < val < 1:
            f.write(f"1 3 {val:.3E} 'INERTIA' /\n")
        else:
            f.write(f"1 3 {val:.4f} 'INERTIA' /\n")

        # 2. DENSITY - always written (Type 1, index 8)
        val = ti_params['DENSITY']
        if -1 < val < 1:
            f.write(f"1 8 {val:.3E} 'DENSITY' /\n")
        else:
            f.write(f"1 8 {val:.4f} 'DENSITY' /\n")

        # 3. N2 - always written (Type 2, index 2)
        f.write(f"2 2 {int(ti_params['N2'])} 'N2' /\n")

        # 4. ConUp0-3 if LKofT is True, or zeros if False (Type 1, indices 49-52)
        # Per Davinci lines 1320-1324
        if ti_params.get('LKofT', True):
            # Write actual conductivity coefficients
            for idx, param in enumerate(['ConUp0', 'ConUp1', 'ConUp2', 'ConUp3']):
                i = 49 + idx  # ConUp0=49, ConUp1=50, ConUp2=51, ConUp3=52
                val = ti_params[param]
                if -1 < val < 1:
                    f.write(f"1 {i} {val:.3E} '{param}' /\n")
                else:
                    f.write(f"1 {i} {val:.4f} '{param}' /\n")
        else:
            # Write zeros for conductivity coefficients
            for idx, param in enumerate(['ConUp0', 'ConUp1', 'ConUp2', 'ConUp3']):
                i = 49 + idx  # ConUp0=49, ConUp1=50, ConUp2=51, ConUp3=52
                f.write(f"1 {i} 0.000E+00 '{param}' /\n")

        # 5. FLAY - always written (Type 1, index 34)
        val = ti_params['FLAY']
        if -1 < val < 1:
            f.write(f"1 34 {val:.3E} 'FLAY' /\n")
        else:
            f.write(f"1 34 {val:.4f} 'FLAY' /\n")

        # 6. RLAY - always written (Type 1, index 33)
        val = ti_params['RLAY']
        if -1 < val < 1:
            f.write(f"1 33 {val:.3E} 'RLAY' /\n")
        else:
            f.write(f"1 33 {val:.4f} 'RLAY' /\n")

        # 7. IC2 - always 999 for point mode (Type 2, index 8)
        f.write(f"2 8 999 'IC2' /\n")

        # 8. N1 - always written (Type 2, index 1)
        f.write(f"2 1 {int(ti_params['N1'])} 'N1' /\n")

    def _write_changecards(self, f, params: Dict[str, Any], workdir: Path, user_params: Optional[Dict[str, Any]] = None):
        """
        Write changecard directives to override parameters.

        Changecards allow runtime parameter modification following the
        punch card convention from KRC's early days. Format:
        <type> <index> <value> '<parameter_name>' /

        Type 1 = floating point (part1)
        Type 2 = integer (part2)
        Type 3 = boolean (part3)
        Type 8 = file path

        For 1-to-1 Davinci parity, writes changecards for ALL parameters that:
        - Differ from master.inp defaults, OR
        - Were touched by PORB (orbital parameters), OR
        - Were touched by internal logic (TPREDICT sets N3/NRSET/GGT), OR
        - Were explicitly set by user

        This matches Davinci's HasValue() behavior (krc.dvrc lines 1026-1089).
        """
        # Get PORB-touched and logic-touched parameters lists
        # Prefer params override if provided, otherwise use canonical set from defaults.py
        porb_touched = params.get('_porb_touched_params', PORB_TOUCHED_PARAMS.copy())
        logic_touched = params.get('_logic_touched_params', set())

        # Debug: Check if TDEEP and PhotoFunc are in porb_touched
        # print(f"DEBUG: porb_touched = {porb_touched}")
        # print(f"DEBUG: logic_touched = {logic_touched}")
        # print(f"DEBUG: TDEEP in porb_touched: {'TDEEP' in porb_touched}")
        # print(f"DEBUG: PhotoFunc in porb_touched: {'PhotoFunc' in porb_touched}")

        # Define parameter lists matching davinci's key.part1/part2/part3
        # These match the order in master.inp

        # Floating point parameters (Type 1) - order matters!
        float_params = [
            "ALBEDO", "EMISS", "INERTIA", "COND2", "DENS2", "PERIOD", "SPEC_HEAT", "DENSITY",
            "CABR", "AMW", "SatPrA", "PTOTAL", "FANON", "TATM", "TDEEP", "SpHeat2",
            "TAUD", "DUSTA", "TAURAT", "TWILI", "ARC2_G0", "ARC3_Safe", "SLOPE", "SLOAZI",
            "TFROST", "CFROST", "AFROST", "FEMIS", "AF1", "AF2", "FROEXT", "SatPrB",
            "RLAY", "FLAY", "CONVF", "DEPTH", "DRSET", "PhotoFunc", "GGT", "DTMAX",
            "DJUL", "DELJUL", "SOLARDEC", "DAU", "LsubS", "SOLCON", "GRAV", "Atm_Cp",
            "ConUp0", "ConUp1", "ConUp2", "ConUp3", "ConLo0", "ConLo1", "ConLo2", "ConLo3",
            "SphUp0", "SphUp1", "SphUp2", "SphUp3", "SphLo0", "SphLo1", "SphLo2", "SphLo3"
        ]

        # Integer parameters (Type 2) - order matters!
        int_params = [
            "N1", "N2", "N3", "N4", "N5", "N24", "IIB", "IC2",
            "NRSET", "NMHA", "NRUN", "JDISK", "IDOWN", "FlxP14", "TUN_Flx15", "KPREF",
            "K4OUT", "JBARE", "Notif", "IDISK2"
        ]

        # Boolean parameters (Type 3) - order matters!
        bool_params = [
            "LP1", "LP2", "LP3", "LP4", "LP5", "LP6", "LPGLOB", "LVFA", "LVFT", "LKofT",
            "LPORB", "LKEY", "LSC", "LZONE", "LOCAL", "Prt76", "LPTAVE", "Prt78", "Prt79", "L_ONE"
        ]

        # Write boolean changecards (Type 3) - for all params that differ from master.inp OR touched by PORB
        for i, param_name in enumerate(bool_params, start=1):
            # Write if parameter is in params dict (set by user OR calculated internally)
            if param_name in params:
                val = params[param_name]
                # Convert True/False or "T"/"F" to boolean for comparison
                if isinstance(val, bool):
                    bool_val = val
                elif isinstance(val, str):
                    bool_val = val.upper() == 'T'
                else:
                    bool_val = bool(val)

                # Write changecard if different from master.inp default OR touched by PORB/logic OR explicitly set by user
                master_default = MASTER_INP_DEFAULTS.get(param_name, False)
                if (bool_val != master_default or
                    param_name in porb_touched or
                    param_name in logic_touched or
                    (user_params and param_name in user_params)):
                    int_val = 1 if bool_val else 0
                    f.write(f"3 {i} {int_val} '{param_name}' /\n")

        # Write integer changecards (Type 2) - for all params that differ from master.inp OR touched by PORB/logic
        for i, param_name in enumerate(int_params, start=1):
            # Write if parameter is in params dict (set by user OR calculated internally)
            if param_name in params:
                # Skip N4 as davinci does (line 1037)
                if param_name == "N4":
                    continue

                # Skip KPREF unless it's in porb_touched (Mars only) or user-specified
                # Per Davinci krc.dvrc lines 367-369, KPREF is only set by PORB for Mars
                if param_name == "KPREF":
                    if (param_name not in porb_touched and
                        not (user_params and param_name in user_params)):
                        continue

                val = int(params[param_name])
                # Write if different from master.inp default OR touched by PORB/logic OR explicitly set by user
                if param_name in MASTER_INP_DEFAULTS:
                    if (val != MASTER_INP_DEFAULTS[param_name] or
                        param_name in porb_touched or
                        param_name in logic_touched or
                        (user_params and param_name in user_params)):
                        f.write(f"2 {i} {val} '{param_name}' /\n")
                else:
                    # No default, always write
                    f.write(f"2 {i} {val} '{param_name}' /\n")

        # Write float changecards (Type 1) - for all params that differ from master.inp OR touched by PORB/logic
        for i, param_name in enumerate(float_params, start=1):
            # Write if parameter is in params dict (set by user OR calculated internally)
            if param_name in params:
                # Skip None values (parameter not set)
                if params[param_name] is None:
                    continue
                val = float(params[param_name])

                # SPECIAL HANDLING FOR TFROST
                # Per Davinci krc.dvrc lines 546/582, TFROST changecard is ALWAYS written
                # (Davinci inconsistency: TFROST doesn't follow normal changecard rules)
                # Mars: TFROST=146.0, All other bodies: TFROST=0.0
                if param_name == "TFROST":
                    should_write = True
                # SPECIAL HANDLING FOR TDEEP
                # Per Davinci krc.dvrc lines 800-801, TDEEP should NOT be written when:
                # - IIB=0 (insulating boundary - no heat flow, no fixed temperature)
                # - IIB>0 (heat flux boundary - bottom heat flow in W/m², TDEEP not used)
                # TDEEP is ONLY meaningful when IIB<0 (fixed temperature boundary conditions)
                # Note: IIB may be float (e.g., 0.5 W/m²) but gets truncated to int in changecard
                elif param_name == "TDEEP" and params.get("IIB") is not None and params.get("IIB") >= 0:
                    should_write = False
                else:
                    # Write if different from master.inp default OR touched by PORB/logic OR explicitly set by user
                    should_write = False
                    if param_name in MASTER_INP_DEFAULTS:
                        # Compare with small tolerance for floating point
                        if (abs(val - MASTER_INP_DEFAULTS[param_name]) > 1e-6 or
                            param_name in porb_touched or
                            param_name in logic_touched or
                            (user_params and param_name in user_params)):
                            should_write = True
                    else:
                        # No default, always write
                        should_write = True

                if should_write:
                    # Check if this is a time-varying array (ALBEDO or TAUD)
                    # Per Davinci krc.dvrc lines 1055-1074
                    if param_name in ['ALBEDO', 'TAUD'] and isinstance(params[param_name], (list, tuple)):
                        # Time-varying array - write to file and use Type 8 changecard
                        if param_name == 'ALBEDO':
                            array_file = workdir / 'albfile.tab'
                            changecard_type = 22
                        else:  # TAUD
                            array_file = workdir / 'taufile.tab'
                            changecard_type = 23

                        # Write array to file
                        with open(array_file, 'w') as af:
                            for j, arr_val in enumerate(params[param_name]):
                                # Format: Ls value, parameter value
                                ls = j * 360.0 / len(params[param_name])
                                af.write(f"{ls:.2f}\t{arr_val:.2f}\n")

                        # Write Type 8 changecard
                        f.write(f"8 {changecard_type} 0 './{array_file.name}' /\n")
                    else:
                        # Regular scalar value
                        # Match Davinci logic (krc.dvrc line 1087): if(val<1 && val>-1) → use .3E
                        # This includes 0.0 (davinci writes 0.000E+00, not 0.0000)
                        if -1 < val < 1:
                            f.write(f"1 {i} {val:.3E} '{param_name}' /\n")
                        else:
                            f.write(f"1 {i} {val:.4f} '{param_name}' /\n")

    def _write_planetary_flux_changecard(self, f, params: Dict[str, Any]):
        """
        Write Type 15 changecard for planetary thermal flux (satellite modeling).

        Format: 15 BT_Avg BT_Min BT_Max Dis_AU Geom_alb Mut_Period Orb_Radius Radius Lon_Hr IR Vis /

        This is used when modeling satellites that receive thermal flux from their parent planet.
        """
        # Extract planetary flux parameters
        BT_Avg = params.get('BT_Avg', 0.0)
        BT_Min = params.get('BT_Min', 0.0)
        BT_Max = params.get('BT_Max', 0.0)
        Dis_AU = params.get('Dis_AU', 0.0)
        Geom_alb = params.get('Geom_alb', 0.0)
        Mut_Period = params.get('Mut_Period', 0.0)
        Orb_Radius = params.get('Orb_Radius', 0.0)
        Radius = params.get('Radius', 0.0)
        Lon_Hr = params.get('Lon_Hr', 12.0)
        IR = params.get('IR', 0.0)
        Vis = params.get('Vis', 0.0)

        # Write Type 15 changecard (matches davinci krc.dvrc line 1095-1101)
        f.write(f"15 {BT_Avg:.4f} {BT_Min:.4f} {BT_Max:.4f} {Dis_AU:.6f} "
                f"{Geom_alb:.4f} {Mut_Period:.6f} {Orb_Radius:.4f} {Radius:.4f} "
                f"{Lon_Hr:.4f} {IR:.4f} {Vis:.4f} /\n")

    def _write_zone_file(self, workdir: Path, zone_data: List[List[float]]) -> Path:
        """
        Generate zone file for exponential profiles or multi-layer tables.

        Per Davinci krc.dvrc lines 2104-2108, zone file has 4 columns:
        - Col 1: Layer thickness (m)
        - Col 2: Density (kg/m³)
        - Col 3: Conductivity (W/m-K) OR -1 for LKofT zones
        - Col 4: Specific heat (J/kg-K) OR zone index for LKofT

        Parameters
        ----------
        workdir : Path
            Working directory for input files
        zone_data : List[List[float]]
            List of [thickness, density, conductivity, specific_heat] for each layer

        Returns
        -------
        Path
            Path to generated zone.inp file

        Notes
        -----
        Zone file format per Davinci:
        C_END
        <thick> <density> <conductivity> <specific_heat>
        ...
        0 0 0 0
        """
        zone_file = workdir / 'zonefile.tab'

        with open(zone_file, 'w') as f:
            # Header line
            f.write("C_END\n")

            # Write each layer
            for row in zone_data:
                # Format: %.4f %.1f %.4f %.1f per Davinci line 2106
                f.write(f"{row[0]:.4f} {row[1]:.1f} {row[2]:.4f} {row[3]:.1f}\n")

            # End marker
            f.write("0 0 0 0\n")

        return zone_file

    def _write_eclipse_changecard(self, f, params: Dict[str, Any]):
        """
        Write Type 14 changecard for eclipse modeling (satellite eclipses).

        Format: 14 Eclipse_Style Eclipser Sun_Dis Eclipser_Rad Eclipsed_Rad CM Gamma Date /

        This is used when modeling satellites that experience eclipses from their parent planet.
        """
        # Check if user provided custom Eclipse_line
        if 'Eclipse_line' in params and params['Eclipse_line']:
            # Use custom line directly
            f.write(params['Eclipse_line'] + '\n')
        else:
            # Build Type 14 changecard from individual parameters
            Eclipse_Style = params.get('Eclipse_Style', 1.0)
            Eclipser = params.get('Eclipser', '')
            Sun_Dis = params.get('Sun_Dis', 0.0)
            Eclipser_Rad = params.get('Eclipser_Rad', 0.0)
            Eclipsed_Rad = params.get('Eclipsed_Rad', 0.0)
            CM = params.get('CM', 0.0)
            Gamma = params.get('Gamma', 0.0)
            Date = params.get('Date', '')

            # Write Type 14 changecard (matches davinci krc.dvrc line 1110-1112)
            # Format depends on Eclipse_Style:
            # Style 1.0 (daily): 14 1.0 'Eclipser' Sun_Dis Eclipser_Rad Eclipsed_Rad CM /
            # Style 2.0 (rare):  14 2.0 'Eclipser' Sun_Dis Eclipser_Rad Eclipsed_Rad Gamma 'Date' /
            if Eclipse_Style == 1.0:
                f.write(f"14 {Eclipse_Style:.1f} '{Eclipser}' {Sun_Dis:.4f} "
                        f"{Eclipser_Rad:.4f} {Eclipsed_Rad:.4f} {CM:.4f} /\n")
            elif Eclipse_Style == 2.0:
                f.write(f"14 {Eclipse_Style:.1f} '{Eclipser}' {Sun_Dis:.4f} "
                        f"{Eclipser_Rad:.4f} {Eclipsed_Rad:.4f} {Gamma:.4f} '{Date}' /\n")
            else:
                # Default to style 1.0
                f.write(f"14 1.0 '{Eclipser}' {Sun_Dis:.4f} "
                        f"{Eclipser_Rad:.4f} {Eclipsed_Rad:.4f} {CM:.4f} /\n")

    def run_krc(
        self,
        params: Dict[str, Any],
        workdir: Optional[Path] = None,
        basename: str = "krc",
        verbose: bool = False,
        timeout: int = 300,
        user_params: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run KRC with given parameters.

        Parameters
        ----------
        params : dict
            KRC input parameters
        workdir : Path, optional
            Working directory (creates temp dir if None)
        basename : str, optional
            Base name for input/output files (default "krc")
        verbose : bool, optional
            Print execution output
        timeout : int, optional
            Timeout in seconds (default 300 = 5 minutes)
        user_params : dict, optional
            Parameters explicitly set by user (for changecard filtering)

        Returns
        -------
        dict
            Dictionary with:
            - success: bool
            - workdir: Path to working directory
            - stdout: str
            - stderr: str
            - output_file: Path to output file
            - prt_file: Path to print file

        Raises
        ------
        FileNotFoundError
            If KRC executable not found
        RuntimeError
            If KRC execution fails
        """
        # Create working directory
        if workdir is None:
            workdir = Path(tempfile.mkdtemp(prefix="krc_"))
        else:
            workdir = Path(workdir)
            workdir.mkdir(parents=True, exist_ok=True)

        # Setup working directory with required data files
        self.setup_working_directory(workdir)

        # Create input file
        input_file = self.create_input_file(workdir, params, f"{basename}.inp", user_params=user_params)

        # KRC expects TWO LINES via stdin:
        # Line 1: input filename (without .inp extension)
        # Line 2: output filename (without .prt extension)
        stdin_input = f"{basename}\n{basename}\n"

        if verbose:
            print(f"Working directory: {workdir}")
            print(f"Input file: {input_file}")
            print(f"KRC executable: {self.krc_exe}")
            print(f"Stdin input: {repr(stdin_input)}")

        # Run KRC from working directory
        # Following davinci's approach: symlink KRC into workdir and run as ./krc
        try:
            # Create symlink to KRC executable in working directory
            krc_link = workdir / "krc"
            if krc_link.exists():
                krc_link.unlink()
            krc_link.symlink_to(self.krc_exe)

            # Run KRC as ./krc (like davinci does)
            result = subprocess.run(
                ["./krc"],
                input=stdin_input,
                capture_output=True,
                text=True,
                cwd=str(workdir),
                timeout=timeout
            )

            if verbose:
                print(f"Return code: {result.returncode}")
                print(f"STDOUT:\n{result.stdout}")
                if result.stderr:
                    print(f"STDERR:\n{result.stderr}")

            # Determine output file path
            k4out = params.get("K4OUT", 52)
            output_file = workdir / f"{basename}.t{k4out}"
            prt_file = workdir / f"{basename}.prt"

            # Check if output files were created
            if not output_file.exists() and k4out != 0:
                raise RuntimeError(
                    f"KRC failed to produce output file: {output_file}\n"
                    f"Return code: {result.returncode}\n"
                    f"STDOUT: {result.stdout}\n"
                    f"STDERR: {result.stderr}"
                )

            return {
                "success": result.returncode == 0,
                "returncode": result.returncode,
                "workdir": workdir,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "output_file": output_file if output_file.exists() else None,
                "prt_file": prt_file if prt_file.exists() else None,
            }

        except subprocess.TimeoutExpired:
            raise RuntimeError(f"KRC execution timed out after {timeout} seconds")
        except Exception as e:
            raise RuntimeError(f"KRC execution failed: {e}")
