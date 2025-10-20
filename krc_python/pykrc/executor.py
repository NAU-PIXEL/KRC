"""Execution engine for running KRC Fortran binary."""

import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import Dict, Any, Optional, List
import platform

from .config import get_krc_executable, get_paths
from .defaults import (
    MASTER_INP_HEADER_DEFAULTS as MASTER_INP_DEFAULTS,
    PORB_TOUCHED_PARAMS
)


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

        For 1-to-1 Davinci parity, writes changecards for ALL parameters that
        differ from master.inp defaults OR were touched by PORB/materials.
        This matches Davinci's HasValue() behavior (krc.dvrc lines 1026-1089).
        """
        # Get PORB-touched parameters list
        # Prefer params override if provided, otherwise use canonical set from defaults.py
        porb_touched = params.get('_porb_touched_params', PORB_TOUCHED_PARAMS.copy())

        # Debug: Check if TDEEP and PhotoFunc are in porb_touched
        # print(f"DEBUG: porb_touched = {porb_touched}")
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

                # Write changecard if different from master.inp default OR touched by PORB OR explicitly set by user
                master_default = MASTER_INP_DEFAULTS.get(param_name, False)
                if (bool_val != master_default or
                    param_name in porb_touched or
                    (user_params and param_name in user_params)):
                    int_val = 1 if bool_val else 0
                    f.write(f"3 {i} {int_val} '{param_name}' /\n")

        # Write integer changecards (Type 2) - for all params that differ from master.inp OR touched by PORB
        for i, param_name in enumerate(int_params, start=1):
            # Write if parameter is in params dict (set by user OR calculated internally)
            if param_name in params:
                # Skip N4 as davinci does (line 1037)
                if param_name == "N4":
                    continue
                val = int(params[param_name])
                # Write if different from master.inp default OR touched by PORB OR explicitly set by user
                if param_name in MASTER_INP_DEFAULTS:
                    if (val != MASTER_INP_DEFAULTS[param_name] or
                        param_name in porb_touched or
                        (user_params and param_name in user_params)):
                        f.write(f"2 {i} {val} '{param_name}' /\n")
                else:
                    # No default, always write
                    f.write(f"2 {i} {val} '{param_name}' /\n")

        # Write float changecards (Type 1) - for all params that differ from master.inp OR touched by PORB
        for i, param_name in enumerate(float_params, start=1):
            # Write if parameter is in params dict (set by user OR calculated internally)
            if param_name in params:
                # Skip None values (parameter not set)
                if params[param_name] is None:
                    continue
                val = float(params[param_name])
                # Write if different from master.inp default OR touched by PORB OR explicitly set by user
                should_write = False
                if param_name in MASTER_INP_DEFAULTS:
                    # Compare with small tolerance for floating point
                    if (abs(val - MASTER_INP_DEFAULTS[param_name]) > 1e-6 or
                        param_name in porb_touched or
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
