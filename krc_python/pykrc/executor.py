"""Execution engine for running KRC Fortran binary."""

import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import Dict, Any, Optional, List
import platform

from .config import get_krc_executable, get_paths


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

# Master.inp default values - these MUST be written to the input file header
# Only changecards should deviate from these values
MASTER_INP_DEFAULTS = {
    # Line 1: KOLD KEEP
    "KOLD": 0,
    "KEEP": 0,

    # Line 3-4: Material properties block 1
    "ALBEDO": 0.25,
    "EMISS": 1.00,
    "INERTIA": 200.0,
    "COND2": 2.77,
    "DENS2": 928.0,
    "PERIOD": 1.0275,
    "SPEC_HEAT": 647.0,
    "DENSITY": 1600.0,

    # Line 5-6: Atmosphere and thermal properties
    "CABR": 0.11,
    "AMW": 43.5,
    "SatPrA": 27.9546,
    "PTOTAL": 546.0,
    "FANON": 0.055,  # NOTE: NOT 0.3!
    "TATM": 200.0,
    "TDEEP": 180.0,
    "SpHeat2": 1711.0,

    # Line 7-8: Atmospheric opacity and scattering
    "TAUD": 0.3,
    "DUSTA": 0.90,
    "TAURAT": 0.25,  # NOTE: NOT 2.0!
    "TWILI": 0.0,
    "ARC2_G0": 0.5,
    "ARC3_Safe": 0.801,
    "SLOPE": 0.0,
    "SLOAZI": 90.0,

    # Line 9-10: Frost properties
    "TFROST": 146.0,
    "CFROST": 589944.0,
    "AFROST": 0.65,
    "FEMIS": 0.95,
    "AF1": 0.54,
    "AF2": 0.0009,
    "FROEXT": 50.0,
    "SatPrB": 3182.48,

    # Line 11-12: Layer and convergence
    "RLAY": 1.15,
    "FLAY": 0.10,
    "CONVF": 3.0,
    "DEPTH": 0.0,
    "DRSET": 0.0,
    "PhotoFunc": 0.0,
    "GGT": 0.1,
    "DTMAX": 0.1,

    # Line 13-14: Orbital and solar
    "DJUL": -1222.69,
    "DELJUL": 17.174822,
    "SOLARDEC": 0.0,
    "DAU": 1.465,
    "LsubS": 0.0,
    "SOLCON": 1368.0,
    "GRAV": 3.727,
    "Atm_Cp": 735.9,

    # Line 15-16: Conductivity polynomial coefficients (upper layer)
    "ConUp0": 0.038640,
    "ConUp1": -0.002145,
    "ConUp2": 0.002347,
    "ConUp3": -0.000750,
    "ConLo0": 2.766722,
    "ConLo1": -1.298966,
    "ConLo2": 0.629224,
    "ConLo3": -0.527291,

    # Line 17-18: Specific heat polynomial coefficients
    "SphUp0": 646.6275,
    "SphUp1": 246.6678,
    "SphUp2": -49.8216,
    "SphUp3": 7.9520,
    "SphLo0": 1710.648,
    "SphLo1": 721.8740,
    "SphLo2": 57.44873,
    "SphLo3": 24.37532,

    # Line 19-20: Grid parameters
    "N1": 28,
    "N2": 1536,
    "N3": 15,
    "N4": 19,
    "N5": 120,
    "N24": 48,
    "IIB": 0,
    "IC2": 999,

    # Line 21-22: Run control
    "NRSET": 3,
    "NMHA": 24,
    "NRUN": 0,
    "JDISK": 81,
    "IDOWN": 0,
    "FlxP14": 45,
    "TUN_Flx15": 65,
    "KPREF": 1,

    # Line 23-24: Output control
    "K4OUT": 52,
    "JBARE": 0,
    "Notif": 50,
    "IDISK2": 0,

    # Booleans (line 25-28)
    "LP1": False,
    "LP2": True,
    "LP3": False,
    "LP4": False,
    "LP5": False,
    "LP6": False,
    "LPGLOB": False,
    "LVFA": False,
    "LVFT": False,
    "LKofT": False,
    "LPORB": True,
    "LKEY": False,
    "LSC": False,
    "LZONE": False,
    "LOCAL": True,
    "Prt76": False,
    "LPTAVE": False,
    "Prt78": False,
    "Prt79": False,
    "L_ONE": False,
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

                # Write PORB parameters (30 values in 6 lines of 5G15.7 each)
                # Format: 5 values per line, each in a 15-character field
                # Fortran G format: uses E notation for |val| < 0.1 or |val| >= 1e8
                # Otherwise uses F format with 7 significant figures
                porb_params = params.get('PORB_PARAMS')
                if porb_params is not None:
                    for i in range(0, len(porb_params), 5):
                        chunk = porb_params[i:i+5]
                        formatted_vals = []
                        for val in chunk:
                            abs_val = abs(val)
                            # Fortran G15.7: uses E notation for |val| < 0.1 or |val| >= 1e8
                            if (abs_val < 0.1 and abs_val > 0) or abs_val >= 1e8:
                                # E format: 0.dddddddE±ee (d=7 digits after decimal)
                                formatted = f"{val:15.7E}"
                            else:
                                # F format with up to 7 significant figures
                                # Determine decimal places needed
                                if abs_val >= 1:
                                    # For values >= 1, show appropriate decimals
                                    if abs_val >= 1000:
                                        decimals = 3  # e.g., 3397.977
                                    elif abs_val >= 100:
                                        decimals = 4  # e.g., 101.0000
                                    elif abs_val >= 10:
                                        decimals = 5  # e.g., 24.62296
                                    else:
                                        decimals = 6  # e.g., 5.544402
                                else:
                                    # For 0.1 <= val < 1
                                    decimals = 7  # e.g., 0.4090926

                                formatted = f"{val:{15}.{decimals}f}"
                            formatted_vals.append(formatted)
                        line = ''.join(formatted_vals) + '    '
                        f.write(line + '\n')

            # Write changecards for parameter overrides
            # This must come BEFORE the disk file specification (see krc.dvrc line 1092)
            self._write_changecards(f, params, workdir, user_params=user_params)

            # Disk file specification (change card type 8) - must come AFTER changecards
            k4out = params.get('K4OUT', 52)
            output_filename = f"./{filename.replace('.inp', f'.t{k4out}')}"
            f.write(f"8 5 0 '{output_filename}' /\n")

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
        # Get PORB-touched parameters list (if provided)
        porb_touched = params.get('_porb_touched_params', set())

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

                # Write changecard if different from master.inp default OR touched by PORB
                master_default = MASTER_INP_DEFAULTS.get(param_name, False)
                if bool_val != master_default or param_name in porb_touched:
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
                # Write if different from master.inp default OR touched by PORB
                if param_name in MASTER_INP_DEFAULTS:
                    if val != MASTER_INP_DEFAULTS[param_name] or param_name in porb_touched:
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
                # Write if different from master.inp default OR touched by PORB
                should_write = False
                if param_name in MASTER_INP_DEFAULTS:
                    # Compare with small tolerance for floating point
                    if abs(val - MASTER_INP_DEFAULTS[param_name]) > 1e-6 or param_name in porb_touched:
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
                        # Use scientific notation for small values
                        if -1 < val < 1 and val != 0:
                            f.write(f"1 {i} {val:.3E} '{param_name}' /\n")
                        else:
                            f.write(f"1 {i} {val:.4f} '{param_name}' /\n")

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
