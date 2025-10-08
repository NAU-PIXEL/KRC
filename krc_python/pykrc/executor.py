"""Execution engine for running KRC Fortran binary."""

import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import Dict, Any, Optional, List
import platform

from .config import get_krc_executable, get_paths


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
        filename: str = "krc.inp"
    ) -> Path:
        """
        Create KRC input file from parameters.

        Parameters
        ----------
        workdir : Path
            Working directory for the run
        params : dict
            Dictionary of KRC parameters
        filename : str
            Input filename

        Returns
        -------
        Path
            Path to created input file
        """
        input_path = workdir / filename

        with open(input_path, 'w') as f:
            # Write KOLD and KEEP
            f.write(f"{params.get('KOLD', 0)} {params.get('KEEP', 0)} / KOLD KEEP\n")
            f.write("Python KRC Interface Generated Input\n")

            # Write parameter blocks (8 values per line)
            self._write_param_line(f, params, [
                "ALBEDO", "EMISS", "INERTIA", "COND2", "DENS2",
                "PERIOD", "SPEC_HEAT", "DENSITY"
            ])

            self._write_param_line(f, params, [
                "CABR", "AMW", "SatPrA", "PTOTAL", "FANON",
                "TATM", "TDEEP", "SpHeat2"
            ])

            self._write_param_line(f, params, [
                "TAUD", "DUSTA", "TAURAT", "TWILI", "ARC2_G0",
                "ARC3_Safe", "SLOPE", "SLOAZI"
            ])

            self._write_param_line(f, params, [
                "TFROST", "CFROST", "AFROST", "FEMIS", "AF1",
                "AF2", "FROEXT", "SatPrB"
            ])

            self._write_param_line(f, params, [
                "RLAY", "FLAY", "CONVF", "DEPTH", "DRSET",
                "PhotoFunc", "GGT", "DTMAX"
            ])

            self._write_param_line(f, params, [
                "DJUL", "DELJUL", "SOLARDEC", "DAU", "LsubS",
                "SOLCON", "GRAV", "Atm_Cp"
            ])

            self._write_param_line(f, params, [
                "ConUp0", "ConUp1", "ConUp2", "ConUp3",
                "ConLo0", "ConLo1", "ConLo2", "ConLo3"
            ])

            self._write_param_line(f, params, [
                "SphUp0", "SphUp1", "SphUp2", "SphUp3",
                "SphLo0", "SphLo1", "SphLo2", "SphLo3"
            ])

            # Integer parameters
            self._write_param_line(f, params, [
                "N1", "N2", "N3", "N4", "N5", "N24", "IIB", "IC2"
            ], is_int=True)

            self._write_param_line(f, params, [
                "NRSET", "NMHA", "NRUN", "JDISK", "IDOWN",
                "FlxP14", "TUN_Flx15", "KPREF"
            ], is_int=True)

            self._write_param_line(f, params, [
                "K4OUT", "JBARE", "Notif", "IDISK2"
            ], is_int=True, end_marker=True)

            # Logical flags
            self._write_logical_flags(f, params)

            # Latitudes and elevations
            self._write_latitudes(f, params)
            self._write_elevations(f, params)

            # Orbital parameters (if LPORB=T)
            if params.get('LPORB', True):
                # Write PORB header line if provided
                porb_header = params.get('PORB_HEADER',
                    ' 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars')
                f.write(porb_header + '\n')

                # Write PORB parameters (30 values in 6 lines of 5G15.7 each)
                # Format: 5 values per line, each in a 15-character field
                porb_params = params.get('PORB_PARAMS')
                if porb_params is not None:
                    for i in range(0, len(porb_params), 5):
                        chunk = porb_params[i:i+5]
                        # G15.7 format: 15 characters wide, 7 decimal places
                        line = ''.join(f"{val:15.7f}" for val in chunk)
                        f.write(line + '\n')

            # Disk file specification (change card type 8)
            k4out = params.get('K4OUT', 52)
            output_filename = f"./{filename.replace('.inp', f'.t{k4out}')}"
            f.write(f"8 5 0 '{output_filename}' / Disk file\n")

            # Write changecards for parameter overrides
            self._write_changecards(f, params, workdir)

            f.write("0/\n")
            f.write("0/ ======================= end of run\n")

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
        f.write(header + "\n")

        # Write values - 10 character fields, right-aligned
        values = []
        for name in param_names:
            val = params.get(name, 0)
            if is_int:
                values.append(f"{int(val):>10}")
            else:
                values.append(f"{float(val):>10.2f}")

        value_line = "".join(values)
        if end_marker:
            value_line += "                                       end"
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
        f.write("Latitudes: in 10F7.2  _____7 _____7 _____7\n")

        # Write in groups of 10
        for i in range(0, len(lats), 10):
            chunk = lats[i:i+10]
            lat_line = " ".join(f"{lat:>7.2f}" for lat in chunk)
            f.write(lat_line + "\n")

    def _write_elevations(self, f, params: Dict[str, Any]):
        """Write elevation values."""
        elevs = params.get("Elevations", [0.0])
        f.write("_____7 _____7 _____7 Elevations: in 10F7.2\n")

        # Write in groups of 10
        for i in range(0, len(elevs), 10):
            chunk = elevs[i:i+10]
            elev_line = " ".join(f"{elev:>7.2f}" for elev in chunk)
            f.write(elev_line + "\n")

    def _write_changecards(self, f, params: Dict[str, Any], workdir: Path):
        """
        Write changecard directives to override parameters.

        Changecards allow runtime parameter modification following the
        punch card convention from KRC's early days. Format:
        <type> <index> <value> '<parameter_name>' /

        Type 1 = floating point (part1)
        Type 2 = integer (part2)
        Type 3 = boolean (part3)
        Type 8 = file path
        """
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

        # Track which parameters to override (those set in params dict that differ from master.inp)
        # For now, we'll write changecards for key parameters that are commonly overridden

        # Write boolean changecards (Type 3)
        for i, param_name in enumerate(bool_params, start=1):
            if param_name in params:
                val = params[param_name]
                # Convert True/False or "T"/"F" to 1/0
                if isinstance(val, bool):
                    int_val = 1 if val else 0
                elif isinstance(val, str):
                    int_val = 1 if val.upper() == 'T' else 0
                else:
                    int_val = int(val)
                f.write(f"3 {i} {int_val} '{param_name}' /\n")

        # Write integer changecards (Type 2)
        for i, param_name in enumerate(int_params, start=1):
            if param_name in params:
                # Skip N4 as davinci does (line 1037)
                if param_name == "N4":
                    continue
                val = int(params[param_name])
                f.write(f"2 {i} {val} '{param_name}' /\n")

        # Write float changecards (Type 1)
        for i, param_name in enumerate(float_params, start=1):
            if param_name in params:
                val = float(params[param_name])
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
        timeout: int = 300
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
        input_file = self.create_input_file(workdir, params, f"{basename}.inp")

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
