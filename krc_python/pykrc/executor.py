"""Execution engine for running KRC Fortran binary."""

import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import Dict, Any, Optional, List
import platform


class KRCExecutor:
    """Manages execution of KRC Fortran binary."""

    def __init__(self, krc_home: Path):
        """
        Initialize KRC executor.

        Parameters
        ----------
        krc_home : Path
            Path to KRC installation directory
        """
        self.krc_home = Path(krc_home)
        self.src_dir = self.krc_home / "src"
        self.support_dir = self.krc_home / "krc_support"

    def find_krc_executable(self, program_name: str = "krc") -> Path:
        """
        Find the KRC executable in the src directory.

        Parameters
        ----------
        program_name : str, optional
            Name of the KRC program, default "krc"

        Returns
        -------
        Path
            Path to the executable

        Raises
        ------
        FileNotFoundError
            If executable not found
        """
        # Try common executable names
        candidates = [
            self.src_dir / program_name,
            self.src_dir / f"{program_name}.x",
            self.src_dir / f"{program_name}.exe",
        ]

        for exe_path in candidates:
            if exe_path.exists():
                return exe_path

        raise FileNotFoundError(
            f"KRC executable not found in {self.src_dir}. "
            "Please build KRC first using 'make' in the src directory."
        )

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

            # Orbital parameters (if present)
            # TODO: Add porb parameters

            # Disk file specification
            f.write(f"8 5 0 './outdata.bin.{params.get('K4OUT', 52)}' / Disk file\n")
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
        # Write header
        header = "    " + "    ".join(f"{name:>9}" for name in param_names)
        f.write(header + "\n")

        # Write values
        values = []
        for name in param_names:
            val = params.get(name, 0)
            if is_int:
                values.append(f"{int(val):>9}")
            else:
                values.append(f"{float(val):>9.2f}")

        value_line = "    " + "    ".join(values)
        if end_marker:
            value_line += "                                       end"
        f.write(value_line + "\n")

    def _write_logical_flags(self, f, params: Dict[str, Any]):
        """Write logical flag lines."""
        lp_names = ["LP1", "LP2", "LP3", "LP4", "LP5", "LP6",
                    "LPGLOB", "LVFA", "LVFT", "LKofT"]
        lporb_names = ["LPORB", "LKEY", "LSC", "LZONE", "LOCAL",
                       "Prt76", "LPTAVE", "Prt78", "Prt79", "L_ONE"]

        # Write headers and values for LP flags
        f.write("   " + "   ".join(f"{name:>6}" for name in lp_names) + "\n")
        lp_vals = "   " + "   ".join(
            "T" if params.get(name, False) else "F" for name in lp_names
        )
        f.write(lp_vals + "\n")

        # Write headers and values for LPORB flags
        f.write(" " + "  ".join(f"{name:>6}" for name in lporb_names) + "\n")
        lporb_vals = "  " + "  ".join(
            "T" if params.get(name, False) else "F" for name in lporb_names
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

    def run_krc(
        self,
        params: Dict[str, Any],
        workdir: Optional[Path] = None,
        verbose: bool = False,
        program: str = "krc"
    ) -> Dict[str, Any]:
        """
        Run KRC with given parameters.

        Parameters
        ----------
        params : dict
            KRC input parameters
        workdir : Path, optional
            Working directory (creates temp dir if None)
        verbose : bool, optional
            Print execution output
        program : str, optional
            KRC program name

        Returns
        -------
        dict
            Dictionary with:
            - success: bool
            - workdir: Path to working directory
            - stdout: str
            - stderr: str
            - output_file: Path to output file

        Raises
        ------
        FileNotFoundError
            If KRC executable not found
        RuntimeError
            If KRC execution fails
        """
        # Find executable
        krc_exe = self.find_krc_executable(program)

        # Create working directory
        if workdir is None:
            workdir = Path(tempfile.mkdtemp(prefix="krc_"))
        else:
            workdir = Path(workdir)
            workdir.mkdir(parents=True, exist_ok=True)

        # Create input file
        input_file = self.create_input_file(workdir, params)

        # Create symlink to executable (or copy on Windows)
        exe_link = workdir / krc_exe.name
        if platform.system() == "Windows":
            shutil.copy(krc_exe, exe_link)
        else:
            if exe_link.exists():
                exe_link.unlink()
            exe_link.symlink_to(krc_exe)

        # Make executable on Unix
        if platform.system() != "Windows":
            exe_link.chmod(0o755)

        # Prepare fake input file (interactive prompts)
        fake_input_file = self.support_dir / "fake_krc344"
        if not fake_input_file.exists():
            # Create a minimal fake input if not found
            fake_input_file = workdir / "fake_input"
            with open(fake_input_file, 'w') as f:
                f.write("krc.inp\n")  # Input filename
                f.write("outdata\n")  # Output base name

        # Run KRC
        try:
            with open(fake_input_file, 'r') as stdin_file:
                result = subprocess.run(
                    [str(exe_link)],
                    cwd=workdir,
                    stdin=stdin_file,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    timeout=300  # 5 minute timeout
                )

            if verbose:
                print(result.stdout)
                if result.stderr:
                    print("STDERR:", result.stderr)

            # Check for output file
            k4out = params.get("K4OUT", 52)
            output_file = workdir / f"outdata.bin.{k4out}"

            if not output_file.exists():
                raise RuntimeError(
                    f"KRC failed to produce output file: {output_file}\n"
                    f"STDOUT: {result.stdout}\n"
                    f"STDERR: {result.stderr}"
                )

            return {
                "success": True,
                "workdir": workdir,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "output_file": output_file,
                "returncode": result.returncode,
            }

        except subprocess.TimeoutExpired:
            raise RuntimeError("KRC execution timed out after 5 minutes")
        except Exception as e:
            raise RuntimeError(f"KRC execution failed: {e}")
