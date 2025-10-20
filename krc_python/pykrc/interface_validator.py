"""
Validation framework for comparing PyKRC output with krc.dvrc (Davinci interface).

TESTING PHILOSOPHY:
This module enforces a two-tier validation hierarchy:

1. PRIMARY: Identical input file generation (test FIRST)
   - Input .inp files must match exactly (line-by-line)
   - This is the single most important success metric
   - Ensures Fortran receives identical instructions

2. SECONDARY: Nearly identical temperature outputs (test SECOND)
   - Temperature arrays must match in size and values (with tolerance)
   - Element-wise comparison allows minor precision/rounding differences
   - Typical tolerance: 0.01 K absolute, 1e-5 relative

Both metrics must pass for overall validation success.

Usage:
    from pykrc.interface_validator import KRCValidator

    validator = KRCValidator(
        krc_home="/path/to/krc",
        davinci_krc_path="/path/to/krc.dvrc"
    )

    # Run comparison
    result = validator.compare_run(
        pykrc_params={"lat": 12.0, "ls": 23.0, "INERTIA": 100.0},
        davinci_cmd='krc(lat=12.,ls=23.,INERTIA=100.)'
    )

    # Check results (two-tier validation)
    assert result["summary"]["input_files_identical"]  # PRIMARY
    assert result["summary"]["temperature_arrays_match"]  # SECONDARY
    assert result["summary"]["overall_pass"]  # Both must pass
"""

import subprocess
import tempfile
import difflib
import struct
import numpy as np
from pathlib import Path
from typing import Dict, Any, Optional, Tuple, List
import json
import re

from .core import krc
from .config import set_krc_home


class InputFileComparator:
    """
    Compare KRC input files (.inp) for exact equivalence.

    PRIMARY VALIDATION METRIC: Input files must match line-by-line.
    This ensures Fortran KRC receives identical instructions.
    """

    @staticmethod
    def compare_inp_files(file1: Path, file2: Path) -> Dict[str, Any]:
        """
        Compare two KRC .inp files with EXACT line-by-line matching.

        This is the PRIMARY validation metric - input files must match exactly
        to ensure Fortran receives identical instructions. No normalization
        is performed; files must be byte-for-byte identical.

        Args:
            file1: First .inp file path
            file2: Second .inp file path

        Returns:
            dict: Comparison results with:
                - identical: bool (True only if line-by-line identical)
                - differences: list of line-by-line differences
                - diff_text: unified diff showing exact mismatches
                - line_count_file1: number of lines in file1
                - line_count_file2: number of lines in file2
        """
        with open(file1) as f1, open(file2) as f2:
            lines1 = f1.readlines()
            lines2 = f2.readlines()

        identical = lines1 == lines2

        if not identical:
            diff = list(difflib.unified_diff(
                lines1, lines2,
                fromfile=str(file1),
                tofile=str(file2),
                lineterm=''
            ))
        else:
            diff = []

        return {
            "identical": identical,
            "differences": diff if not identical else [],
            "diff_text": "\n".join(diff) if diff else "",
            "line_count_file1": len(lines1),
            "line_count_file2": len(lines2)
        }


class BinaryFileComparator:
    """Compare KRC binary output files (t52, bin52)."""

    @staticmethod
    def compare_binary_files(file1: Path, file2: Path,
                            tolerance: float = 1e-6) -> Dict[str, Any]:
        """
        Compare two binary files byte-by-byte and value-by-value.

        Args:
            file1: First binary file path
            file2: Second binary file path
            tolerance: Tolerance for floating point comparison

        Returns:
            dict: Comparison results
        """
        # Check file sizes
        size1 = file1.stat().st_size
        size2 = file2.stat().st_size

        if size1 != size2:
            return {
                "identical": False,
                "size_match": False,
                "size1": size1,
                "size2": size2,
                "error": f"File sizes differ: {size1} vs {size2} bytes"
            }

        # Byte-by-byte comparison
        with open(file1, 'rb') as f1, open(file2, 'rb') as f2:
            bytes1 = f1.read()
            bytes2 = f2.read()

        byte_identical = bytes1 == bytes2

        if byte_identical:
            return {
                "identical": True,
                "size_match": True,
                "byte_identical": True,
                "size": size1
            }

        # If not byte-identical, check numerical differences
        differences = []
        for i, (b1, b2) in enumerate(zip(bytes1, bytes2)):
            if b1 != b2:
                differences.append({
                    "offset": i,
                    "byte1": b1,
                    "byte2": b2
                })

        return {
            "identical": False,
            "size_match": True,
            "byte_identical": False,
            "size": size1,
            "num_byte_differences": len(differences),
            "first_differences": differences[:10]  # Show first 10
        }

    @staticmethod
    def compare_float_arrays(file1: Path, file2: Path,
                            record_size: int = 4,
                            tolerance: float = 1e-6) -> Dict[str, Any]:
        """
        Compare binary files as arrays of floats.

        Args:
            file1: First binary file
            file2: Second binary file
            record_size: Size of each record in bytes (4 for float32)
            tolerance: Relative tolerance for comparison

        Returns:
            dict: Numerical comparison results
        """
        with open(file1, 'rb') as f1, open(file2, 'rb') as f2:
            data1 = np.fromfile(f1, dtype=np.float32)
            data2 = np.fromfile(f2, dtype=np.float32)

        if len(data1) != len(data2):
            return {
                "identical": False,
                "error": f"Array lengths differ: {len(data1)} vs {len(data2)}"
            }

        # Check for NaN/Inf
        valid1 = np.isfinite(data1)
        valid2 = np.isfinite(data2)

        if not np.array_equal(valid1, valid2):
            return {
                "identical": False,
                "error": "NaN/Inf patterns differ",
                "num_nan_file1": np.sum(~valid1),
                "num_nan_file2": np.sum(~valid2)
            }

        # Compare valid values
        valid_mask = valid1 & valid2
        diff = np.abs(data1[valid_mask] - data2[valid_mask])

        # Use the maximum of the two values as denominator to avoid division by near-zero
        # This prevents huge relative differences when both values are tiny
        denominator = np.maximum(np.abs(data1[valid_mask]), np.abs(data2[valid_mask]))
        # Add epsilon only where denominator is actually zero
        denominator = np.where(denominator > 0, denominator, 1e-10)
        rel_diff = diff / denominator

        max_diff = np.max(diff) if len(diff) > 0 else 0
        max_rel_diff = np.max(rel_diff) if len(rel_diff) > 0 else 0

        # Use absolute difference for comparison (tolerance is in absolute units)
        # For thermal models, absolute temperature/flux differences matter, not relative
        values_match = max_diff < tolerance

        return {
            "identical": values_match,
            "num_values": len(data1),
            "max_absolute_diff": float(max_diff),
            "max_relative_diff": float(max_rel_diff),
            "mean_absolute_diff": float(np.mean(diff)),
            "mean_relative_diff": float(np.mean(rel_diff)),
            "values_within_tolerance": values_match
        }


class OutputDataComparator:
    """
    Compare parsed KRC output data structures.

    SECONDARY VALIDATION METRIC: Temperature arrays must be nearly identical.
    """

    @staticmethod
    def compare_temperature_arrays(arr1: np.ndarray, arr2: np.ndarray,
                                   name: str = "surf_temp",
                                   atol: float = 0.01,
                                   rtol: float = 1e-5) -> Dict[str, Any]:
        """
        Compare temperature arrays with appropriate tolerances.

        This is the SECONDARY validation metric - temperature arrays should be
        nearly identical, allowing for minor precision/rounding differences.

        Args:
            arr1: First temperature array
            arr2: Second temperature array
            name: Array name (default: "surf_temp")
            atol: Absolute tolerance in Kelvin (default: 0.01 K)
            rtol: Relative tolerance (default: 1e-5)

        Returns:
            dict: Comparison results including match status and statistics
        """
        if arr1.shape != arr2.shape:
            return {
                "match": False,
                "name": name,
                "error": f"Array size mismatch: {arr1.shape} vs {arr2.shape}",
                "size1": arr1.shape,
                "size2": arr2.shape
            }

        # Element-wise comparison with tolerance
        match = np.allclose(arr1, arr2, atol=atol, rtol=rtol)
        diff = np.abs(arr1 - arr2)

        return {
            "match": match,
            "name": name,
            "shape": arr1.shape,
            "max_abs_diff_K": float(np.max(diff)),
            "mean_abs_diff_K": float(np.mean(diff)),
            "median_abs_diff_K": float(np.median(diff)),
            "num_elements": arr1.size,
            "tolerance_atol_K": atol,
            "tolerance_rtol": rtol
        }

    @staticmethod
    def compare_arrays(arr1: np.ndarray, arr2: np.ndarray,
                      name: str, tolerance: float = 1e-6) -> Dict[str, Any]:
        """
        Compare two numpy arrays (generic comparison).

        Note: For temperature arrays, prefer compare_temperature_arrays()
        which uses appropriate tolerances and units.
        """
        if arr1.shape != arr2.shape:
            return {
                "match": False,
                "name": name,
                "error": f"Shape mismatch: {arr1.shape} vs {arr2.shape}"
            }

        diff = np.abs(arr1 - arr2)

        # Use maximum of the two values as denominator to avoid division by near-zero
        denominator = np.maximum(np.abs(arr1), np.abs(arr2))
        denominator = np.where(denominator > 0, denominator, 1e-10)
        rel_diff = diff / denominator

        return {
            "match": np.max(diff) < tolerance,  # Use absolute difference
            "name": name,
            "shape": arr1.shape,
            "max_abs_diff": float(np.max(diff)),
            "max_rel_diff": float(np.max(rel_diff)),
            "mean_abs_diff": float(np.mean(diff)),
            "mean_rel_diff": float(np.mean(rel_diff))
        }

    @staticmethod
    def compare_outputs(output1: Dict[str, Any], output2: Dict[str, Any],
                       tolerance: float = 1e-6) -> Dict[str, Any]:
        """Compare two KRC output dictionaries."""
        results = {
            "match": True,
            "arrays": {},
            "scalars": {},
            "missing_keys": []
        }

        # Compare array fields
        array_fields = ["tsurf", "tdeep", "depth", "time", "ls"]
        for field in array_fields:
            if field in output1 and field in output2:
                comp = OutputDataComparator.compare_arrays(
                    output1[field], output2[field], field, tolerance
                )
                results["arrays"][field] = comp
                if not comp["match"]:
                    results["match"] = False
            elif field in output1 or field in output2:
                results["missing_keys"].append(field)
                results["match"] = False

        # Compare scalar/metadata fields
        scalar_fields = ["body", "lat", "lon", "alb"]
        for field in scalar_fields:
            if field in output1 and field in output2:
                match = output1[field] == output2[field]
                results["scalars"][field] = {
                    "match": match,
                    "value1": output1[field],
                    "value2": output2[field]
                }
                if not match:
                    results["match"] = False

        return results


class KRCValidator:
    """Main validation class for comparing PyKRC with krc.dvrc."""

    def __init__(self, krc_home: str, davinci_krc_path: Optional[str] = None,
                 dv_script_files: Optional[str] = None):
        """
        Initialize validator.

        Args:
            krc_home: Path to KRC installation
            davinci_krc_path: Path to krc.dvrc file (optional)
            dv_script_files: Path to Davinci script files (defaults to krc_home/krc_python/pykrc/data)
        """
        self.krc_home = Path(krc_home)
        self.davinci_krc_path = Path(davinci_krc_path) if davinci_krc_path else None

        # Set default DV_SCRIPT_FILES if not provided
        if dv_script_files:
            self.dv_script_files = Path(dv_script_files)
        else:
            self.dv_script_files = self.krc_home / "krc_python" / "pykrc" / "data"

        if not self.krc_home.exists():
            raise ValueError(f"KRC home not found: {krc_home}")

    def run_davinci_krc(self, command: str, workdir: Path, outdir: Path = None) -> Dict[str, Any]:
        """
        Run a davinci krc command and capture results.

        Args:
            command: Davinci krc command (e.g., 'krc(lat=12.,ls=23.,KEEP="T")')
            workdir: Working directory for script file
            outdir: Optional output directory for KRC files (passed to davinci krc)

        Returns:
            dict: Results including stdout, stderr, and file locations
        """
        # Modify command to include outdir if specified
        if outdir:
            # Insert outdir parameter into the krc() call
            # Handle both with and without KEEP parameter
            if 'KEEP=' in command:
                # Insert outdir before KEEP
                command = command.replace('KEEP=', f'outdir="{outdir}",KEEP=')
            else:
                # Insert outdir before closing parenthesis
                command = command.replace(')', f',outdir="{outdir}")')

        # Create executable davinci script with shebang
        # This method works reliably because it loads .dvrc automatically
        # which sets up DV_KRC_HOME and sources krc.dvrc
        script = f"""#!/Applications/davinci.app/Contents/Resources/bin/davinci -f

# Auto-generated validation script
# Note: KRC is loaded from ~/.dvrc which sources krc.dvrc
result = {command}
printf("Davinci KRC complete\\n")
"""

        script_file = workdir / "test_krc.dv"
        with open(script_file, 'w') as f:
            f.write(script)

        # Make script executable
        script_file.chmod(0o755)

        # Run the script directly (not via davinci -f)
        # This allows the shebang to work and .dvrc to be loaded
        result = subprocess.run(
            [str(script_file)],
            cwd=workdir,
            capture_output=True,
            text=True,
            timeout=300  # 5 minute timeout for long runs
        )

        return {
            "success": result.returncode == 0,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "returncode": result.returncode,
            "workdir": workdir,
            "script_file": script_file
        }

    def find_krc_files(self, workdir: Path, is_davinci: bool = False) -> Dict[str, Optional[Path]]:
        """
        Find KRC input and output files in a directory.

        Args:
            workdir: Working directory to search
            is_davinci: If True, search for davinci temp directories in /tmp
        """
        files = {
            "inp": None,
            "t52": None,
            "bin52": None,
            "prt": None,
            "elog": None
        }

        search_dir = workdir

        # For davinci, search in /tmp/dv_*/krc_* directories
        if is_davinci:
            import os
            import glob
            # Davinci creates directories like /tmp/dv_<pid>/krc_<random>
            # Search for the most recently modified one
            pattern = "/tmp/dv_*/krc_*"
            krc_dirs = glob.glob(pattern)
            if krc_dirs:
                # Get the most recently modified directory
                krc_dirs.sort(key=lambda x: os.path.getmtime(x), reverse=True)
                search_dir = Path(krc_dirs[0])
            else:
                # Fallback: search workdir
                search_dir = workdir

        # Find .inp file
        inp_files = list(search_dir.glob("*.inp"))
        if inp_files:
            files["inp"] = inp_files[0]

        # Find t52 file (pykrc names it krc.t52)
        t52_files = list(search_dir.glob("*t52*"))
        if t52_files:
            files["t52"] = t52_files[0]

        # Find bin52 file (davinci: outdata.bin.52, pykrc: krc.t52)
        # Both .t52 and .bin.52 are the same binary KRC output format
        bin52_files = list(search_dir.glob("*.bin.52")) + list(search_dir.glob("*.t52"))
        if bin52_files:
            files["bin52"] = bin52_files[0]

        # Find .prt file
        prt_files = list(search_dir.glob("*.prt"))
        if prt_files:
            files["prt"] = prt_files[0]

        # Find eLog file (KRC error log)
        elog_files = list(search_dir.glob("eLog*"))
        if elog_files:
            files["elog"] = elog_files[0]

        return files

    def compare_run(self, pykrc_params: Dict[str, Any],
                   davinci_cmd: str,
                   tolerance: float = 1e-6,
                   keep_files: bool = False,
                   test_name: str = None) -> Dict[str, Any]:
        """
        Run both PyKRC and davinci krc.dvrc and compare results.

        Args:
            pykrc_params: Parameters for PyKRC krc() call
            davinci_cmd: Davinci command string
            tolerance: Numerical comparison tolerance
            keep_files: Keep temporary files after comparison
            test_name: Optional name for test directory (creates persistent /tmp/krc_integration_test_{test_name}/)

        Returns:
            dict: Comprehensive comparison results including test_directory path
        """
        # Create temporary working directories
        if test_name:
            # Use persistent named directory in /tmp
            tmpdir = Path(f"/tmp/krc_integration_test_{test_name}")
            if tmpdir.exists():
                import shutil
                shutil.rmtree(tmpdir)
            tmpdir.mkdir(parents=True)
            pykrc_dir = tmpdir / "pykrc"
            davinci_dir = tmpdir / "davinci"
            pykrc_dir.mkdir()
            davinci_dir.mkdir()
            should_cleanup = False
        else:
            # Use auto-cleanup temp directory
            tmpdir = Path(tempfile.mkdtemp())
            pykrc_dir = tmpdir / "pykrc"
            davinci_dir = tmpdir / "davinci"
            pykrc_dir.mkdir()
            davinci_dir.mkdir()
            should_cleanup = not keep_files

        # Run PyKRC
        # Set KRC_HOME before running
        set_krc_home(str(self.krc_home))

        pykrc_params_with_keep = {
            **pykrc_params,
            "workdir": str(pykrc_dir),
            "verbose": True
        }

        try:
            pykrc_output = krc(**pykrc_params_with_keep)
            pykrc_success = True
            pykrc_error = None
        except Exception as e:
            pykrc_success = False
            pykrc_error = str(e)
            pykrc_output = None

        # Run davinci krc.dvrc with outdir parameter to write directly to our directory
        # davinci_krc_path is optional - davinci loads krc.dvrc from ~/.dvrc automatically
        davinci_result = self.run_davinci_krc(davinci_cmd, davinci_dir, outdir=davinci_dir)
        davinci_success = davinci_result["success"]
        davinci_error = davinci_result["stderr"] if not davinci_success else None

        # Find files (davinci now writes directly to davinci_dir via outdir parameter)
        pykrc_files = self.find_krc_files(pykrc_dir, is_davinci=False)
        davinci_files = self.find_krc_files(davinci_dir, is_davinci=False)

        # Compare input files
        inp_comparison = None
        if pykrc_files["inp"] and davinci_files["inp"]:
            inp_comparison = InputFileComparator.compare_inp_files(
                pykrc_files["inp"], davinci_files["inp"]
            )

        # Compare binary output files
        bin_comparison = None
        float_comparison = None
        if pykrc_files["bin52"] and davinci_files["bin52"]:
            bin_comparison = BinaryFileComparator.compare_binary_files(
                pykrc_files["bin52"], davinci_files["bin52"], tolerance
            )

            # Also compare as float arrays
            float_comparison = BinaryFileComparator.compare_float_arrays(
                pykrc_files["bin52"], davinci_files["bin52"],
                tolerance=tolerance
            )

        # Cleanup temporary directory if needed
        if should_cleanup:
            import shutil
            shutil.rmtree(tmpdir)

        return {
            "test_directory": str(tmpdir) if not should_cleanup else None,
            "pykrc": {
                "success": pykrc_success,
                "error": pykrc_error,
                "files": {k: str(v) if v else None for k, v in pykrc_files.items()}
            },
            "davinci": {
                "success": davinci_success,
                "error": davinci_error,
                "files": {k: str(v) if v else None for k, v in davinci_files.items()}
            },
            "input_file_comparison": inp_comparison,
            "binary_file_comparison": bin_comparison,
            "float_array_comparison": float_comparison,
            "summary": {
                # Execution status
                "both_succeeded": pykrc_success and davinci_success,

                # PRIMARY METRIC: Input file parity (must match exactly)
                "input_files_identical": inp_comparison["identical"] if inp_comparison else False,

                # SECONDARY METRIC: Temperature output parity (must match within tolerance)
                "temperature_arrays_match": float_comparison["identical"] if float_comparison else False,

                # Overall pass/fail based on BOTH metrics
                "overall_pass": (
                    pykrc_success and
                    davinci_success and
                    (inp_comparison["identical"] if inp_comparison else False) and  # PRIMARY must pass
                    (float_comparison["identical"] if float_comparison else False)  # SECONDARY must pass
                ),

                # Additional metrics for detailed analysis
                "binary_files_match": bin_comparison["identical"] if bin_comparison else False,
            }
        }


class ValidationTestSuite:
    """Collection of validation test cases from test_KRC.dv."""

    @staticmethod
    def get_basic_tests() -> List[Dict[str, Any]]:
        """Get basic validation test cases."""
        return [
            {
                "name": "Mars default run",
                "pykrc": {"lat": 25.0, "KEEP": "T"},
                "davinci": 'krc(lat=25.,KEEP="T")'
            },
            {
                "name": "Mars with Ls",
                "pykrc": {"lat": 12.0, "ls": 123.0, "KEEP": "T"},
                "davinci": 'krc(lat=12.,ls=123.,KEEP="T")'
            },
            {
                "name": "Mars with thermal inertia",
                "pykrc": {"lat": 12.0, "ls": 23.0, "INERTIA": 100.0, "KEEP": "T"},
                "davinci": 'krc(lat=12.,ls=23.,INERTIA=100.,KEEP="T")'
            },
            {
                "name": "Europa no flux",
                "pykrc": {"lat": 0.0, "INERTIA": 50.0, "body": "Europa",
                         "ALBEDO": 0.55, "LKofT": False, "KEEP": "T"},
                "davinci": 'krc(lat=0,INERTIA=50.,body="Europa",ALBEDO=.55,LKofT="F",KEEP="T")'
            },
            {
                "name": "Phobos",
                "pykrc": {"lat": 12.0, "body": "Phobos", "KEEP": "T"},
                "davinci": 'krc(lat=12.,body="Phobos",KEEP="T")'
            },
        ]

    @staticmethod
    def get_advanced_tests() -> List[Dict[str, Any]]:
        """Get advanced validation test cases."""
        return [
            {
                "name": "Two-layer regolith",
                "pykrc": {"lat": 0.0, "lon": 0.0, "thick": 0.3,
                         "INERTIA": 200.0, "INERTIA2": 1200.0, "LKofT": False},
                "davinci": 'krc(lat=0.,lon=0.,thick=0.3,INERTIA=200.,INERTIA2=1200.,LKofT="F",KEEP="T")'
            },
            {
                "name": "Eclipse modeling",
                "pykrc": {"lat": 0.0, "INERTIA": 45.0, "N1": 32, "body": "Europa",
                         "Eclipse": "T", "Eclipser": "Jupiter", "Eclipse_Style": 1.0},
                "davinci": 'krc(lat=0.,INERTIA=045.,N1=32,body="Europa",Eclipse="T",Eclipser="Jupiter",Eclipse_Style=1.,KEEP="T")'
            },
            {
                "name": "Planetary flux",
                "pykrc": {"lat": 0.0, "INERTIA": 50.0, "body": "Europa",
                         "ALBEDO": 0.55, "PFlux": "T", "Lon_Hr": 12.0, "LKofT": False},
                "davinci": 'krc(lat=0.,INERTIA=50.,body="Europa",ALBEDO=0.55,PFlux="T",Lon_Hr=12.,LKofT="F",KEEP="T")'
            },
        ]


def run_validation_suite(validator: KRCValidator,
                        test_suite: str = "basic",
                        tolerance: float = 1e-6) -> Dict[str, Any]:
    """
    Run a validation test suite.

    Args:
        validator: KRCValidator instance
        test_suite: "basic" or "advanced"
        tolerance: Numerical tolerance

    Returns:
        dict: Test results summary
    """
    if test_suite == "basic":
        tests = ValidationTestSuite.get_basic_tests()
    elif test_suite == "advanced":
        tests = ValidationTestSuite.get_advanced_tests()
    else:
        raise ValueError(f"Unknown test suite: {test_suite}")

    results = {
        "test_suite": test_suite,
        "total_tests": len(tests),
        "passed": 0,
        "failed": 0,
        "tests": []
    }

    for test in tests:
        print(f"\nRunning: {test['name']}")
        result = validator.compare_run(
            pykrc_params=test["pykrc"],
            davinci_cmd=test["davinci"],
            tolerance=tolerance
        )

        passed = result["summary"]["overall_pass"]
        if passed:
            results["passed"] += 1
            print(f"  ✓ PASSED")
            print(f"    • Input files: IDENTICAL")
            print(f"    • Temp arrays: MATCH")
        else:
            results["failed"] += 1
            print(f"  ✗ FAILED")
            if result["pykrc"]["error"]:
                print(f"    PyKRC error: {result['pykrc']['error']}")
            if result["davinci"]["error"]:
                print(f"    Davinci error: {result['davinci']['error']}")
            if not result["summary"]["input_files_identical"]:
                print(f"    PRIMARY FAILURE: Input files do not match")
            if not result["summary"]["temperature_arrays_match"]:
                print(f"    SECONDARY FAILURE: Temperature arrays differ")

        results["tests"].append({
            "name": test["name"],
            "passed": passed,
            "result": result
        })

    print(f"\n{'='*60}")
    print(f"Results: {results['passed']}/{results['total_tests']} passed")
    print(f"{'='*60}")

    return results
