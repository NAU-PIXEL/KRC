#!/usr/bin/env python
"""
Minimal test to run KRC properly based on investigation findings.

Key findings:
1. KRC expects TWO lines via stdin: input_name and output_name (no extensions)
2. KRC adds .inp and .prt extensions automatically
3. Binary output path is specified IN the .inp file
"""
import os
import sys
import subprocess
from pathlib import Path
import tempfile

# Set up KRC_HOME
KRC_HOME = Path("/Users/chaberle/Documents/GitHab/KRC")
os.environ["KRC_HOME"] = str(KRC_HOME)

# Import pykrc
sys.path.insert(0, str(KRC_HOME / "krc_python"))
from pykrc.bin5_reader import load_bin5, load_bin5_with_metadata

def create_minimal_inp(workdir: Path, name: str = "test") -> Path:
    """
    Create a minimal KRC input file based on master.inp.
    Simplified to run faster - single latitude, fewer time steps.
    """
    inp_file = workdir / f"{name}.inp"

    # Minimal input - based on master.inp but simplified
    content = """0 0 / KOLD: season to start with;  KEEP: continue saving data in same disk file
Version 356 minimal test - 1 latitude, short run
    ALBEDO     EMISS   INERTIA     COND2     DENS2    PERIOD SPEC_HEAT   DENSITY
       .25      1.00     200.0      2.77     928.0    1.0275      647.     1600.
      CABR       AMW    SatPrA    PTOTAL     FANON      TATM     TDEEP   SpHeat2
      0.11      43.5   27.9546     546.0      .055      200.     180.0     1711.
      TAUD     DUSTA    TAURAT     TWILI   ARC2_G0 ARC3_Safe     SLOPE    SLOAZI
       0.3       .90      0.25       0.0       0.5     0.801       0.0       90.
    TFROST    CFROST    AFROST     FEMIS       AF1       AF2    FROEXT    SatPrB
     146.0   589944.       .65      0.95      0.54    0.0009       50.   3182.48
      RLAY      FLAY     CONVF     DEPTH     DRSET PhotoFunc       GGT     DTMAX
    1.1500      .100       3.0       0.0       0.0       0.0       0.1       0.1
      DJUL    DELJUL  SOLARDEC       DAU     LsubS    SOLCON      GRAV    Atm_Cp
  -1222.69 17.174822      00.0     1.465        .0     1368.     3.727     735.9
    ConUp0    ConUp1    ConUp2    ConUp3    ConLo0    ConLo1    ConLo2    ConLo3
  0.038640 -0.002145  0.002347 -0.000750  2.766722 -1.298966  0.629224 -0.527291
    SphUp0    SphUp1    SphUp2    SphUp3    SphLo0    SphLo1    SphLo2    SphLo3
  646.6275  246.6678  -49.8216    7.9520  1710.648  721.8740  57.44873  24.37532
        N1        N2        N3        N4        N5       N24       IIB       IC2
        15       288        15         1        60        24         0       999
     NRSET      NMHA      NRUN     JDISK     IDOWN    FlxP14 TUN_Flx15     KPREF
         3        24         0        52         0        45        65         1
     K4OUT     JBARE     Notif    IDISK2                                     end
        52         0        50         0                                       0
    LP1    LP2    LP3    LP4    LP5    LP6 LPGLOB   LVFA   LVFT  LKofT
      F      T      F      F      F      F      F      F      F      F
  LPORB   LKEY    LSC  LZONE  LOCAL  Prt76 LPTAVE  Prt78  Prt79  L_ONE
      T      F      F      F      T      F      F      F      F      F
Latitudes: in 10F7.2  _____7 _____7 _____7 _____7 _____7 _____7 _____7
   0.00
 _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7
   0.00
PORB:2014jun10 2019 Aug 14 22:20:06 IPLAN,TC= 101.0 0.10000 Mars:Mars
   101.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586
  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373
   5.544402       0.000000       0.000000       686.9929       3397.977
   24.62296       0.000000      -1.240317       0.000000       0.000000
   0.000000      0.3244965      0.8559126      0.4026359     -0.9458869
  0.2936298      0.1381285       0.000000     -0.4256703      0.9048783
8 5 0 './test.t52' / Disk file name for Run 1
0/
0/  ======================= end of run
"""

    with open(inp_file, 'w') as f:
        f.write(content)

    return inp_file

def run_krc_properly(workdir: Path, name: str = "test") -> dict:
    """
    Run KRC the way Davinci does it.

    Returns dict with:
        - success: bool
        - output_file: Path to .t52 file
        - print_file: Path to .prt file
        - stdout: str
        - stderr: str
    """
    krc_exe = KRC_HOME / "krc"

    if not krc_exe.exists():
        return {"success": False, "error": f"KRC executable not found at {krc_exe}"}

    # Copy required data files to working directory
    # KRC needs these when LPORB=T
    required_files = ["standish.tab", "spinaxis.tab", "PORBCM.mat"]
    for filename in required_files:
        src = KRC_HOME / "run" / filename
        if src.exists():
            import shutil
            shutil.copy(src, workdir / filename)
            print(f"Copied {filename} to working directory")
        else:
            print(f"Warning: {filename} not found")

    # Create input file
    inp_file = create_minimal_inp(workdir, name)
    print(f"Created input file: {inp_file}")

    # KRC expects two lines via stdin:
    # 1. Input filename (without .inp)
    # 2. Output filename (without .prt)
    stdin_input = f"{name}\n{name}\n"

    # Run KRC from the working directory
    print(f"Running KRC with input: {repr(stdin_input)}")
    print(f"Working directory: {workdir}")

    try:
        result = subprocess.run(
            [str(krc_exe)],
            input=stdin_input,
            capture_output=True,
            text=True,
            cwd=str(workdir),
            timeout=120  # 2 minute timeout
        )

        output_file = workdir / f"{name}.t52"
        print_file = workdir / f"{name}.prt"

        return {
            "success": result.returncode == 0,
            "returncode": result.returncode,
            "output_file": output_file,
            "print_file": print_file,
            "stdout": result.stdout,
            "stderr": result.stderr
        }

    except subprocess.TimeoutExpired:
        return {"success": False, "error": "KRC timed out after 120 seconds"}
    except Exception as e:
        return {"success": False, "error": str(e)}

def main():
    print("=" * 70)
    print("KRC Minimal Test - Proper Execution Method")
    print("=" * 70)
    print()

    # Create temporary working directory
    with tempfile.TemporaryDirectory() as tmpdir:
        workdir = Path(tmpdir)
        print(f"Working directory: {workdir}")
        print()

        # Run KRC
        result = run_krc_properly(workdir)

        if not result["success"]:
            print(f"ERROR: {result.get('error', 'KRC failed')}")
            print(f"Return code: {result.get('returncode', 'N/A')}")
            print(f"\nSTDOUT:\n{result.get('stdout', '')}")
            print(f"\nSTDERR:\n{result.get('stderr', '')}")
            return 1

        print("KRC ran successfully!")
        print(f"Return code: {result['returncode']}")
        print()

        # Check for output files
        if result["output_file"].exists():
            print(f"Output file created: {result['output_file']}")
            print(f"  Size: {result['output_file'].stat().st_size} bytes")

            # Try to read with pykrc
            try:
                data, header = load_bin5_with_metadata(result["output_file"])
                print(f"\n✓ Successfully read output with pykrc!")
                print(f"  Shape: {data.shape}")
                print(f"  Dtype: {data.dtype}")
                print(f"  Min: {data.min():.2f}, Max: {data.max():.2f}, Mean: {data.mean():.2f}")

            except Exception as e:
                print(f"\n✗ Failed to read output with pykrc: {e}")
        else:
            print(f"WARNING: Output file not created at {result['output_file']}")

        if result["print_file"].exists():
            print(f"\nPrint file created: {result['print_file']}")
            print(f"  Size: {result['print_file'].stat().st_size} bytes")
            # Show first 50 lines
            with open(result["print_file"]) as f:
                lines = f.readlines()[:50]
                print(f"  First {len(lines)} lines:")
                for line in lines[:10]:
                    print(f"    {line.rstrip()}")

        print()
        print("=" * 70)
        print("TEST COMPLETE")
        print("=" * 70)

    return 0

if __name__ == "__main__":
    sys.exit(main())
