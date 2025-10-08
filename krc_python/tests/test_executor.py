#!/usr/bin/env python
"""
Test the updated KRC executor with minimal parameters.
"""
import os
import sys
from pathlib import Path

# Set up KRC_HOME
KRC_HOME = Path("/Users/chaberle/Documents/GitHab/KRC")
os.environ["KRC_HOME"] = str(KRC_HOME)

# Add to path
sys.path.insert(0, str(KRC_HOME / "krc_python"))

from pykrc.executor import KRCExecutor
from pykrc.bin5_reader import load_bin5_with_metadata


def get_minimal_params():
    """Get default parameters from master.inp - exact defaults, no modifications."""
    return {
        # KOLD/KEEP
        'KOLD': 0,
        'KEEP': 0,

        # Title
        'TITLE': 'Python KRC Executor Test - Using master.inp defaults',

        # Real parameters - Surface properties (from master.inp)
        'ALBEDO': 0.25,
        'EMISS': 1.00,
        'INERTIA': 200.0,
        'COND2': 2.77,
        'DENS2': 928.0,
        'PERIOD': 1.0275,
        'SPEC_HEAT': 647.0,
        'DENSITY': 1600.0,

        # Atmospheric properties (from master.inp)
        'CABR': 0.11,
        'AMW': 43.5,
        'SatPrA': 27.9546,
        'PTOTAL': 546.0,
        'FANON': 0.055,
        'TATM': 200.0,
        'TDEEP': 180.0,
        'SpHeat2': 1711.0,

        # Dust & Slope (from master.inp)
        'TAUD': 0.3,
        'DUSTA': 0.90,
        'TAURAT': 0.25,
        'TWILI': 0.0,
        'ARC2_G0': 0.5,
        'ARC3_Safe': 0.801,
        'SLOPE': 0.0,
        'SLOAZI': 90.0,

        # Frost properties (from master.inp)
        'TFROST': 146.0,
        'CFROST': 589944.0,
        'AFROST': 0.65,
        'FEMIS': 0.95,
        'AF1': 0.54,
        'AF2': 0.0009,
        'FROEXT': 50.0,
        'SatPrB': 3182.48,

        # Thermal solution (from master.inp)
        'RLAY': 1.15,
        'FLAY': 0.100,
        'CONVF': 3.0,
        'DEPTH': 0.0,
        'DRSET': 0.0,
        'PhotoFunc': 0.0,
        'GGT': 0.1,
        'DTMAX': 0.1,

        # Orbital/time (from master.inp)
        'DJUL': -1222.69,
        'DELJUL': 17.174822,
        'SOLARDEC': 0.0,
        'DAU': 1.465,
        'LsubS': 0.0,
        'SOLCON': 1368.0,
        'GRAV': 3.727,
        'Atm_Cp': 735.9,

        # Temperature-dependent K and Cp coefficients (from master.inp)
        'ConUp0': 0.038640,
        'ConUp1': -0.002145,
        'ConUp2': 0.002347,
        'ConUp3': -0.000750,
        'ConLo0': 2.766722,
        'ConLo1': -1.298966,
        'ConLo2': 0.629224,
        'ConLo3': -0.527291,
        'SphUp0': 646.6275,
        'SphUp1': 246.6678,
        'SphUp2': -49.8216,
        'SphUp3': 7.9520,
        'SphLo0': 1710.648,
        'SphLo1': 721.8740,
        'SphLo2': 57.44873,
        'SphLo3': 24.37532,

        # Integer parameters - USE DEFAULTS FROM MASTER.INP
        'N1': 28,        # Default: 28 layers
        'N2': 1536,      # Default: 1536 times per day
        'N3': 15,        # Default: 15 days iteration
        'N4': 19,        # Default: 19 latitudes
        'N5': 120,       # Default: 120 seasons
        'N24': 48,       # Default: 48 hours output
        'IIB': 0,        # Insulating bottom
        'IC2': 999,      # Homogeneous

        'NRSET': 3,      # Default: 3
        'NMHA': 24,
        'NRUN': 0,
        'JDISK': 81,     # Default: 81 (start disk output later)
        'IDOWN': 0,
        'FlxP14': 45,
        'TUN_Flx15': 65,
        'KPREF': 1,

        'K4OUT': 52,     # bin5 type 52
        'JBARE': 0,
        'Notif': 50,
        'IDISK2': 0,

        # Logical flags (from master.inp)
        'LP1': False,
        'LP2': True,     # Print parameters
        'LP3': False,
        'LP4': False,    # Default: False
        'LP5': False,
        'LP6': False,
        'LPGLOB': False,
        'LVFA': False,
        'LVFT': False,
        'LKofT': False,
        'LPORB': True,   # Use orbital calculations
        'LKEY': False,
        'LSC': False,
        'LZONE': False,
        'LOCAL': True,
        'Prt76': False,
        'LPTAVE': False,
        'Prt78': False,
        'Prt79': False,
        'L_ONE': False,

        # Latitudes and elevations (from master.inp - 19 latitudes)
        'Latitudes': [-87.50, -80.00, -70.00, -60.00, -50.00, -40.00, -30.00, -20.00, -10.00, 0.00,
                      10.00, 20.00, 30.00, 40.00, 50.00, 60.00, 70.00, 80.00, 87.50],
        'Elevations': [3.51, 2.01, 1.39, 1.22, 0.38, 0.48, 1.17, 1.67, 1.26, 0.17,
                      -0.94, -1.28, -1.99, -2.51, -3.52, -4.08, -4.51, -4.38, -2.57],

        # PORB parameters for Mars (from master.inp)
        'PORB_PARAMS': [
            104.0000, 0.1000000, 0.8644665, 0.3226901E-01, -1.281586,
            0.9340198E-01, 1.523712, 0.4090926, 0.000000, 0.9229373,
            5.544402, 0.000000, 0.000000, 686.9929, 3397.977,
            24.62296, 0.000000, -1.240317, 0.000000, 0.000000,
            0.000000, 0.3244965, 0.8559126, 0.4026359, -0.9458869,
            0.2936298, 0.1381285, 0.000000, -0.4256703, 0.9048783,
        ],
    }


def main():
    print("=" * 70)
    print("Testing KRC Executor with Minimal Parameters")
    print("=" * 70)
    print()

    # Create executor
    executor = KRCExecutor()

    print(f"KRC executable: {executor.krc_exe}")
    print(f"Support dir: {executor.paths.support_dir}")
    print()

    # Get minimal parameters
    params = get_minimal_params()

    print(f"Running KRC with DEFAULT parameters from master.inp:")
    print(f"  N1={params['N1']}, N2={params['N2']}, N3={params['N3']}")
    print(f"  N4={params['N4']}, N5={params['N5']}")
    print(f"  Estimated iterations: {params['N1'] * params['N2'] * params['N3'] * params['N4'] * params['N5']:,}")
    print(f"  This will take several minutes...")
    print()

    # Run KRC
    try:
        result = executor.run_krc(
            params,
            basename="test_executor",
            verbose=True,
            timeout=600  # 10 minutes for full default run
        )

        print()
        print("=" * 70)
        print("SUCCESS!")
        print("=" * 70)
        print(f"Return code: {result['returncode']}")
        print(f"Working directory: {result['workdir']}")

        if result['output_file'] and result['output_file'].exists():
            print(f"\n✓ Output file created: {result['output_file']}")
            print(f"  Size: {result['output_file'].stat().st_size:,} bytes")

            # Try to read with bin5_reader
            try:
                data, header = load_bin5_with_metadata(result['output_file'])
                print(f"\n✓ Successfully read bin5 file!")
                print(f"  Shape: {data.shape}")
                print(f"  Dtype: {data.dtype}")
                print(f"  Min: {data.min():.2f}, Max: {data.max():.2f}, Mean: {data.mean():.2f}")
            except Exception as e:
                print(f"\n✗ Failed to read bin5 file: {e}")

        if result['prt_file'] and result['prt_file'].exists():
            print(f"\n✓ Print file created: {result['prt_file']}")
            print(f"  Size: {result['prt_file'].stat().st_size:,} bytes")

            # Show first few lines
            with open(result['prt_file']) as f:
                lines = f.readlines()[:30]
                print(f"\n  First {len(lines)} lines:")
                for line in lines[:15]:
                    print(f"    {line.rstrip()}")

        print()
        return 0

    except Exception as e:
        print()
        print("=" * 70)
        print("FAILED!")
        print("=" * 70)
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
