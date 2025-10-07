#!/usr/bin/env python
"""
Simple test script to run KRC and read output using pykrc.
"""
import os
import sys
from pathlib import Path
import subprocess

# Set up KRC_HOME
KRC_HOME = Path("/Users/chaberle/Documents/GitHab/KRC")
os.environ["KRC_HOME"] = str(KRC_HOME)

# Import pykrc
sys.path.insert(0, str(KRC_HOME / "krc_python"))
from pykrc.config import set_krc_home, get_paths
from pykrc.bin5_reader import load_bin5, load_bin5_with_metadata

def main():
    # Set up paths
    set_krc_home(KRC_HOME)
    paths = get_paths()

    print(f"KRC Home: {KRC_HOME}")
    print(f"KRC Executable: {KRC_HOME / 'krc'}")
    print(f"Run Directory: {paths.run_dir}")
    print()

    # Check if KRC executable exists
    krc_exe = KRC_HOME / "krc"
    if not krc_exe.exists():
        print(f"ERROR: KRC executable not found at {krc_exe}")
        return 1

    # Run KRC with master.inp
    print("Running KRC with master.inp...")
    print("-" * 60)

    # Change to run directory and execute KRC
    original_dir = os.getcwd()
    try:
        os.chdir(paths.run_dir)

        # Create output directory if it doesn't exist
        out_dir = Path("out")
        out_dir.mkdir(exist_ok=True)

        # Run KRC
        result = subprocess.run(
            [str(krc_exe)],
            stdin=open("master.inp", "r"),
            capture_output=True,
            text=True,
            timeout=60
        )

        print("STDOUT:")
        print(result.stdout)
        if result.stderr:
            print("\nSTDERR:")
            print(result.stderr)

        print("-" * 60)
        print(f"Return code: {result.returncode}")
        print()

        if result.returncode != 0:
            print("ERROR: KRC returned non-zero exit code")
            return result.returncode

        # Check for output file
        output_file = out_dir / "master356.t52"
        if not output_file.exists():
            print(f"ERROR: Output file not found at {output_file}")
            print(f"Files in {out_dir}:")
            for f in out_dir.iterdir():
                print(f"  {f.name}")
            return 1

        print(f"Output file created: {output_file}")
        print(f"File size: {output_file.stat().st_size} bytes")
        print()

        # Try to read the output file with pykrc
        print("Reading output file with pykrc...")
        print("-" * 60)

        try:
            data, header = load_bin5_with_metadata(output_file)
            print(f"Successfully loaded bin5 file!")
            print(f"  Shape: {data.shape}")
            print(f"  Dtype: {data.dtype}")
            print(f"  Dimensions: {header.dims}")
            print(f"  Word type: {header.word_type}")
            print(f"  Architecture: {header.arch}")
            print(f"  Min value: {data.min():.2f}")
            print(f"  Max value: {data.max():.2f}")
            print(f"  Mean value: {data.mean():.2f}")
            print()
            print("First few values:")
            print(data.flat[:10])

        except Exception as e:
            print(f"ERROR reading output file: {e}")
            import traceback
            traceback.print_exc()
            return 1

        print()
        print("=" * 60)
        print("SUCCESS! KRC ran and pykrc successfully read the output!")
        print("=" * 60)

    finally:
        os.chdir(original_dir)

    return 0

if __name__ == "__main__":
    sys.exit(main())
