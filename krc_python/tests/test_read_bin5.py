#!/usr/bin/env python
"""
Test reading existing bin5 files with pykrc.
"""
import os
import sys
from pathlib import Path

# Set up KRC_HOME
KRC_HOME = Path("/Users/chaberle/Documents/GitHab/KRC")
os.environ["KRC_HOME"] = str(KRC_HOME)

# Import pykrc
sys.path.insert(0, str(KRC_HOME / "krc_python"))
from pykrc.bin5_reader import load_bin5, load_bin5_with_metadata

def main():
    print("=" * 70)
    print("Testing pykrc bin5 reader with existing files")
    print("=" * 70)
    print()

    # Test file
    test_file = KRC_HOME / "run" / "THEMIS1yearDustIce.bin5"

    if not test_file.exists():
        print(f"ERROR: Test file not found at {test_file}")
        return 1

    print(f"Reading file: {test_file}")
    print(f"File size: {test_file.stat().st_size} bytes")
    print()

    try:
        # Load with metadata
        data, header = load_bin5_with_metadata(test_file)

        print("Successfully loaded bin5 file!")
        print("-" * 70)
        print(f"Header Information:")
        print(f"  Number of dimensions: {header.ndim}")
        print(f"  Dimensions: {header.dims}")
        print(f"  Total elements: {header.nel}")
        print(f"  Word type: {header.word_type}")
        print(f"  Architecture: {header.arch!r}")
        print(f"  Header length: {header.lbl_len} bytes")
        if header.text:
            print(f"  Description: {header.text}")
        print()

        print(f"Array Information:")
        print(f"  Shape: {data.shape}")
        print(f"  Data type: {data.dtype}")
        print(f"  Min value: {data.min():.6f}")
        print(f"  Max value: {data.max():.6f}")
        print(f"  Mean value: {data.mean():.6f}")
        print(f"  Std dev: {data.std():.6f}")
        print()

        # Show a sample of the data
        print(f"Sample data (first 10 elements, flattened):")
        print(data.flat[:10])
        print()

        # If it's multidimensional, show shape info
        if len(data.shape) > 1:
            print(f"Array shape details:")
            for i, dim_size in enumerate(data.shape):
                print(f"  Dimension {i}: {dim_size} elements")
            print()

        print("=" * 70)
        print("SUCCESS! pykrc successfully read the bin5 file!")
        print("=" * 70)

        return 0

    except Exception as e:
        print(f"ERROR reading bin5 file: {e}")
        import traceback
        traceback.print_exc()
        return 1

if __name__ == "__main__":
    sys.exit(main())
