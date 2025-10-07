#!/usr/bin/env python
"""Debug bin5 header reading."""
import sys
from pathlib import Path

sys.path.insert(0, "/Users/chaberle/Documents/GitHab/KRC/krc_python")
from pykrc.bin5_reader import read_bin5_header, TYPE_SIZES, NUMPY_DTYPES

test_file = Path("/Users/chaberle/Documents/GitHab/KRC/run/THEMIS1yearDustIce.bin5")

print(f"File size: {test_file.stat().st_size} bytes")
print()

# Read header
header = read_bin5_header(test_file)

print(f"Header information:")
print(f"  ndim: {header.ndim}")
print(f"  dims: {header.dims}")
print(f"  word_type: {header.word_type}")
print(f"  nel: {header.nel}")
print(f"  arch: {header.arch!r}")
print(f"  arch_id: {header.arch_id}")
print(f"  lbl_len: {header.lbl_len}")
print(f"  text: {header.text!r}")
print()

# Calculate expected data size
if header.word_type in TYPE_SIZES:
    item_size = TYPE_SIZES[header.word_type]
    expected_data_size = header.nel * item_size
    data_start = header.lbl_len
    actual_data_size = test_file.stat().st_size - data_start

    print(f"Data size calculation:")
    print(f"  Item size: {item_size} bytes")
    print(f"  Number of elements: {header.nel}")
    print(f"  Expected data size: {expected_data_size} bytes")
    print(f"  Data starts at byte: {data_start}")
    print(f"  Actual data size: {actual_data_size} bytes")
    print(f"  Difference: {actual_data_size - expected_data_size} bytes")
    print()

    # Read first 200 bytes of file
    with open(test_file, 'rb') as f:
        first_bytes = f.read(200)

    print("First 200 bytes (hex):")
    print(' '.join(f'{b:02x}' for b in first_bytes[:100]))
    print()

    print("First 200 bytes (ASCII, dots for non-printable):")
    ascii_str = ''.join(chr(b) if 32 <= b < 127 else '.' for b in first_bytes)
    print(ascii_str)
else:
    print(f"Unknown word type: {header.word_type}")
