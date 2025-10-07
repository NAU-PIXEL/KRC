"""
Bin5 file format reader - Python implementation of ff_bin5.c

This implements the binary format used by Davinci/KRC for multidimensional array storage.
Based on the C implementation in ff_bin5.c.
"""

import struct
from pathlib import Path
from typing import Dict, Any, Optional, Tuple, List
import numpy as np
import sys


# Word types from ff_bin5.c (enum WordType)
B5WT_BYTE = 1
B5WT_INTEGER = 2
B5WT_LONG = 3
B5WT_FLOAT = 4
B5WT_DOUBLE = 5
B5WT_FLOAT_COMPLEX = 6
B5WT_STRING = 7
B5WT_STRUCTURE = 8
B5WT_DOUBLE_COMPLEX = 9
B5WT_PTR = 10
B5WT_OBJ_REF = 11
B5WT_UINTEGER = 12
B5WT_ULONG = 13
B5WT_LONGLONG = 14
B5WT_ULONGLONG = 15

# Type sizes in bytes
TYPE_SIZES = {
    B5WT_BYTE: 1,
    B5WT_INTEGER: 2,
    B5WT_LONG: 4,
    B5WT_FLOAT: 4,
    B5WT_DOUBLE: 8,
    B5WT_UINTEGER: 2,
    B5WT_ULONG: 4,
    B5WT_LONGLONG: 8,
    B5WT_ULONGLONG: 8,
}

# Numpy dtype mapping
NUMPY_DTYPES = {
    B5WT_BYTE: np.uint8,
    B5WT_INTEGER: np.int16,
    B5WT_LONG: np.int32,
    B5WT_FLOAT: np.float32,
    B5WT_DOUBLE: np.float64,
    B5WT_UINTEGER: np.uint16,
    B5WT_ULONG: np.uint32,
    B5WT_LONGLONG: np.int64,
    B5WT_ULONGLONG: np.uint64,
}

# Architecture IDs
ARCH_X86 = 1
ARCH_SUN = 2

ARCH_NAMES = {
    "x86  ": ARCH_X86,
    "sun  ": ARCH_SUN,
}


class Bin5Header:
    """Bin5 file header structure."""

    def __init__(self):
        self.ndim: int = 0
        self.dims: List[int] = []
        self.word_type: int = 0
        self.nel: int = 0  # Number of elements
        self.lbl_len: int = 0  # Label length in bytes
        self.arch: str = ""
        self.arch_id: int = 0
        self.text: str = ""


def read_bin5_header(filepath: Path) -> Bin5Header:
    """
    Read bin5 file header.

    Parameters
    ----------
    filepath : Path
        Path to bin5 file

    Returns
    -------
    Bin5Header
        Parsed header information

    Notes
    -----
    The header format is:
    - Variable-length ASCII section ending with "C_END"
    - Format: "ndim dim1 dim2 ... dimN word_type nel >> optional_text C_END arch"
    - Architecture is 5 chars before "C_END"
    """
    header = Bin5Header()
    block_size = 512
    lbl_end_marker = b"C_END"

    with open(filepath, 'rb') as f:
        # Read blocks until we find C_END marker
        buff = b""
        while True:
            chunk = f.read(block_size)
            if not chunk:
                raise ValueError(f"C_END marker not found in {filepath}")

            buff += chunk

            if lbl_end_marker in buff:
                break

        # Find the position of C_END
        end_pos = buff.find(lbl_end_marker)

        # Extract architecture (5 chars BEFORE C_END, matching C code)
        # C code: strncpy(arch, &buff[buff_sz-1-strlen(lbl_end_marker)-5], 5);
        arch = buff[end_pos - 5:end_pos].decode('ascii', errors='ignore')
        header.arch = arch.strip()
        header.arch_id = ARCH_NAMES.get(arch.strip(), 0)

        # Header length is the total buffer size (which is multiple of 512)
        # C code: (*b5h)->lbl_len = buff_sz-1;
        # This is where data starts (end of the last 512-byte block read)
        header.lbl_len = len(buff)

        # Parse the header data (everything before arch string, which is 5 bytes before C_END)
        header_text = buff[:end_pos - 5].decode('ascii', errors='ignore')

        # Tokenize by spaces
        tokens = header_text.split()

        # First token: number of dimensions
        header.ndim = int(tokens[0])

        # Next N tokens: dimensions
        header.dims = [int(tokens[i + 1]) for i in range(header.ndim)]

        # Next: word type
        header.word_type = int(tokens[header.ndim + 1])

        # Next: number of elements
        header.nel = int(tokens[header.ndim + 2])

        # Extract text if present (after ">>")
        if ">>" in header_text:
            text_start = header_text.find(">>") + 2
            text_end = header_text.find("C_END")
            header.text = header_text[text_start:text_end].strip()
        else:
            header.text = ""

    return header


def byte_swap(data: bytes) -> bytes:
    """Swap byte order of data."""
    return bytes(reversed(data))


def load_bin5(filepath: str | Path) -> np.ndarray:
    """
    Load a bin5 file into a numpy array.

    Parameters
    ----------
    filepath : str or Path
        Path to bin5 file

    Returns
    -------
    np.ndarray
        Multi-dimensional array from file

    Notes
    -----
    This implements the algorithm from ff_bin5.c:
    1. Read header to get dimensions and data type
    2. Read binary data after header
    3. Handle byte swapping if needed
    4. Reshape to proper dimensions

    The C code organizes dimensions such that the last 3 dimensions
    form a "block" that is read contiguously, with higher dimensions
    creating a structure hierarchy. For Python, we simplify this to
    a direct multi-dimensional array.
    """
    filepath = Path(filepath)

    # Read header
    header = read_bin5_header(filepath)

    # Get data type info
    if header.word_type not in TYPE_SIZES:
        raise ValueError(f"Unsupported word type: {header.word_type}")

    item_bytes = TYPE_SIZES[header.word_type]
    dtype = NUMPY_DTYPES[header.word_type]

    # Determine if we need byte swapping
    # Check system byte order
    is_big_endian = sys.byteorder == 'big'
    need_swap = (is_big_endian and header.arch_id == ARCH_X86) or \
                (not is_big_endian and header.arch_id == ARCH_SUN)

    # Read binary data
    with open(filepath, 'rb') as f:
        # Skip to data section (after header)
        f.seek(header.lbl_len)

        # Read all data
        data_bytes = f.read()

    # Convert to numpy array
    if need_swap:
        # Create array with swapped byte order
        dt = np.dtype(dtype).newbyteorder()
        data = np.frombuffer(data_bytes, dtype=dt)
        data = data.byteswap().newbyteorder()
    else:
        data = np.frombuffer(data_bytes, dtype=dtype)

    # Reshape to proper dimensions
    # Note: C code uses reverse dimension ordering (column-major)
    # while numpy uses row-major by default
    if header.ndim > 0:
        # Reshape with dimensions in reverse order (C convention)
        dims = header.dims[::-1]  # Reverse for C-style indexing
        data = data.reshape(dims)
        # Transpose to get proper orientation
        data = np.transpose(data)

    return data


def load_bin5_with_metadata(filepath: str | Path) -> Tuple[np.ndarray, Bin5Header]:
    """
    Load bin5 file and return both data and header.

    Parameters
    ----------
    filepath : str or Path
        Path to bin5 file

    Returns
    -------
    tuple
        (data array, header object)
    """
    filepath = Path(filepath)
    header = read_bin5_header(filepath)
    data = load_bin5(filepath)
    return data, header


def create_bin5_structure(filepath: str | Path) -> Dict[str, Any]:
    """
    Create a structure similar to Davinci's output.

    For bin5 files with more than 3 dimensions, this creates
    a nested dictionary structure following the Davinci convention.

    Parameters
    ----------
    filepath : str or Path
        Path to bin5 file

    Returns
    -------
    dict
        Structured data matching Davinci output format
    """
    data, header = load_bin5_with_metadata(filepath)

    # If 3D or less, return simple array
    if header.ndim <= 3:
        return {
            'data': data,
            'dims': header.dims,
            'type': header.word_type,
            'text': header.text,
        }

    # For >3D, create nested structure
    # The C code creates structures for dimensions beyond the last 3
    # For simplicity, we'll return the full array with metadata

    return {
        'data': data,
        'dims': header.dims,
        'type': header.word_type,
        'text': header.text,
        'ndim': header.ndim,
    }


def test_bin5_reader():
    """Test function to verify bin5 reader."""
    import tempfile

    # Create a test bin5 file
    test_data = np.arange(24, dtype=np.float32).reshape(2, 3, 4)

    with tempfile.NamedTemporaryFile(mode='wb', delete=False, suffix='.bin5') as f:
        # Write header
        ndim = 3
        dims = test_data.shape
        word_type = B5WT_FLOAT
        nel = test_data.size

        # Header format: "ndim dim1 dim2 ... word_type nel >> text C_END arch"
        header = f"{ndim} {dims[0]} {dims[1]} {dims[2]} {word_type} {nel} >> Test data C_ENDx86  "
        f.write(header.encode('ascii'))

        # Write data
        f.write(test_data.tobytes())

        test_file = f.name

    # Read it back
    loaded_data = load_bin5(test_file)

    print(f"Original shape: {test_data.shape}")
    print(f"Loaded shape: {loaded_data.shape}")
    print(f"Data matches: {np.allclose(test_data, loaded_data)}")

    # Clean up
    Path(test_file).unlink()


if __name__ == "__main__":
    test_bin5_reader()
