"""Tests for bin5 file reader."""

import pytest
import numpy as np
import tempfile
from pathlib import Path

from pykrc.bin5_reader import (
    read_bin5_header,
    load_bin5,
    load_bin5_with_metadata,
    B5WT_BYTE,
    B5WT_INTEGER,
    B5WT_FLOAT,
    B5WT_DOUBLE,
)


def create_test_bin5(data: np.ndarray, word_type: int) -> Path:
    """Create a test bin5 file."""
    temp_file = tempfile.NamedTemporaryFile(mode='wb', delete=False, suffix='.bin5')

    ndim = len(data.shape)
    dims = data.shape
    nel = data.size

    # Write header with arch BEFORE C_END (matching C implementation)
    header = f"{ndim} {' '.join(map(str, dims))} {word_type} {nel} >> Test data "
    temp_file.write(header.encode('ascii'))

    # Write architecture string (5 chars BEFORE C_END)
    temp_file.write(b"x86  ")

    # Write C_END marker
    temp_file.write(b"C_END")

    # Pad to 512-byte boundary
    current_pos = temp_file.tell()
    pad_to = ((current_pos + 511) // 512) * 512
    padding = pad_to - current_pos
    temp_file.write(b'\x00' * padding)

    # Write data in Fortran (column-major) order to match real bin5 files
    # Transpose before writing so that when read back it matches original
    if ndim > 0:
        fortran_data = np.transpose(data)
        temp_file.write(fortran_data.tobytes())
    else:
        temp_file.write(data.tobytes())
    temp_file.close()

    return Path(temp_file.name)


def test_read_header():
    """Test reading bin5 header."""
    test_data = np.arange(24, dtype=np.float32).reshape(2, 3, 4)
    test_file = create_test_bin5(test_data, B5WT_FLOAT)

    try:
        header = read_bin5_header(test_file)

        assert header.ndim == 3
        assert header.dims == [2, 3, 4]
        assert header.word_type == B5WT_FLOAT
        assert header.nel == 24
        assert header.arch == "x86"  # Stripped version
        assert "Test data" in header.text

    finally:
        test_file.unlink()


def test_load_float_data():
    """Test loading float data."""
    original = np.arange(24, dtype=np.float32).reshape(2, 3, 4)
    test_file = create_test_bin5(original, B5WT_FLOAT)

    try:
        loaded = load_bin5(test_file)

        assert loaded.shape == original.shape
        assert loaded.dtype == np.float32
        assert np.allclose(loaded, original)

    finally:
        test_file.unlink()


def test_load_double_data():
    """Test loading double precision data."""
    original = np.linspace(0, 1, 60, dtype=np.float64).reshape(3, 4, 5)
    test_file = create_test_bin5(original, B5WT_DOUBLE)

    try:
        loaded = load_bin5(test_file)

        assert loaded.shape == original.shape
        assert loaded.dtype == np.float64
        assert np.allclose(loaded, original)

    finally:
        test_file.unlink()


def test_load_integer_data():
    """Test loading integer data."""
    original = np.arange(100, dtype=np.int16).reshape(10, 10)
    test_file = create_test_bin5(original, B5WT_INTEGER)

    try:
        loaded = load_bin5(test_file)

        assert loaded.shape == original.shape
        assert loaded.dtype == np.int16
        assert np.array_equal(loaded, original)

    finally:
        test_file.unlink()


def test_load_1d_data():
    """Test loading 1D data."""
    original = np.arange(100, dtype=np.float32)
    test_file = create_test_bin5(original, B5WT_FLOAT)

    try:
        loaded = load_bin5(test_file)

        assert loaded.shape == original.shape
        assert np.allclose(loaded, original)

    finally:
        test_file.unlink()


def test_load_with_metadata():
    """Test loading with metadata."""
    original = np.random.rand(5, 6, 7).astype(np.float32)
    test_file = create_test_bin5(original, B5WT_FLOAT)

    try:
        loaded, header = load_bin5_with_metadata(test_file)

        assert loaded.shape == original.shape
        assert header.ndim == 3
        assert header.dims == [5, 6, 7]
        assert header.word_type == B5WT_FLOAT

    finally:
        test_file.unlink()


def test_byte_data():
    """Test loading byte data."""
    original = np.arange(256, dtype=np.uint8).reshape(16, 16)
    test_file = create_test_bin5(original, B5WT_BYTE)

    try:
        loaded = load_bin5(test_file)

        assert loaded.shape == original.shape
        assert loaded.dtype == np.uint8
        assert np.array_equal(loaded, original)

    finally:
        test_file.unlink()


def test_complex_shape():
    """Test loading complex multi-dimensional data."""
    original = np.random.rand(2, 3, 4, 5).astype(np.float64)
    test_file = create_test_bin5(original, B5WT_DOUBLE)

    try:
        loaded = load_bin5(test_file)

        assert loaded.shape == original.shape
        assert np.allclose(loaded, original)

    finally:
        test_file.unlink()
