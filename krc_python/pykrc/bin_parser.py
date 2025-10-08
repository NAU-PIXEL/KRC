"""
Consolidated bin5/bin52 parser for KRC output files.

This module combines functionality from bin5_reader.py, bin52_parser.py,
and bin52_complete.py into a single, working parser based on analysis
of actual KRC output structure.
"""

import struct
from pathlib import Path
from typing import Dict, Any, Optional, Tuple, List
import numpy as np
import sys


# ============================================================================
# BIN5 FORMAT CONSTANTS
# ============================================================================

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
    "x86_64": ARCH_X86,  # Modern x86-64 systems
    "86_64": ARCH_X86,   # If we only read last 5 chars
    "sun  ": ARCH_SUN,
}


# ============================================================================
# BIN5 HEADER STRUCTURE
# ============================================================================

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


# ============================================================================
# BIN5 LOW-LEVEL READER
# ============================================================================

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

        # Extract architecture (5 chars BEFORE C_END)
        arch = buff[end_pos - 5:end_pos].decode('ascii', errors='ignore')
        header.arch = arch.strip()
        header.arch_id = ARCH_NAMES.get(arch.strip(), 0)

        # Header length
        header.lbl_len = len(buff)

        # Parse the header data (everything before arch string)
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
            header.text = header_text[text_start:].strip()
        else:
            header.text = ""

    return header


def load_bin5(filepath: str | Path):
    """
    Load a bin5 file, returning nested lists for dims > 3 (like davinci).

    For bin files with ndim <= 3, returns a simple numpy array.
    For ndim > 3, returns nested Python lists where the first (ndim-3)
    dimensions are list indices, and the last 3 dimensions are numpy arrays.

    This matches davinci's ff_bin5.c behavior where dimensions beyond the
    last 3 become nested structures.

    Parameters
    ----------
    filepath : str or Path
        Path to bin5 file

    Returns
    -------
    list or np.ndarray
        For ndim <= 3: numpy array
        For ndim > 3: nested lists with 3D numpy arrays at the leaves

    Example
    -------
    For shape (48, 7, 1, 42, 1):
    Returns data[hour][field] where each element is a (1, 42, 1) array
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
        dt = np.dtype(dtype).newbyteorder()
        data = np.frombuffer(data_bytes, dtype=dt)
        data = data.byteswap().newbyteorder()
    else:
        data = np.frombuffer(data_bytes, dtype=dtype)

    ndim = header.ndim
    dims = header.dims

    if ndim <= 3:
        # Simple case: return numpy array
        if ndim > 0:
            dims_reversed = dims[::-1]
            data = data.reshape(dims_reversed)
            data = np.transpose(data)
        return data

    # Complex case: create nested structure like davinci
    # Bin5 files use Fortran column-major order where first index varies fastest
    # For dims=[A,B,C,D,E], linear data is: data[0,0,0,0,0], data[1,0,0,0,0], ...

    # First, reshape the entire dataset with Fortran order to get correct multidimensional array
    data_full = data.reshape(dims, order='F')

    # Now carve into nested structure: first (ndim-3) dimensions become nested lists,
    # last 3 dimensions stay as numpy arrays
    def build_nested(arr, depth):
        """
        Recursively build nested structure from numpy array.
        arr: current array slice
        depth: current nesting depth (0 = outermost)
        """
        if arr.ndim == 3:
            # At the leaf level - return the 3D array as-is
            return arr
        else:
            # Create list for this dimension
            result = []
            for i in range(arr.shape[0]):
                result.append(build_nested(arr[i], depth + 1))
            return result

    nested_data = build_nested(data_full, 0)
    return nested_data


def load_bin5_with_metadata(filepath: str | Path) -> Tuple[any, Bin5Header]:
    """
    Load bin5 file and return both data and header.

    Parameters
    ----------
    filepath : str or Path
        Path to bin5 file

    Returns
    -------
    tuple
        (data - nested list or array, header object)
    """
    filepath = Path(filepath)
    header = read_bin5_header(filepath)
    data = load_bin5(filepath)
    return data, header


# ============================================================================
# BIN52 HIGH-LEVEL PARSER
# ============================================================================

def parse_bin52(
    filepath: str | Path,
    hour: Optional[float] = None,
    ls: Optional[float] = None,
    raw: bool = False,
    one_point: bool = False
) -> Dict[str, Any]:
    """
    Parse KRC bin52 output file.

    Parameters
    ----------
    filepath : str or Path
        Path to bin52 file (.t52)
    hour : float, optional
        Return specific hour (None for all hours)
    ls : float, optional
        Return specific solar longitude (None for all)
    raw : bool, optional
        Include raw bin5 data in output
    one_point : bool, optional
        Parse for one-point mode (TI derivation)

    Returns
    -------
    dict
        Structured output data with keys:
        - time: Time axis (local hours)
        - lat: Latitude values
        - elev: Elevation values
        - surf: Surface temperature [time, lat, season]
        - bol: Bolometer temperature [time, lat, season]
        - tatm: Atmospheric temperature [time, lat, season]
        - down_vis: Downward visible flux [time, lat, season]
        - down_ir: Downward IR flux [time, lat, season]
        - ls: Solar longitude values
        - layer: Layer properties
        - anc: Ancillary data including KRCCOM

    Notes
    -----
    The bin52 format structure from KRC documentation:
    - Shape: (N24_hours, 7_items, N4_lats, NDX+seasons, cases)
    - 7 items are: TSF, TPF, TAF, DOWNVIS, DOWNIR, field6, field7
    - 4th dimension contains NDX "extra" seasons before actual output seasons
    - Data starts at season index NDX (0-based Python index)
    """
    filepath = Path(filepath)

    # Load bin5 data
    data_raw, header = load_bin5_with_metadata(filepath)

    # Extract KRC version from header text
    version_info = _parse_krc_version(header.text)

    # Read the prefix metadata (first 4 values after 512-byte header)
    with open(filepath, 'rb') as f:
        f.seek(512)
        if version_info['major'] == 3:
            dtype_size = 8
            dtype = np.float64
        else:
            dtype_size = 4
            dtype = np.float32

        meta = np.frombuffer(f.read(4 * dtype_size), dtype=dtype)
        NWKRC = int(meta[0])   # Number of words in KRCCOM
        IDX = int(meta[1])     # Dimension index with extra values
        NDX = int(meta[2])     # Number of extra seasons
        NSOUT = int(meta[3])   # Number of output seasons

    # Parse KRCCOM from the file
    krccom = _parse_krccom_from_bin52(filepath, version_info['major'])

    # Data is now nested: data_raw[hour][field] = (lat, season, case) array
    # Extract dimensions from header
    n_time = len(data_raw)  # Number of hours
    n_fields = len(data_raw[0])  # Number of fields (should be 7)

    # Get shape of 3D block from first element
    first_block = data_raw[0][0]
    n_lat = first_block.shape[0]
    n_seasons_total = first_block.shape[1]
    n_cases = first_block.shape[2] if len(first_block.shape) > 2 else 1

    # Extract KRCCOM parameters
    N1 = int(krccom['id'][0])   # Number of layers
    N2 = int(krccom['id'][1])   # Calculations per day
    N3 = int(krccom['id'][2])   # Number of latitudes
    N24 = int(krccom['id'][5])  # Output timesteps per day

    # Use data shape for latitude count (KRCCOM lats may be zeros)
    actual_n_lats = n_lat if n_lat > 0 else max(1, N3)

    # Output seasons start after NDX prefix seasons
    # NSOUT is the number of output seasons to extract
    n_output_seasons = NSOUT

    # Build output structure
    out = {}

    # Time axis
    out['time'] = np.linspace(24.0 / n_time, 24.0, n_time)

    # Coordinate arrays (use placeholders if KRCCOM values are all zeros)
    lats_nonzero = krccom['lats'][krccom['lats'] != 0]
    if len(lats_nonzero) > 0:
        out['lat'] = krccom['lats'][:actual_n_lats]
        out['elev'] = krccom['elevs'][:actual_n_lats]
    else:
        # KRCCOM has no lat/elev data, create placeholder
        out['lat'] = np.zeros(actual_n_lats)
        out['elev'] = np.zeros(actual_n_lats)

    # Extract temperature and flux data using nested structure
    # data_raw[hour][field] gives (lat, season, case) array
    # davinci: data[i][1][j,start:,k] where start=NDX+1 (1-based) = NDX (0-based)
    # Field indices: 0=TSF, 1=TPF, 2=TAF, 3=DOWNVIS, 4=DOWNIR, 5/6=special
    start_season = NDX

    # Build output arrays by looping through hours
    surf_list = []
    bol_list = []
    tatm_list = []
    down_vis_list = []
    down_ir_list = []

    for i in range(n_time):
        # data_raw[i][field] gives (lat, season, case) array
        # Extract season range [start_season:start_season+n_output_seasons]
        # For single latitude: lat_idx=0, case_idx=0
        surf_list.append(data_raw[i][0][0, start_season:start_season+n_output_seasons, 0])
        bol_list.append(data_raw[i][1][0, start_season:start_season+n_output_seasons, 0])
        tatm_list.append(data_raw[i][2][0, start_season:start_season+n_output_seasons, 0])
        down_vis_list.append(data_raw[i][3][0, start_season:start_season+n_output_seasons, 0])
        down_ir_list.append(data_raw[i][4][0, start_season:start_season+n_output_seasons, 0])

    # Convert to numpy arrays
    out['surf'] = np.array(surf_list).squeeze()
    out['bol'] = np.array(bol_list).squeeze()
    out['tatm'] = np.array(tatm_list).squeeze()
    out['down_vis'] = np.array(down_vis_list).squeeze()
    out['down_ir'] = np.array(down_ir_list).squeeze()

    # Ensure arrays have at least 2D shape for consistency
    for key in ['surf', 'bol', 'tatm', 'down_vis', 'down_ir']:
        if out[key].ndim == 1:
            out[key] = out[key][:, np.newaxis]

    # Extract ancillary data from special fields
    anc = {}

    # Field 5 and 6 contain special data at specific timesteps
    # Hour 1 (index 0): converge_days, frost
    if n_time >= 1:
        anc['converge_days'] = data_raw[0][5][0, start_season:start_season+n_output_seasons, 0].squeeze()
        anc['frost'] = data_raw[0][6][0, start_season:start_season+n_output_seasons, 0].squeeze()

    # Hour 2 (index 1): delta_t_rms, frost_alb
    if n_time >= 2:
        anc['delta_t_rms'] = data_raw[1][5][0, start_season:start_season+n_output_seasons, 0].squeeze()
        anc['frost_alb'] = data_raw[1][6][0, start_season:start_season+n_output_seasons, 0].squeeze()

    # Hour 3 (index 2): tatm_predict, avg_heat_flow
    if n_time >= 3:
        anc['tatm_predict'] = data_raw[2][5][0, start_season:start_season+n_output_seasons, 0].squeeze()
        anc['avg_heat_flow'] = data_raw[2][6][0, start_season:start_season+n_output_seasons, 0].squeeze()

    anc['krccom'] = krccom
    out['anc'] = anc

    # Calculate layer properties
    layer = _calculate_layer_properties(krccom)

    # Extract layer min/max temperatures if available
    # Starting at hour 3 (index 2+1=3), fields 5 and 6 contain layer temps
    NumLayers = min(N1 // N24 - 2, n_time - 3) if N24 > 0 else 0
    if NumLayers > 0:
        tmin_list = []
        tmax_list = []
        for i in range(3, 3 + NumLayers):
            if i < n_time:
                tmin_list.append(data_raw[i][5][0, start_season:start_season+n_output_seasons, 0])
                tmax_list.append(data_raw[i][6][0, start_season:start_season+n_output_seasons, 0])
        if tmin_list:
            layer['tmin'] = np.array(tmin_list).squeeze()
            layer['tmax'] = np.array(tmax_list).squeeze()

    out['layer'] = layer

    # Extract solar longitude from prefix data (in season dimension)
    # For now, use placeholder
    out['ls'] = np.array([0.0])

    if raw:
        out['_raw_bin5'] = data_raw
        out['_header'] = header

    return out


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

def _parse_krc_version(header_text: str) -> Dict[str, int]:
    """Extract KRC version from header text."""
    vpos = header_text.find("KRCv")
    if vpos == -1:
        raise ValueError("Invalid bin52 file: no version string")

    major = int(header_text[vpos + 4])
    minor = int(header_text[vpos + 6])
    revision = int(header_text[vpos + 8])

    return {'major': major, 'minor': minor, 'revision': revision}


def _parse_krccom_from_bin52(filepath: Path, version: int) -> Dict[str, Any]:
    """
    Parse KRCCOM block from bin52 file.

    Parameters
    ----------
    filepath : Path
        Path to bin52 file
    version : int
        KRC major version (2 or 3)

    Returns
    -------
    dict
        KRCCOM parameters
    """
    krccom = {}

    # Determine dtype
    if version == 3:
        dtype_size = 8
    elif version == 2:
        dtype_size = 4
    else:
        raise ValueError(f"Unsupported KRC version: {version}")

    with open(filepath, 'rb') as f:
        # Skip header (512 bytes) and 4 metadata values
        f.seek(512 + 4 * dtype_size)

        if version == 3:
            # Version 3 format
            fd = np.frombuffer(f.read(96 * 8), dtype=np.float64)
            lats = np.frombuffer(f.read(37 * 8), dtype=np.float64)
            elevs = np.frombuffer(f.read(37 * 8), dtype=np.float64)
            id_vals = np.frombuffer(f.read(40 * 4), dtype=np.uint32)
            ld_vals = np.frombuffer(f.read(20 * 4), dtype=np.uint32)
            title = f.read(80).decode('ascii', errors='ignore').strip()
            runtime = f.read(24).decode('ascii', errors='ignore').strip()

        elif version == 2:
            # Version 2 format
            fd = np.frombuffer(f.read(96 * 4), dtype=np.float32).astype(np.float64)
            id_vals = np.frombuffer(f.read(40 * 4), dtype=np.uint32)
            ld_vals = np.frombuffer(f.read(20 * 4), dtype=np.uint32)
            title = f.read(80).decode('ascii', errors='ignore').strip()
            runtime = f.read(20).decode('ascii', errors='ignore').strip()
            lats = np.frombuffer(f.read(37 * 4), dtype=np.float32).astype(np.float64)
            elevs = np.frombuffer(f.read(37 * 4), dtype=np.float32).astype(np.float64)

        krccom['fd'] = fd
        krccom['id'] = id_vals
        krccom['ld'] = ld_vals.astype(bool)
        krccom['lats'] = lats
        krccom['elevs'] = elevs
        krccom['title'] = title
        krccom['runtime'] = runtime

    return krccom


def _calculate_layer_properties(krccom: Dict[str, Any]) -> Dict[str, Any]:
    """
    Calculate layer thickness, depth, and thermal properties.

    Parameters
    ----------
    krccom : dict
        KRCCOM parameters from bin52

    Returns
    -------
    dict
        Layer properties including:
        - thickness_m: Layer thickness in meters
        - thickness_sd: Layer thickness in skin depths
        - center_m: Center position in meters
        - center_sd: Center position in skin depths
        - top_m: Top position in meters
        - top_sd: Top position in skin depths
        - diffusivity: Thermal diffusivity
        - skin_depth_1: Skin depth for material 1
        - skin_depth_2: Skin depth for material 2
    """
    # Extract parameters
    N1 = int(krccom['id'][0])      # Number of layers
    N24 = int(krccom['id'][5])     # ?
    IC2 = int(krccom['id'][7])     # Layer index for material 2

    INERTIA = krccom['fd'][2]      # Thermal inertia
    COND2 = krccom['fd'][3]        # Conductivity layer 2
    DENS2 = krccom['fd'][4]        # Density layer 2
    PERIOD = krccom['fd'][5]       # Orbital period
    SPHT = krccom['fd'][6]         # Specific heat
    DENS = krccom['fd'][7]         # Density
    SPHT2 = krccom['fd'][15]       # Specific heat layer 2
    RLAY = krccom['fd'][32]        # Layer ratio
    FLAY = krccom['fd'][33]        # First layer size
    LOCAL = krccom['ld'][14]       # Local flag

    # Calculate conductivity and diffusivity
    COND = INERTIA**2 / (DENS * SPHT)
    INERTIA2 = np.sqrt(COND2 * DENS2 * SPHT2)
    PERSEC = PERIOD * 86400.0
    DIFFU = COND / (DENS * SPHT)
    DIFF2 = COND2 / (DENS2 * SPHT2)
    SCAL1 = np.sqrt(DIFFU * PERSEC / np.pi)
    SCAL2 = np.sqrt(DIFF2 * PERSEC / np.pi)

    # Diffusivity array
    DIFF = np.full(N1, DIFFU)
    if IC2 > 1 and IC2 < N1:
        DIFF[IC2-1:] = DIFF2

    # Layer progression in skin depths
    yy = FLAY * RLAY ** np.arange(N1)

    # Layer thickness in meters
    if LOCAL:
        TLAY = yy * np.sqrt(DIFF * PERSEC / np.pi)
    else:
        TLAY = yy * np.sqrt(DIFF[0] * PERSEC / np.pi)

    # Depth to center in meters
    CDEPTHS = np.zeros(N1)
    CDEPTHS[0] = -TLAY[0] / 2.0
    for i in range(1, N1):
        CDEPTHS[i] = CDEPTHS[i-1] - TLAY[i-1]/2.0 - TLAY[i]/2.0

    # Top depths
    TDEPTHS = CDEPTHS + TLAY / 2.0

    # Center depths in skin depths
    if LOCAL:
        CDEPTHS_SD = CDEPTHS / np.sqrt(DIFF * PERSEC / np.pi)
    else:
        CDEPTHS_SD = CDEPTHS / SCAL1

    # Top depths in skin depths
    if LOCAL:
        TDEPTHS_SD = TDEPTHS / np.sqrt(DIFF * PERSEC / np.pi)
    else:
        TDEPTHS_SD = TDEPTHS / SCAL1

    return {
        "thickness_m": TLAY,
        "thickness_sd": yy,
        "center_m": CDEPTHS,
        "center_sd": CDEPTHS_SD,
        "top_m": TDEPTHS,
        "top_sd": TDEPTHS_SD,
        "diffusivity": DIFF,
        "skin_depth_1": SCAL1,
        "skin_depth_2": SCAL2,
    }
