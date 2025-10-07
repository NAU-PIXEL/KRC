"""Parser for KRC binary output files (bin52 format)."""

import struct
from pathlib import Path
from typing import Dict, Any, Optional, Tuple
import numpy as np


class Bin52Parser:
    """Parser for KRC bin52 binary output format."""

    def __init__(self, filepath: str | Path):
        """
        Initialize bin52 parser.

        Parameters
        ----------
        filepath : str or Path
            Path to the bin52 output file
        """
        self.filepath = Path(filepath)
        self.version = None
        self.dtype = None

    def parse(
        self,
        hour: Optional[float] = None,
        ls: Optional[float] = None,
        raw: bool = False
    ) -> Dict[str, Any]:
        """
        Parse bin52 output file.

        Parameters
        ----------
        hour : float, optional
            Return specific hour (None for all hours)
        ls : float, optional
            Return specific solar longitude (None for all)
        raw : bool, optional
            Include raw data in output

        Returns
        -------
        dict
            Structured output data containing:
            - surf: Surface temperature (K) [Hours × Latitudes × Seasons]
            - bol: Bolometer temperature (K) [Hours × Latitudes × Seasons]
            - tatm: Atmosphere temperature (K) [Hours × Latitudes × Seasons]
            - down_vis: Solar flux (W/m²) [Hours × Latitudes × Seasons]
            - down_ir: IR downwelling flux (W/m²) [Hours × Latitudes × Seasons]
            - time: Local hour axis [Hours × 1 × 1]
            - ls: Solar longitude axis [1 × 1 × Seasons]
            - lat: Latitude axis [1 × Latitudes × 1]
            - elev: Elevation axis [1 × Latitudes × 1]
            - layer: Layer properties structure
            - anc: Ancillary data structure
        """
        with open(self.filepath, 'rb') as f:
            # Read header (512 bytes)
            header = f.read(512).decode('ascii', errors='ignore')

            # Extract version
            vpos = header.find("KRCv")
            if vpos == -1:
                raise ValueError("Invalid bin52 file: version string not found")

            major = int(header[vpos + 4])
            minor = int(header[vpos + 6])
            revision = int(header[vpos + 8])
            self.version = (major, minor, revision)

            # Determine data type
            if major == 3:
                self.dtype = np.float64
                dtype_size = 8
            elif major == 2:
                self.dtype = np.float32
                dtype_size = 4
            else:
                raise ValueError(
                    f"Unsupported bin52 version {major}.{minor}.{revision}, "
                    "requires >= 2.3.2 or <= 3.6"
                )

            # Read data array
            # Structure: data[hour][field][lat, season, case]
            # Where fields are: 1-7 (7 output variables per timestep)
            f.seek(512)  # Skip header

            # Read first 4 metadata values
            meta = np.frombuffer(f.read(4 * dtype_size), dtype=self.dtype)
            NWKRC = int(meta[0])  # Number of words in KRCCOM
            IDX = int(meta[1])     # Dimension index with extra values
            NDX = int(meta[2])     # Number of extra seasons
            NSOUT = int(meta[3])   # Number of output seasons

            # Read main data - this is simplified, actual format is complex
            # Full implementation would need to match the exact binary layout
            # used by the Fortran KRC code

            # Placeholder structure
            out = {
                "surf": None,
                "bol": None,
                "tatm": None,
                "down_vis": None,
                "down_ir": None,
                "time": None,
                "ls": None,
                "lat": None,
                "elev": None,
                "layer": {},
                "anc": {},
            }

            # TODO: Complete binary parsing
            # This requires detailed knowledge of the bin52 format:
            # 1. Parse data arrays based on N1, N2, N3, N4, N5
            # 2. Reshape into proper dimensions
            # 3. Extract KRCCOM parameters
            # 4. Calculate layer properties
            # 5. Filter by hour/ls if requested

            raise NotImplementedError(
                "Full bin52 parsing not yet implemented. "
                "This requires matching the exact binary layout from KRC Fortran code."
            )

        return out

    def _parse_krccom(self, f, case: int = 0) -> Dict[str, Any]:
        """
        Parse KRCCOM structure from bin52 file.

        Parameters
        ----------
        f : file
            Open file handle
        case : int
            Case number (for multi-case outputs)

        Returns
        -------
        dict
            KRCCOM parameters
        """
        krccom = {}

        if self.version[0] == 3:
            # Version 3 format
            # x8*4: First 4 descriptor values (already read)
            # r8*96: 64 input + 32 calculated real parameters
            # r8*37: MAXN4 (37) latitude values
            # r8*37: MAXN3 (37) elevation values
            # u4*40: 40 integer parameters
            # u4*20: 20 boolean parameters
            # a80: 80 character title
            # a24: 24 character runtime

            fd = np.frombuffer(f.read(96 * 8), dtype=np.float64)
            lats = np.frombuffer(f.read(37 * 8), dtype=np.float64)
            elevs = np.frombuffer(f.read(37 * 8), dtype=np.float64)
            id_vals = np.frombuffer(f.read(40 * 4), dtype=np.uint32)
            ld_vals = np.frombuffer(f.read(20 * 4), dtype=np.uint32)
            title = f.read(80).decode('ascii', errors='ignore').strip()
            runtime = f.read(24).decode('ascii', errors='ignore').strip()

        elif self.version[0] == 2:
            # Version 2 format
            fd = np.frombuffer(f.read(96 * 4), dtype=np.float32)
            id_vals = np.frombuffer(f.read(40 * 4), dtype=np.uint32)
            ld_vals = np.frombuffer(f.read(20 * 4), dtype=np.uint32)
            title = f.read(80).decode('ascii', errors='ignore').strip()
            runtime = f.read(20).decode('ascii', errors='ignore').strip()
            lats = np.frombuffer(f.read(37 * 4), dtype=np.float32)
            elevs = np.frombuffer(f.read(37 * 4), dtype=np.float32)

        krccom['fd'] = fd
        krccom['id'] = id_vals
        krccom['ld'] = ld_vals.astype(bool)
        krccom['lats'] = lats
        krccom['elevs'] = elevs
        krccom['title'] = title
        krccom['runtime'] = runtime

        return krccom


def calculate_layer_properties(krccom: Dict[str, Any]) -> Dict[str, Any]:
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
        - thickness: Layer thickness [m, skin depths]
        - center: Center position [m, skin depths]
        - top: Top position [m, skin depths]
        - center_mass: Columnar mass at center
        - mass_burden: Mass burden above layer
        - thermal_scales: Thermal scales above layer
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


def parse_bin52(
    filepath: str | Path,
    hour: Optional[float] = None,
    ls: Optional[float] = None,
    raw: bool = False
) -> Dict[str, Any]:
    """
    Convenience function to parse a bin52 file.

    Parameters
    ----------
    filepath : str or Path
        Path to the bin52 file
    hour : float, optional
        Specific hour to extract
    ls : float, optional
        Specific solar longitude to extract
    raw : bool, optional
        Include raw data

    Returns
    -------
    dict
        Parsed output structure
    """
    parser = Bin52Parser(filepath)
    return parser.parse(hour=hour, ls=ls, raw=raw)
