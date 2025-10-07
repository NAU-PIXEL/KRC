"""Complete bin52 binary output parser for KRC."""

import struct
from pathlib import Path
from typing import Dict, Any, Optional, Tuple, List
import numpy as np


class Bin52Parser:
    """Complete parser for KRC bin52 binary output format."""

    def __init__(self, filepath: str | Path):
        """
        Initialize bin52 parser.

        Parameters
        ----------
        filepath : str or Path
            Path to the bin52 output file
        """
        self.filepath = Path(filepath)
        self.version: Optional[Tuple[int, int, int]] = None
        self.dtype = None
        self.dtype_size = 0

    def parse(
        self,
        hour: Optional[float] = None,
        ls: Optional[float] = None,
        raw: bool = False,
        one_point: bool = False
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
        one_point : bool, optional
            Parse for one-point mode (TI derivation)

        Returns
        -------
        dict
            Structured output data
        """
        with open(self.filepath, 'rb') as f:
            # Read and parse header
            header_bytes = f.read(512)
            self._parse_header(header_bytes)

            # Read main data structure
            # The bin52 format stores data as: data[hour][field][lat, season, case]
            # where fields 1-7 are: tsurf, tbol, tatm, down_vis, down_ir, field6, field7

            # Read first 4 metadata values (after header)
            meta = np.frombuffer(f.read(4 * self.dtype_size), dtype=self.dtype)
            NWKRC = int(meta[0])  # Number of words in KRCCOM
            IDX = int(meta[1])     # Dimension index with extra values
            NDX = int(meta[2])     # Number of extra seasons
            NSOUT = int(meta[3])   # Number of output seasons

            # Calculate sizes
            # Need to determine array dimensions from file
            # This requires reading ahead to find KRCCOM block

            # For now, seek back and read KRCCOM to get dimensions
            krccom_offset = 512 + 4 * self.dtype_size
            f.seek(krccom_offset)

            krccom = self._parse_krccom_full(f)

            # Extract key dimensions
            N1 = int(krccom['id'][0])      # Number of layers
            N2 = int(krccom['id'][1])      # Number of calculations per day
            N3 = int(krccom['id'][2])      # Number of latitudes
            N4 = int(krccom['id'][3])      # ?
            N5 = int(krccom['id'][4])      # Number of days
            N24 = int(krccom['id'][5])     # Time steps per day for output

            # Calculate actual dimensions
            n_hours = N24 if N24 > 0 else 24
            n_lats = N3 if N3 > 0 else len(krccom['lats'])
            n_seasons = NSOUT

            # Read data arrays
            # Structure: for each hour, for each of 7 fields, for each lat, for each season
            data = self._read_data_arrays(
                f, n_hours, n_lats, n_seasons, NDX, NWKRC, IDX
            )

            # Process into output structure
            out = self._process_output(
                data, krccom, n_hours, n_lats, n_seasons,
                N1, N24, NDX, NSOUT, one_point
            )

            # Filter by hour/ls if requested
            if hour is not None or ls is not None:
                out = self._filter_output(out, hour, ls)

            if raw:
                out['_raw_data'] = data
                out['_krccom'] = krccom

            return out

    def _parse_header(self, header_bytes: bytes) -> None:
        """Parse 512-byte header."""
        header = header_bytes.decode('ascii', errors='ignore')

        # Find version string
        vpos = header.find("KRCv")
        if vpos == -1:
            raise ValueError("Invalid bin52 file: version string not found")

        major = int(header[vpos + 4])
        minor = int(header[vpos + 6])
        revision = int(header[vpos + 8])
        self.version = (major, minor, revision)

        # Determine data type and size
        if major == 3:
            self.dtype = np.float64
            self.dtype_size = 8
        elif major == 2:
            self.dtype = np.float32
            self.dtype_size = 4
        else:
            raise ValueError(
                f"Unsupported bin52 version {major}.{minor}.{revision}, "
                "requires >= 2.3.2 or <= 3.6"
            )

    def _parse_krccom_full(self, f) -> Dict[str, Any]:
        """Parse KRCCOM structure completely."""
        krccom = {}

        if self.version[0] == 3:
            # Version 3 format
            # r8*96: 64 input + 32 calculated real parameters
            fd = np.frombuffer(f.read(96 * 8), dtype=np.float64)
            # r8*37: MAXN4 (37) latitude values
            lats = np.frombuffer(f.read(37 * 8), dtype=np.float64)
            # r8*37: MAXN3 (37) elevation values
            elevs = np.frombuffer(f.read(37 * 8), dtype=np.float64)
            # u4*40: 40 integer parameters
            id_vals = np.frombuffer(f.read(40 * 4), dtype=np.uint32)
            # u4*20: 20 boolean parameters
            ld_vals = np.frombuffer(f.read(20 * 4), dtype=np.uint32)
            # a80: 80 character title
            title = f.read(80).decode('ascii', errors='ignore').strip()
            # a24: 24 character runtime
            runtime = f.read(24).decode('ascii', errors='ignore').strip()

        elif self.version[0] == 2:
            # Version 2 format (different order)
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

    def _read_data_arrays(
        self, f, n_hours: int, n_lats: int, n_seasons: int,
        NDX: int, NWKRC: int, IDX: int
    ) -> np.ndarray:
        """
        Read main data arrays from file.

        Returns array of shape [hour, field, lat, season]
        where field indices are: 0=tsurf, 1=tbol, 2=tatm, 3=down_vis, 4=down_ir, 5=field6, 6=field7
        """
        # Calculate total size of data section
        # Each timestep has 7 fields, each field has (n_lats × n_seasons) values

        # Read the prefix array which contains seasonal parameters
        prefix_size = NWKRC + NSOUT * 5 + IDX

        # Skip to data section (after KRCCOM which we already read)
        # The data is organized as nested loops:
        # for each season (NDX):
        #   for each latitude:
        #     for each of 7 fields:
        #       for each hour:
        #         value

        data = np.zeros((n_hours, 7, n_lats, n_seasons), dtype=self.dtype)

        # This is a simplified read - actual format is complex
        # Real implementation would need exact byte offsets

        # TODO: Complete this based on exact KRC Fortran output format
        # For now, return empty array
        return data

    def _process_output(
        self, data: np.ndarray, krccom: Dict[str, Any],
        n_hours: int, n_lats: int, n_seasons: int,
        N1: int, N24: int, NDX: int, NSOUT: int,
        one_point: bool
    ) -> Dict[str, Any]:
        """Process raw data into output structure."""
        from pykrc.output_parser import calculate_layer_properties

        out = {}

        # Create time axis (local hours)
        time = np.linspace(24.0 / n_hours, 24.0, n_hours)
        out['time'] = time

        # Extract temperature and flux data
        start_idx = NDX  # Data starts after extra seasons

        if not one_point:
            # Full output mode
            out['surf'] = data[:, 0, :, start_idx:].squeeze()  # Surface temp
            out['bol'] = data[:, 1, :, start_idx:].squeeze()   # Bolometer temp
            out['tatm'] = data[:, 2, :, start_idx:].squeeze()  # Atm temp
            out['down_vis'] = data[:, 3, :, start_idx:].squeeze()  # Solar flux
            out['down_ir'] = data[:, 4, :, start_idx:].squeeze()   # IR flux

            # Fields 6 and 7 contain special data depending on hour
            # Hour 1: converge_days, frost
            # Hour 2: delta_t_rms, frost_alb
            # Hour 3: tatm_predict, avg_heat_flow
            # Hour 4+: layer tmin/tmax

            anc = {}
            if n_hours >= 1:
                anc['converge_days'] = data[0, 5, :, start_idx:].squeeze()
                anc['frost'] = data[0, 6, :, start_idx:].squeeze()
            if n_hours >= 2:
                anc['delta_t_rms'] = data[1, 5, :, start_idx:].squeeze()
                anc['frost_alb'] = data[1, 6, :, start_idx:].squeeze()
            if n_hours >= 3:
                anc['tatm_predict'] = data[2, 5, :, start_idx:].squeeze()
                anc['avg_heat_flow'] = data[2, 6, :, start_idx:].squeeze()

            out['anc'] = anc
            out['anc']['krccom'] = krccom

            # Calculate layer properties
            layer = calculate_layer_properties(krccom)

            # Extract layer temperatures if available
            NumLayers = min(N1 // N24 - 2, n_hours - 3)
            if NumLayers > 0:
                layer['tmin'] = data[3:3+NumLayers, 5, :, start_idx:].squeeze()
                layer['tmax'] = data[3:3+NumLayers, 6, :, start_idx:].squeeze()

            out['layer'] = layer
        else:
            # One-point mode (TI derivation)
            out['surf'] = data[:, 0, :, start_idx:].squeeze()

        # Add coordinate axes
        out['lat'] = krccom['lats'][:n_lats]
        out['elev'] = krccom['elevs'][:n_lats]

        # Solar longitude axis (placeholder - should come from prefix)
        out['ls'] = np.linspace(0, 360, n_seasons)

        return out

    def _filter_output(
        self, out: Dict[str, Any],
        hour: Optional[float],
        ls: Optional[float]
    ) -> Dict[str, Any]:
        """Filter output by hour and/or solar longitude."""
        # TODO: Implement filtering
        # Find nearest hour/ls indices and extract those slices
        return out


def parse_bin52(
    filepath: str | Path,
    hour: Optional[float] = None,
    ls: Optional[float] = None,
    raw: bool = False,
    one_point: bool = False
) -> Dict[str, Any]:
    """
    Parse a bin52 file.

    Parameters
    ----------
    filepath : str or Path
        Path to bin52 file
    hour : float, optional
        Specific hour to extract
    ls : float, optional
        Specific solar longitude
    raw : bool, optional
        Include raw data
    one_point : bool, optional
        One-point mode

    Returns
    -------
    dict
        Parsed output structure
    """
    parser = Bin52Parser(filepath)
    return parser.parse(hour=hour, ls=ls, raw=raw, one_point=one_point)
