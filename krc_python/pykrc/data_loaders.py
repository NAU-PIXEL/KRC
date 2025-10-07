"""Data loaders for KRC support files."""

from pathlib import Path
from typing import Dict, Any, List, Optional
import numpy as np
import pandas as pd

try:
    import h5py
    HAS_HDF5 = True
except ImportError:
    HAS_HDF5 = False


class DataCache:
    """Singleton cache for loaded data files."""

    _instance: Optional['DataCache'] = None
    _cache: Dict[str, Any] = {}

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def get(self, key: str) -> Optional[Any]:
        """Get cached data by key."""
        return self._cache.get(key)

    def set(self, key: str, value: Any) -> None:
        """Set cached data by key."""
        self._cache[key] = value

    def clear(self) -> None:
        """Clear all cached data."""
        self._cache.clear()


def read_ascii_table(filepath: Path) -> List[str]:
    """
    Read ASCII table file as list of lines.

    Parameters
    ----------
    filepath : Path
        Path to the ASCII file

    Returns
    -------
    list of str
        List of lines from the file
    """
    with open(filepath, 'r') as f:
        return [line.rstrip('\n') for line in f.readlines()]


def read_csv_table(filepath: Path) -> pd.DataFrame:
    """
    Read CSV file into pandas DataFrame.

    Parameters
    ----------
    filepath : Path
        Path to the CSV file

    Returns
    -------
    pandas.DataFrame
        DataFrame containing the CSV data
    """
    return pd.read_csv(filepath)


def read_hdf5_file(filepath: Path) -> Dict[str, Any]:
    """
    Read HDF5 file and return contents as dictionary.

    Parameters
    ----------
    filepath : Path
        Path to the HDF5 file

    Returns
    -------
    dict
        Dictionary containing HDF5 datasets

    Raises
    ------
    ImportError
        If h5py is not installed
    """
    if not HAS_HDF5:
        raise ImportError("h5py is required to read HDF5 files. Install with: pip install h5py")

    data = {}

    def extract_datasets(name, obj):
        if isinstance(obj, h5py.Dataset):
            # Store dataset as numpy array
            data[name] = obj[()]
        elif isinstance(obj, h5py.Group):
            # Recursively process groups
            pass

    with h5py.File(filepath, 'r') as f:
        f.visititems(extract_datasets)

    return data


def read_vicar_image(filepath: Path) -> np.ndarray:
    """
    Read VICAR format image file.

    Note: This is a simplified reader. For full VICAR support,
    consider using specialized libraries like pvl or rasterio.

    Parameters
    ----------
    filepath : Path
        Path to the VICAR file

    Returns
    -------
    numpy.ndarray
        Image data array

    Raises
    ------
    NotImplementedError
        VICAR format requires specialized parsing
    """
    # TODO: Implement full VICAR reader or use external library
    # For now, attempt to read as raw binary after header
    raise NotImplementedError(
        "VICAR format reader not fully implemented. "
        "Consider using pvl, gdal, or rasterio libraries for VICAR support."
    )


class KRCDataLoader:
    """Loader for KRC support data files with caching."""

    def __init__(self, support_dir: Path):
        """
        Initialize data loader.

        Parameters
        ----------
        support_dir : Path
            Path to the krc_support directory
        """
        self.support_dir = Path(support_dir)
        self.cache = DataCache()

    def load_standish_table(self) -> List[str]:
        """Load standish.tab orbital elements table."""
        key = "standish"
        if cached := self.cache.get(key):
            return cached

        data = read_ascii_table(self.support_dir / "standish.tab")
        self.cache.set(key, data)
        return data

    def load_spinaxis_table(self) -> List[str]:
        """Load spinaxis.tab rotation axis table."""
        key = "spinaxis"
        if cached := self.cache.get(key):
            return cached

        data = read_ascii_table(self.support_dir / "spinaxis.tab")
        self.cache.set(key, data)
        return data

    def load_planetary_params(self) -> pd.DataFrame:
        """Load planetary_params3.csv parameter table."""
        key = "planetary_params"
        if cached := self.cache.get(key):
            return cached

        data = read_csv_table(self.support_dir / "planetary_params3.csv")
        self.cache.set(key, data)
        return data

    def load_small_bodies(self) -> Dict[str, Any]:
        """Load small_bodies.hdf database."""
        key = "small_bodies"
        if cached := self.cache.get(key):
            return cached

        data = read_hdf5_file(self.support_dir / "small_bodies.hdf")
        self.cache.set(key, data)
        return data

    def load_comets(self) -> Dict[str, Any]:
        """Load comets.hdf database."""
        key = "comets"
        if cached := self.cache.get(key):
            return cached

        data = read_hdf5_file(self.support_dir / "comets.hdf")
        self.cache.set(key, data)
        return data

    def load_porb_master(self) -> Dict[str, Any]:
        """Load porb_master.hdf orbital database."""
        key = "porb_master"
        if cached := self.cache.get(key):
            return cached

        data = read_hdf5_file(self.support_dir / "porb_master.hdf")
        self.cache.set(key, data)
        return data

    def load_var_header(self) -> List[str]:
        """Load var_header.ascii variable definitions."""
        key = "var_header"
        if cached := self.cache.get(key):
            return cached

        data = read_ascii_table(self.support_dir / "var_header.ascii")
        self.cache.set(key, data)
        return data

    def load_inertia_map(self) -> np.ndarray:
        """Load thermal inertia map (VICAR format)."""
        key = "inertia_map"
        if cached := self.cache.get(key):
            return cached

        # Placeholder - actual implementation depends on VICAR reader
        raise NotImplementedError("VICAR map loading not yet implemented")

    def load_albedo_map(self) -> np.ndarray:
        """Load albedo map (VICAR format)."""
        key = "albedo_map"
        if cached := self.cache.get(key):
            return cached

        # Placeholder - actual implementation depends on VICAR reader
        raise NotImplementedError("VICAR map loading not yet implemented")

    def load_elevation_map(self) -> np.ndarray:
        """Load elevation map (VICAR format)."""
        key = "elevation_map"
        if cached := self.cache.get(key):
            return cached

        # Placeholder - actual implementation depends on VICAR reader
        raise NotImplementedError("VICAR map loading not yet implemented")

    def get_porb_defaults(self) -> List[str]:
        """Get list of default planetary body names."""
        return [
            "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn",
            "Uranus", "Neptune", "Pluto", "Phobos", "Deimos", "Europa",
            "Ganymede", "Io", "Callisto", "Bennu", "Ceres", "Vesta",
            "Pallas", "Juno", "1P-Halley", "9P-Tempel_1", "10P-Tempel_2",
            "81P-Wild_2", "22P-Kopff", "21P-Giacobini-Zinner",
            "19P-Borrelly", "103P-Hartley_2", "67P-Churyumov-Gerasimenko",
            "Dinkinesh"
        ]

    def load_porb_default(self, body_name: str) -> Dict[str, Any]:
        """
        Load default orbital parameters for a body.

        Parameters
        ----------
        body_name : str
            Name of the celestial body

        Returns
        -------
        dict
            Orbital parameters for the body
        """
        porb_file = self.support_dir / "porb_defaults" / f"{body_name}.porb.hdf"
        if not porb_file.exists():
            raise FileNotFoundError(f"No default porb file for body: {body_name}")

        return read_hdf5_file(porb_file)

    def clear_cache(self) -> None:
        """Clear all cached data."""
        self.cache.clear()
