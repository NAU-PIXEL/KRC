"""Ancillary data lookup functions for KRC (MOLA elevation, TES albedo, etc.)."""

import numpy as np
from pathlib import Path
from typing import Optional, Tuple
from .data_loaders import read_vicar_image
from .config import get_paths


class AncillaryDataCache:
    """Cache for ancillary data maps."""

    _albedo_map: Optional[np.ndarray] = None
    _elevation_map: Optional[np.ndarray] = None
    _inertia_map: Optional[np.ndarray] = None

    @classmethod
    def get_albedo_map(cls) -> np.ndarray:
        """Load and cache TES albedo map (360x720, 2 pixels per degree)."""
        if cls._albedo_map is None:
            paths = get_paths()
            cls._albedo_map = read_vicar_image(paths.albedo_map)
        return cls._albedo_map

    @classmethod
    def get_elevation_map(cls) -> np.ndarray:
        """Load and cache MOLA elevation map (360x720, 2 pixels per degree)."""
        if cls._elevation_map is None:
            paths = get_paths()
            # MOLA is in meters, divide by 1000 to get km (matches Davinci)
            cls._elevation_map = read_vicar_image(paths.elevation_map) / 1000.0
        return cls._elevation_map

    @classmethod
    def get_inertia_map(cls) -> np.ndarray:
        """Load and cache TES thermal inertia map (360x720, 2 pixels per degree)."""
        if cls._inertia_map is None:
            paths = get_paths()
            cls._inertia_map = read_vicar_image(paths.inertia_map)
        return cls._inertia_map


def latlon_to_pixel(lat: float, lon: float) -> Tuple[int, int]:
    """
    Convert lat/lon to pixel coordinates for 2ppd (2 pixels per degree) maps.

    Matches Davinci's geo_trans(lat, 360-lon, 2) calculation.

    Maps are 360 lines x 720 samples:
    - Latitude: -90 to +90 (180 degrees, 2 pixels/degree = 360 lines)
    - Longitude: 0 to 360 (360 degrees, 2 pixels/degree = 720 samples)

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to +90)
    lon : float
        East longitude in degrees (0 to 360)

    Returns
    -------
    tuple of int
        (line, sample) pixel coordinates
    """
    # Normalize longitude to 0-360 range
    lon = lon % 360.0

    # Clamp latitude to valid range
    lat = max(-90.0, min(90.0, lat))

    # Davinci uses geo_trans(lat, 360-lon, res=2, center=0)
    # geo_trans calculates (1-indexed):
    #   image_y = int((90 - lat) * res + 1)
    #   image_x = int((center + 180 - west_lon) * res) + 1
    #   if (center + 180 - west_lon) <= 0: image_x = int(360*res) + image_x - 1

    # Convert East longitude to West longitude
    west_lon = 360.0 - lon
    if west_lon == 360.0:
        west_lon = 0.0

    image_res = 2.0
    center = 0.0

    # Calculate 1-indexed coordinates
    image_y = int((90.0 - lat) * image_res + 1)
    image_x = int((center + 180.0 - west_lon) * image_res) + 1

    # Handle longitude wraparound
    if (center + 180.0 - west_lon) <= 0:
        image_x = int(360 * image_res) + image_x - 1

    # Convert to 0-indexed (Python/numpy indexing)
    # Note: Davinci arrays use different indexing convention
    # Empirically determined to match Davinci results:
    # For elevation at lat=12,lon=0: Davinci=-1.41km at [157,358]
    # For inertia at lat=25,lon=0: Davinci=55.0 at [130,359]
    line = image_y - 1  # Standard 1-indexed to 0-indexed conversion
    sample = 720 - image_x  # Reverse longitude direction and convert

    # Clamp to valid pixel range
    line = max(0, min(359, line))
    sample = max(0, min(719, sample))

    return (line, sample)


def lookup_elevation(lat: float, lon: float) -> float:
    """
    Look up MOLA elevation at given lat/lon.

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to +90)
    lon : float
        East longitude in degrees (0 to 360)

    Returns
    -------
    float
        Elevation in kilometers
    """
    elev_map = AncillaryDataCache.get_elevation_map()
    line, sample = latlon_to_pixel(lat, lon)
    return float(elev_map[line, sample])


def lookup_albedo(lat: float, lon: float) -> float:
    """
    Look up TES albedo at given lat/lon.

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to +90)
    lon : float
        East longitude in degrees (0 to 360)

    Returns
    -------
    float
        Albedo (0 to 1)
    """
    albedo_map = AncillaryDataCache.get_albedo_map()
    line, sample = latlon_to_pixel(lat, lon)
    return float(albedo_map[line, sample])


def lookup_inertia(lat: float, lon: float) -> float:
    """
    Look up TES thermal inertia at given lat/lon.

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to +90)
    lon : float
        East longitude in degrees (0 to 360)

    Returns
    -------
    float
        Thermal inertia in J m^-2 K^-1 s^-1/2
    """
    inertia_map = AncillaryDataCache.get_inertia_map()
    line, sample = latlon_to_pixel(lat, lon)
    return float(inertia_map[line, sample])


def get_ancillary_data(lat: float, lon: float) -> dict:
    """
    Get all ancillary data for a given lat/lon.

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to +90)
    lon : float
        East longitude in degrees (0 to 360)

    Returns
    -------
    dict
        Dictionary with keys: 'elevation' (km), 'albedo', 'inertia'
    """
    return {
        'elevation': lookup_elevation(lat, lon),
        'albedo': lookup_albedo(lat, lon),
        'inertia': lookup_inertia(lat, lon)
    }
