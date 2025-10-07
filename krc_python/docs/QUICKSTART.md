# KRC Python Interface - Quick Start Guide

## Installation

### 1. Prerequisites

Before installing the Python interface, ensure you have:

- **Python 3.8+** installed
- **KRC Fortran code** built (see main KRC Makefile)
- **KRC support files** in `krc_support/` directory

### 2. Build KRC Fortran

```bash
cd /path/to/KRC
make
```

This creates the KRC executable in `src/`.

### 3. Install Python Package

```bash
cd krc_python
pip install -e .
```

For development with testing tools:
```bash
pip install -e ".[dev]"
```

### 4. Set Environment Variable

```bash
export KRC_HOME=/path/to/KRC
```

Or in Python:
```python
from pykrc import set_krc_home
set_krc_home("/path/to/KRC")
```

## Basic Usage

### Simple Mars Run

```python
from pykrc import pykrc

# Run thermal model for Mars equator
result = krc(
    lat=0.0,        # Latitude (degrees)
    lon=0.0,        # Longitude (degrees)
    body="Mars",    # Celestial body
    ls=270,         # Solar longitude (degrees)
    INERTIA=200,    # Thermal inertia (SI units)
    ALBEDO=0.25     # Surface albedo
)

# Access results
print(result)
```

### With More Parameters

```python
result = krc(
    lat=45.0,
    lon=180.0,
    body="Mars",
    ls=90,                  # Northern summer
    INERTIA=300,
    ALBEDO=0.20,
    TDEEP=200.0,           # Deep temperature (K)
    SLOPE=15.0,            # Surface slope (degrees)
    SLOAZI=180.0,          # Slope azimuth
    k_style="Mars",        # Conductivity model
    verbose=True           # Print progress
)
```

### Different Materials

```python
# Basalt surface
result = krc(
    lat=0.0, lon=0.0,
    body="Mars",
    ls=0,
    INERTIA=400,
    ALBEDO=0.25,
    Mat1="basalt",
    Mat2="basalt"
)

# Water ice surface
result = krc(
    lat=80.0, lon=0.0,
    body="Mars",
    ls=0,
    INERTIA=2000,
    ALBEDO=0.65,
    Mat1="H2O",
    Mat2="H2O"
)
```

## Available Materials

- `"basalt"` - Basaltic rock (default)
- `"H2O"` - Water ice
- `"CO2"` - Carbon dioxide ice

## Conductivity Models

- `"Mars"` - Mars style (sqrt(T) trend)
- `"Moon"` - Lunar style (T³ trend)
- `"Bulk"` - Bulk conductivity driven

## Supported Bodies

Currently implemented (partial):
- `"Mars"` - Mars
- `"Earth"` - Earth
- `"Phobos"` - Martian moon

See `krc.dvrc` line 64 for full list of supported bodies in Davinci interface.

## Output Structure

The `krc()` function returns a dictionary with:

```python
{
    'surf': Surface temperature array,
    'bol': Bolometer temperature,
    'tatm': Atmospheric temperature,
    'down_vis': Solar flux,
    'down_ir': IR flux,
    'time': Time axis,
    'ls': Solar longitude,
    'lat': Latitude,
    'lon': Longitude,
    'elev': Elevation,
    'body': Body name,
    'alb': Albedo,
    'porb': Orbital parameters,
    'layer': {
        'thickness_m': Layer thickness,
        'center_m': Layer centers,
        'top_m': Layer tops,
        ...
    },
    'anc': {
        'krccom': KRC parameters,
        ...
    }
}
```

## Running Examples

```bash
cd examples
python basic_usage.py
```

## Running Tests

```bash
pytest tests/
pytest tests/test_materials.py -v
```

## Troubleshooting

### KRC_HOME Not Set
```
RuntimeError: KRC_HOME not set
```
**Solution**: Set environment variable or use `set_krc_home()`

### KRC Executable Not Found
```
FileNotFoundError: KRC executable not found
```
**Solution**: Build KRC first with `make` in the `src/` directory

### Missing Support Files
```
FileNotFoundError: No such file or directory: 'krc_support/...'
```
**Solution**: Ensure `krc_support/` directory exists with all data files

### Import Error
```
ImportError: No module named 'krc'
```
**Solution**: Install package with `pip install -e .`

### h5py Not Installed
```
ImportError: h5py is required
```
**Solution**: `pip install h5py`

## Current Status

### ✅ Implemented (Extended)

1. **Bin5/Bin52 Format Reader** - Complete implementation based on ff_bin5.c
2. **KRCCOM Parsing** - Full extraction of KRC parameters
3. **Layer Calculations** - Depth, mass, thermal scale computations
4. **Orbital Table Parsers** - Standish.tab and spinaxis.tab parsing

### ⚠️ Partially Implemented

1. **Bin52 Data Arrays** - KRCCOM works, temperature/flux extraction needs mapping
2. **Orbital Mechanics** - Table parsers done, porbmn integration pending
3. **VICAR Maps** - Not yet implemented (requires gdal/rasterio)

### 📋 Pending

1. **Eclipse Modeling** - Not yet implemented
2. **One-Point Mode** - TI derivation not implemented
3. **TUN8 Modes** - Special output formats not supported

See [EXTENSION_SUMMARY.md](EXTENSION_SUMMARY.md) for detailed status.

## Getting Help

1. Check [SUMMARY.md](SUMMARY.md) for feature overview
2. Read [DEVELOPMENT.md](DEVELOPMENT.md) for technical details
3. Review Davinci interface in `../krc.dvrc` for reference
4. Examine test files in `tests/` for usage patterns
5. Run examples in `examples/` directory

## Next Steps

After getting basic usage working:

1. Review the output structure
2. Understand material property calculations
3. Explore different body parameters
4. Run seasonal variations
5. Experiment with slope effects

## Contributing

To contribute to development:

1. Read [DEVELOPMENT.md](DEVELOPMENT.md)
2. Focus on critical TODO items (bin52 parser, orbital mechanics)
3. Add tests for new features
4. Follow existing code patterns
5. Update documentation

## Resources

- **KRC Paper**: Piqueux et al. (2018) LPSC Abstract #1027
- **Davinci Interface**: `../krc.dvrc` (4,379 lines)
- **KRC Source**: `../src/` directory
- **Support Files**: `../krc_support/` directory

---

**Quick Reference Card**

```python
from pykrc import krc, set_krc_home

# Setup (once)
set_krc_home("/path/to/KRC")

# Basic run
result = krc(lat=0, lon=0, body="Mars", ls=270,
             INERTIA=200, ALBEDO=0.25)

# With options
result = krc(lat=45, lon=180, body="Mars", ls=90,
             INERTIA=300, ALBEDO=0.20,
             SLOPE=15, k_style="Mars",
             Mat1="basalt", verbose=True)
```

Good luck! 🚀
