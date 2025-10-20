# KRC Python Interface (PyKRC)

Python interface for the **KRC (Kieffer Rapid Calculation)** thermal model, a sophisticated 1D planetary surface thermal modeling tool.

## Overview

PyKRC provides a Pythonic interface to the KRC Fortran thermal model, enabling scientists to run thermal calculations for planetary surfaces. It maintains **byte-exact compatibility** with the reference Davinci implementation while providing a modern, user-friendly API.

**Supported Bodies**: Mars, Moon, Phobos, Deimos, Europa, and custom bodies via orbital parameters

## Installation

```bash
pip install -e .
```

For development:
```bash
pip install -e ".[dev]"
```

## Requirements

- Python >= 3.8
- NumPy, SciPy (installed automatically)
- KRC Fortran executable (built from source)
- Support data files: `standish.tab`, `spinaxis.tab`, `PORBCM.mat`

## Environment Setup

Set `KRC_HOME` to point to your KRC installation:
```bash
export KRC_HOME=/path/to/KRC
```

The KRC Fortran executable should be at `$KRC_HOME/bin/krc` or in your PATH.

## Quick Start

### Basic Mars Run

```python
from pykrc import krc

# Run a simple Mars thermal model at equator
result = krc(
    lat=0.0,           # Latitude (degrees)
    lon=0.0,           # Longitude (degrees)
    body="Mars",       # Celestial body
    INERTIA=200,       # Thermal inertia (J m⁻² K⁻¹ s⁻½)
    ALBEDO=0.25,       # Surface albedo
)

# Access results
surf_temp = result['surf']  # Surface temperature array [hours, seasons]
print(f"Mean surface temp: {surf_temp.mean():.2f} K")
```

### Multiple Latitudes

```python
# Run for multiple latitudes
result = krc(
    body="Mars",
    N4=3,                          # Number of latitudes
    lat=[-30.0, 0.0, 30.0],       # Latitude array
    ELEV=[-2.0, 0.0, 1.5],        # Elevation array (km)
    INERTIA=200,
    ALBEDO=0.25,
)

# Results: surf_temp shape = [hours, latitudes, seasons]
print(f"Temperature at equator: {result['surf'][:, 1, :].mean():.2f} K")
```

## Common Parameters

### Key Parameters (Most Frequently Used)

| Parameter | Type | Default | Units | Description |
|-----------|------|---------|-------|-------------|
| `body` | str | "Mars" | - | Celestial body name |
| `lat` | float | 0.0 | degrees | Latitude (-90 to 90) |
| `lon` | float | 0.0 | degrees E | Longitude (0 to 360) |
| `ALBEDO` | float | 0.25 | - | Surface albedo (0-1) |
| `INERTIA` | float | 200 | J m⁻² K⁻¹ s⁻½ | Thermal inertia |
| `EMISS` | float | 1.0 | - | Thermal emissivity (0-1) |
| `TAUD` | float | 0.30 | - | Atmospheric opacity |
| `N1` | int | auto | - | Number of subsurface layers |
| `N2` | int | auto | - | Timesteps per day |
| `N5` | int | 120 | - | Number of seasons |
| `K4OUT` | int | 52 | - | Output format (52 recommended) |

### Body-Specific Defaults

**Mars**:
```python
{
    'PERIOD': 1.0275,     # Sol length (days)
    'PTOTAL': 546.0,      # Atmospheric pressure (Pa)
    'GRAV': 3.727,        # Surface gravity (m/s²)
    'SOLCON': 589.0,      # Solar constant at Mars (W/m²)
    'Mat1': 'basalt',     # Default material
    'k_style': 'Mars',    # k(T) formulation
}
```

**Moon**:
```python
{
    'PERIOD': 29.5,       # Lunar day (Earth days)
    'PTOTAL': 0.0,        # No atmosphere
    'GRAV': 1.62,         # Surface gravity (m/s²)
    'Mat1': 'basalt',     # Default material
    'k_style': 'Moon',    # k(T) formulation (Hayne et al.)
}
```

**Europa**:
```python
{
    'PERIOD': 3.551,      # Rotation period (days)
    'PTOTAL': 0.0,        # Thin atmosphere ignored
    'Mat1': 'H2O',        # Water ice
    'ALBEDO': 0.67,       # High ice albedo
    'INERTIA': 100,       # Ice-like inertia
}
```

## Common Configuration Patterns

### High-Resolution Mars Run

```python
result = krc(
    body="Mars",
    lat=22.3,             # VL1 latitude
    lon=312.05,           # VL1 longitude
    INERTIA=250,          # Rocky site
    ALBEDO=0.22,          # Measured albedo
    N1=30,                # Deep layers
    N2=384,               # High temporal resolution
    N5=120,               # Full Mars year
    K4OUT=52,             # Comprehensive output
)
```

### Two-Layer Regolith

```python
result = krc(
    body="Mars",
    lat=0.0,
    thick=0.05,           # 5 cm upper layer
    INERTIA=100,          # Dusty upper layer
    INERTIA2=400,         # Rocky lower layer
    Mat1='dust',
    Mat2='basalt',
)
```

### Moon with Slope

```python
result = krc(
    body="Moon",
    lat=-85.0,            # South polar region
    SLOPE=10.0,           # 10° slope
    SLOAZI=180.0,         # South-facing
    INERTIA=50,           # Lunar regolith
    ALBEDO=0.12,          # Low albedo
)
```

### Custom Body

```python
result = krc(
    body="generic",
    PERIOD=24.0,          # 24-hour day
    GRAV=9.8,             # Earth gravity
    SOLCON=1361.0,        # Solar constant at 1 AU
    PTOTAL=101325.0,      # Earth atmosphere
    LPORB=False,          # Skip orbital calculations
)
```

## Fast Testing Configuration

For quick tests (< 1 second):

```python
result = krc(
    body="Mars",
    N1=10,                # Minimal layers
    N2=96,                # Low resolution
    N3=3,                 # Few iteration days
    N4=1,                 # Single latitude
    N5=3,                 # Few seasons
    NRSET=1,              # Quick spin-up
)
```

## Output Structure

```python
result = krc(...)

# Available outputs:
result['surf']         # Surface temperature [hours, lats, seasons]
result['tbol']         # Bolometric temperature
result['tatm']         # Atmospheric temperature
result['down_vis']     # Downwelling visible flux
result['down_ir']      # Downwelling IR flux
result['time']         # Time axis
result['lat']          # Latitude array
result['layer']        # Layer information (depths, temps)
```

## Troubleshooting

### Common Issues

**"KRC executable not found"**:
- Set `KRC_HOME` environment variable
- Ensure KRC binary is compiled and in `$KRC_HOME/bin/`

**"Missing data files (standish.tab, etc.)"**:
- Copy files from `krc_support/` to `$KRC_HOME/run/`
- Or specify path with `data_dir` parameter

**"SIGKILL or timeout"**:
- Reduce parameters (use fast test configuration)
- Increase timeout: `krc(..., timeout=600)`
- Check `CONVF >= 2.0` for stability

**"Input file mismatch / wrong results"**:
- Verify parameter alignment
- Check that RLAY=1.15, FLAY=0.10 (not other values)
- Review input file with `debug=True`

## Documentation

- **[KRC_ARCHITECTURE.md](docs/KRC_ARCHITECTURE.md)** - Technical architecture and runtime environment
- **[PARAMETER_REFERENCE.md](docs/PARAMETER_REFERENCE.md)** - Complete parameter documentation
- **[CLAUDE.md](CLAUDE.md)** - Developer guidelines and conventions
- **[Examples](examples/)** - Usage examples and case studies

## Citation

If you use this software, please cite:

> Improving Thermal Model Capability for the Planetary Science Community,
> S. Piqueux, C. S. Edwards, R. L. Fergason, J. Laura, A. Weintraub, P. R. Christensen, H. H. Kieffer
> 49th Lunar and Planetary Science Conference (2018), Abstract #1027

## License

Copyright 2018-2025. All Rights reserved.
Part of this work was performed at the Jet Propulsion Laboratory, California Institute of Technology under a contract with NASA.
