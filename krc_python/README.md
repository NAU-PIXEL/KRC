# KRC Python Interface

Python interface for the KRC (Kieffer Rapid Calculation) thermal model, a sophisticated planetary surface thermal modeling tool.

## Overview

This package provides a Pythonic interface to the KRC Fortran thermal model, similar to the existing Davinci interface. It enables scientists to run thermal calculations for planetary surfaces with support for various celestial bodies including planets, moons, asteroids, and comets.

## Installation

```bash
pip install -e .
```

For development:
```bash
pip install -e ".[dev]"
```

## Quick Start

```python
from pykrc import krc

# Run a simple Mars thermal model
result = krc(
    lat=0.0,           # Latitude (degrees)
    lon=0.0,           # Longitude (degrees)
    body="Mars",       # Celestial body
    ls=270,            # Solar longitude (degrees)
    INERTIA=200,       # Thermal inertia (SI)
    ALBEDO=0.25,       # Surface albedo
)

# Access results
print(f"Surface temperature: {result['temp']}")
```

## Requirements

- Python >= 3.8
- KRC Fortran executable (built from source)
- Support data files in `krc_support/` directory

## Environment Variables

Set `KRC_HOME` to point to your KRC installation:
```bash
export KRC_HOME=/path/to/KRC
```

## Features

- **Material Properties**: Automatic calculation of thermal properties
- **Orbital Mechanics**: Support for planetary orbital calculations
- **Multiple Bodies**: Planets, moons, asteroids, comets
- **Eclipse Modeling**: Eclipse and planetary flux calculations
- **Output Formats**: Parse bin52 and other KRC output formats

## Documentation

- [Quick Start Guide](docs/QUICKSTART.md) - Get started quickly
- [Development Guide](docs/DEVELOPMENT.md) - Developer documentation
- [Implementation Status](docs/IMPLEMENTATION_STATUS.md) - Complete status
- [Binary Format Spec](docs/BIN5_FORMAT.md) - Bin5/Bin52 format details
- [Examples](examples/) - Usage examples

## Citation

If you use this software, please cite:

> Improving Thermal Model Capability for the Planetary Science Community,
> S. Piqueux, C. S. Edwards, R. L. Fergason, J. Laura, A. Weintraub, P. R. Christensen, H. H. Kieffer
> 49th Lunar and Planetary Science Conference (2018), Abstract #1027

## License

Copyright 2018-2025. All Rights reserved.
Part of this work was performed at the Jet Propulsion Laboratory, California Institute of Technology under a contract with NASA.
