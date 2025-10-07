# KRC Python Interface - Implementation Summary

## Overview

A Python interface for the KRC (Kieffer Rapid Calculation) thermal model has been created, mirroring the functionality of the existing Davinci interface (`krc.dvrc`). This implementation provides a Pythonic API for running planetary surface thermal models.

## Project Structure

```
krc_python/
├── krc/                          # Main package
│   ├── __init__.py              # Package initialization
│   ├── config.py                # Configuration and paths (155 lines)
│   ├── core.py                  # Main krc() function (271 lines)
│   ├── input_processor.py       # master.inp parser (214 lines)
│   ├── data_loaders.py          # Support file loaders (223 lines)
│   ├── materials.py             # Material properties (289 lines)
│   ├── orbital.py               # Orbital mechanics (260 lines)
│   ├── executor.py              # KRC execution engine (356 lines)
│   ├── output_parser.py         # Bin52 output parser (303 lines)
│   └── data/                    # Support data (to be linked)
├── tests/                        # Unit tests
│   ├── __init__.py
│   ├── test_config.py           # Configuration tests
│   └── test_materials.py        # Material property tests
├── examples/                     # Usage examples
│   └── basic_usage.py           # Basic usage demonstrations
├── setup.py                      # Package setup
├── README.md                     # User documentation
├── DEVELOPMENT.md               # Developer guide
├── SUMMARY.md                   # This file
└── .gitignore                   # Git ignore rules

Total: ~2,500 lines of Python code
```

## Implemented Features

### Core Functionality ✓

1. **Configuration Management** ([config.py](krc_python/krc/config.py))
   - KRC_HOME path management
   - Environment variable support
   - Path utilities for all KRC directories
   - Support file path accessors

2. **Input Processing** ([input_processor.py](krc_python/krc/input_processor.py))
   - Full master.inp file parser
   - Parameter block extraction
   - Logical flag handling
   - Latitude/elevation parsing

3. **Data Loading** ([data_loaders.py](krc_python/krc/data_loaders.py))
   - HDF5 file reader
   - CSV table loader
   - ASCII file reader
   - Data caching system
   - Support for: standish.tab, spinaxis.tab, planetary_params3.csv
   - Database access for: small_bodies.hdf, comets.hdf, porb_master.hdf

4. **Material Properties** ([materials.py](krc_python/krc/materials.py))
   - Temperature-dependent properties (Cp, k, ρ)
   - Material database (H2O, basalt, CO2)
   - Polynomial coefficient fitting
   - Conductivity models: "Mars", "Moon", "Bulk"
   - Porosity calculations
   - Thermal inertia relationships

5. **Orbital Mechanics** ([orbital.py](krc_python/krc/orbital.py))
   - Body type identification
   - Orbital elements structure
   - Generic orbital parameters
   - Solar longitude calculations
   - Julian date conversions
   - Default body parameter loading

6. **Execution Engine** ([executor.py](krc_python/krc/executor.py))
   - KRC input file generation
   - Fortran binary execution
   - Cross-platform support (Unix/Windows)
   - Working directory management
   - Timeout handling
   - Error detection

7. **Output Parsing** ([output_parser.py](krc_python/krc/output_parser.py))
   - Bin52 format detection
   - Version handling (2.x, 3.x)
   - KRCCOM parameter extraction
   - Layer property calculations
   - Data structure definitions

8. **Main Interface** ([core.py](krc_python/krc/core.py))
   - Pythonic `krc()` function
   - Parameter validation
   - Smart defaults from master.inp
   - Material property integration
   - Orbital parameter integration
   - Comprehensive error handling

### Testing & Documentation ✓

- Unit tests for configuration and materials
- Basic usage examples
- Installation instructions
- API documentation (docstrings)
- Developer guide with TODO items

## Usage Example

```python
from pykrc import pykrc

# Run Mars thermal model
result = krc(
    lat=0.0,           # Equator
    lon=0.0,           # Prime meridian
    body="Mars",
    ls=270,            # Solar longitude
    INERTIA=200,       # Thermal inertia (SI)
    ALBEDO=0.25,       # Albedo
    verbose=True
)

# Access results
print(result['surf'])  # Surface temperature
print(result['body'])  # Body name
print(result['porb'])  # Orbital parameters
```

## Comparison with Davinci Interface

| Feature | Davinci (krc.dvrc) | Python | Status |
|---------|-------------------|--------|--------|
| Main krc() function | ✓ (line 85) | ✓ (core.py) | Complete |
| master.inp parsing | ✓ (krc_process_input) | ✓ (input_processor.py) | Complete |
| Material properties | ✓ (Mat_Prop) | ✓ (materials.py) | Complete |
| Orbital mechanics | ✓ (porb, generic_porb) | ✓ (orbital.py) | Simplified* |
| KRC execution | ✓ (system calls) | ✓ (executor.py) | Complete |
| bin52 parsing | ✓ (process_bin52) | ✓ (output_parser.py) | Partial* |
| Data caching | ✓ (global vars) | ✓ (DataCache) | Complete |
| Eclipse modeling | ✓ (krc_eclipse) | ✗ | TODO |
| Planetary flux | ✓ (krc_planetary_flux_*) | ✗ | TODO |
| VICAR maps | ✓ (load maps) | ✗ | TODO* |
| One-point mode | ✓ (TI derivation) | ✗ | TODO |

\* = Critical items requiring completion (see DEVELOPMENT.md)

## Key Differences from Davinci

1. **Type Safety**: Python implementation uses type hints and dataclasses
2. **Error Handling**: Explicit exceptions vs. Davinci's return codes
3. **API Style**: Pythonic kwargs vs. Davinci's positional parameters
4. **Data Structures**: NumPy arrays + dicts vs. Davinci's arrays
5. **Execution**: subprocess module vs. system() calls
6. **Caching**: Singleton pattern vs. global variables

## Critical TODO Items

### Must Complete for Full Functionality

1. **Bin52 Binary Parser** (HIGH PRIORITY)
   - Current implementation is a placeholder
   - Need exact binary format matching
   - Multi-dimensional array reshaping
   - Multi-case output support
   - Reference: krc.dvrc lines 3283-3750

2. **Orbital Mechanics** (HIGH PRIORITY)
   - Full standish.tab/spinaxis.tab parsing
   - porbmn executable integration
   - Rotation matrix calculations
   - All body types (planets, satellites, asteroids, comets)
   - Reference: krc.dvrc lines 2205-2860

3. **VICAR Format Support** (MEDIUM PRIORITY)
   - Image reader implementation
   - Integration with pvl/gdal/rasterio
   - Thermal inertia map loading
   - Albedo map loading
   - Elevation map loading
   - Spatial queries for lat/lon

4. **Advanced Features** (MEDIUM PRIORITY)
   - Eclipse modeling (krc_eclipse)
   - Planetary flux calculations
   - One-point TI derivation mode
   - CROCUS function
   - Frost modeling

## Dependencies

### Required
- Python >= 3.8
- numpy >= 1.20.0
- pandas >= 1.3.0
- h5py >= 3.0.0

### Optional
- astropy >= 5.0.0 (astronomical calculations)
- pytest >= 7.0.0 (testing)
- gdal or rasterio (VICAR support)

### System Requirements
- KRC Fortran executable (built from source)
- KRC support files in krc_support/
- Unix-like OS or Windows with compatible toolchain

## Installation

```bash
cd krc_python
pip install -e .

# For development
pip install -e ".[dev]"

# For astronomy features
pip install -e ".[astro]"
```

## Testing

```bash
# Run tests
pytest tests/

# With coverage
pytest --cov=krc tests/

# Specific test
pytest tests/test_materials.py -v
```

## Performance Considerations

- **Data Caching**: Singleton cache reduces file I/O
- **Lazy Loading**: Support files loaded on demand
- **Binary Parsing**: Needs optimization for large outputs
- **Parallel Execution**: Not yet implemented (future enhancement)

## Validation Strategy

To validate the Python interface:

1. **Unit Tests**: Test individual components
2. **Integration Tests**: Full krc() execution
3. **Comparison Tests**: Match Davinci outputs exactly
4. **Regression Tests**: Known good configurations

## Known Limitations

1. **VICAR Maps**: Cannot load spatial data yet
2. **Bin52 Parsing**: Incomplete, needs full implementation
3. **Orbital Mechanics**: Simplified, doesn't execute porbmn
4. **Eclipse/Flux**: Not implemented
5. **One-Point Mode**: Not implemented
6. **Multi-case**: Structure exists but untested

## Future Enhancements

1. Jupyter notebook integration
2. Visualization tools (matplotlib)
3. Parameter sweep utilities
4. Result export formats (NetCDF, HDF5)
5. Parallel execution for multiple runs
6. Web API wrapper
7. Interactive parameter selection

## References

1. **Davinci Interface**: `krc.dvrc` (4,379 lines)
2. **KRC Source**: `src/` directory
3. **Support Files**: `krc_support/` directory
4. **Original Paper**:
   > Improving Thermal Model Capability for the Planetary Science Community,
   > S. Piqueux, C. S. Edwards, R. L. Fergason, J. Laura, A. Weintraub,
   > P. R. Christensen, H. H. Kieffer
   > 49th Lunar and Planetary Science Conference (2018), Abstract #1027

## License & Attribution

Copyright 2018-2025. All Rights reserved.

Part of this work was performed at the Jet Propulsion Laboratory, California Institute of Technology under a contract with NASA. Government support acknowledged.

This Python interface is a derivative work designed to provide modern access to the KRC thermal model through a Python API.

---

**Status**: Initial implementation complete, critical features pending
**Version**: 0.1.0
**Date**: 2025-10-06
