# PyKRC Project Structure

Clean, organized structure for the Python KRC interface.

## Directory Layout

```
krc_python/                      # Python interface package root
├── README.md                    # Main project readme
├── PROJECT_STRUCTURE.md         # This file
├── setup.py                     # Package installation config
├── .gitignore                   # Git ignore rules
│
├── pykrc/                       # Main Python package
│   ├── __init__.py             # Package initialization (exports krc())
│   ├── config.py               # Configuration & path management
│   ├── core.py                 # Main krc() interface function
│   ├── input_processor.py      # master.inp parser
│   ├── data_loaders.py         # Support file loaders
│   ├── materials.py            # Material property calculations
│   ├── orbital.py              # Orbital mechanics (basic)
│   ├── orbital_tables.py       # Standish/spinaxis parsers
│   ├── executor.py             # KRC execution engine
│   ├── bin5_reader.py          # Bin5 format reader
│   ├── bin52_complete.py       # Complete bin52 parser
│   ├── bin52_parser.py         # Alternative bin52 parser
│   ├── output_parser.py        # Output utilities
│   └── data/                   # Package data
│       └── README.md           # Data directory info
│
├── tests/                       # Test suite
│   ├── __init__.py
│   ├── test_config.py          # Configuration tests
│   ├── test_materials.py       # Material property tests
│   └── test_bin5_reader.py     # Bin5 format tests
│
├── examples/                    # Usage examples
│   └── basic_usage.py          # Basic usage demonstrations
│
└── docs/                        # Documentation
    ├── QUICKSTART.md           # Quick start guide
    ├── DEVELOPMENT.md          # Developer guide
    ├── SUMMARY.md              # Implementation summary
    ├── EXTENSION_SUMMARY.md    # Extension work details
    ├── IMPLEMENTATION_STATUS.md # Complete status tracking
    ├── BIN5_FORMAT.md          # Binary format specification
    └── PACKAGE_RENAME.md       # Package naming rationale
```

## External Dependencies

### Parent KRC Repository

The Python interface relies on files in the parent KRC repository:

```
KRC/                            # Parent repository
├── krc_support/               # Support files (~40 MB total)
│   ├── albedo_2ppd.vicar      # Albedo map (1 MB)
│   ├── mola_2ppd.vicar        # Elevation map (1 MB)
│   ├── ti_map2ppd_v4.vicar    # Thermal inertia map (1 MB)
│   ├── small_bodies.hdf       # Small body database (25 MB)
│   ├── porb_master.hdf        # Orbital database (12 MB)
│   ├── comets.hdf             # Comet database (180 KB)
│   ├── standish.tab           # Orbital elements
│   ├── spinaxis.tab           # Rotation parameters
│   ├── planetary_params3.csv  # Planetary parameters
│   ├── var_header.ascii       # Variable definitions
│   ├── fake_krc344            # Fake input for execution
│   └── porb_defaults/         # Default orbital files
│       ├── Mars.porb.hdf
│       ├── Earth.porb.hdf
│       └── ...
│
├── run/                        # KRC run directory
│   └── master.inp             # Master input file
│
├── src/                        # KRC Fortran source
│   └── krc                    # KRC executable (after build)
│
└── krc_python/                # This package
    └── ...
```

### Configuration

The interface finds these files via `KRC_HOME`:

```python
from pykrc.config import get_krc_home, get_support_dir

krc_home = get_krc_home()        # -> /path/to/KRC
support = get_support_dir()       # -> /path/to/KRC/krc_support
```

## File Sizes

### Python Package (krc_python/)
- Source code: ~4,000 lines (~200 KB)
- Tests: ~350 lines (~15 KB)
- Documentation: ~2,000 lines (~80 KB)
- **Total: ~300 KB** (lightweight, portable)

### Support Files (krc_support/)
- VICAR maps: 3 × 1 MB = 3 MB
- HDF databases: 37 MB
- Text/CSV files: < 1 MB
- **Total: ~40 MB** (not in Python package)

## Import Structure

```python
# Top-level imports
from pykrc import krc, set_krc_home, get_krc_home

# Module-level imports
from pykrc.config import get_paths, KRCPaths
from pykrc.materials import calculate_thermal_properties
from pykrc.bin5_reader import load_bin5
from pykrc.orbital_tables import StandishTableParser
```

## Key Design Decisions

1. **Package Name**: `pykrc` (Python KRC) to distinguish from Fortran `krc`
2. **Function Name**: `krc()` kept simple and intuitive
3. **Large Files**: Kept in parent `krc_support/`, not in Python package
4. **Documentation**: Organized in `docs/` folder
5. **Tests**: Standard pytest structure in `tests/`
6. **Examples**: Separate `examples/` directory

## Installation

From the krc_python directory:

```bash
# Install package
pip install -e .

# Set KRC_HOME
export KRC_HOME=/path/to/KRC

# Run tests
pytest tests/

# Try examples
python examples/basic_usage.py
```

## Git Structure

The `.gitignore` excludes:
- Python bytecode (`__pycache__/`, `*.pyc`)
- Build artifacts (`dist/`, `build/`)
- Test output (`.pytest_cache/`)
- KRC output files (`*.bin.*`, `outdata.*`)
- Temporary directories (`tmp_krc_*/`)
- Large data files in `pykrc/data/`

## Documentation Organization

### Main README (package root)
- Project overview
- Quick installation
- Basic usage example
- Links to detailed docs

### docs/ folder
- **QUICKSTART.md**: New user guide
- **DEVELOPMENT.md**: Developer guide, TODO items
- **IMPLEMENTATION_STATUS.md**: Detailed status tracking
- **BIN5_FORMAT.md**: Binary format specification
- **SUMMARY.md**: Original implementation summary
- **EXTENSION_SUMMARY.md**: Extension work details
- **PACKAGE_RENAME.md**: Package naming rationale

### Code Documentation
- Docstrings in all modules (NumPy style)
- Type hints throughout
- Inline comments for complex algorithms

## Clean Structure Benefits

1. **Clear separation**: Code, tests, docs, examples
2. **Small package**: No large binary files
3. **Easy navigation**: Logical file organization
4. **Version control**: Only source code tracked
5. **Documentation**: Centralized in docs/
6. **Testing**: Standard pytest structure
7. **Examples**: Separate, runnable scripts
