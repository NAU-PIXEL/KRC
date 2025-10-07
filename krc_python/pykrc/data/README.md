# PyKRC Data Directory

This directory is reserved for small package-specific data files.

## Large Support Files

The main KRC support files are located in the parent repository's `krc_support/` directory:

```
KRC/
├── krc_support/              # Main support files (40+ MB)
│   ├── albedo_2ppd.vicar    # Albedo map (1 MB)
│   ├── mola_2ppd.vicar      # Elevation map (1 MB)
│   ├── ti_map2ppd_v4.vicar  # Thermal inertia map (1 MB)
│   ├── small_bodies.hdf     # Small body database (25 MB)
│   ├── porb_master.hdf      # Orbital database (12 MB)
│   ├── comets.hdf           # Comet database (180 KB)
│   ├── standish.tab         # Orbital elements
│   ├── spinaxis.tab         # Rotation parameters
│   ├── planetary_params3.csv
│   ├── porb_defaults/       # Default orbital files
│   └── ...
└── krc_python/
    └── pykrc/
        └── data/            # Small package data only
            └── README.md    # This file
```

These files are accessed via the `KRC_HOME` environment variable and the `config.get_support_dir()` function.

## Why Not Include Support Files Here?

1. **Size**: Support files total ~40 MB (too large for package)
2. **Shared**: Used by both Fortran KRC and Python interface
3. **Updates**: Maintained separately from Python code
4. **License**: May have different licensing/attribution

## Usage

The data loader automatically finds support files:

```python
from pykrc.config import get_support_dir
from pykrc.data_loaders import KRCDataLoader

# Get support directory path
support_dir = get_support_dir()  # -> $KRC_HOME/krc_support

# Load data
loader = KRCDataLoader(support_dir)
standish = loader.load_standish_table()
planetary_params = loader.load_planetary_params()
```

## Small Package Data

This directory can contain:
- Configuration templates
- Small lookup tables
- Default constants
- Example data files

But not the large VICAR maps or HDF databases.
