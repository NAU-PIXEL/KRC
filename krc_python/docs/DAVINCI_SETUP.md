# Davinci KRC Setup Guide

This guide will help you set up and test the KRC thermophysical model using the davinci interface developed by the KRC authors. This is useful for isolating whether issues are in the KRC build itself or in the Python interface.

## Prerequisites

- ✅ Davinci installed at: `/Applications/davinci.app/Contents/Resources/bin/davinci`
- ✅ KRC binary built at: `/Users/chaberle/Documents/GitHab/KRC/krc`
- ✅ Support files at: `/Users/chaberle/Documents/GitHab/KRC/krc_python/pykrc/data/krc_support/`

## Quick Start

### Option 1: Using the setup script (Recommended)

```bash
cd /Users/chaberle/Documents/GitHab/KRC
./setup_davinci_krc.sh
```

This will:
1. Set the required environment variables (`DV_KRC_HOME` and `DV_SCRIPT_FILES`)
2. Verify the KRC binary and support files exist
3. Launch davinci with the KRC interface loaded

### Option 2: Manual setup

```bash
# Set environment variables
export DV_KRC_HOME="/Users/chaberle/Documents/GitHab/KRC"
export DV_SCRIPT_FILES="/Users/chaberle/Documents/GitHab/KRC/krc_python/pykrc/data"

# Launch davinci with KRC interface
davinci -f /Users/chaberle/Documents/GitHab/KRC/krc_python/docs/davinci/krc.dvrc
```

## Running Tests

### Basic test (after davinci starts)

Once davinci is running, you can test KRC interactively:

```davinci
# Simple test at latitude 12 degrees
result = krc(lat=12.)

# Check the output
printf("Min temp: %.2f K\n", min(result.tsurf))
printf("Max temp: %.2f K\n", max(result.tsurf))
```

### Run the test script

From the davinci prompt:

```davinci
load("test_davinci_krc.dv")
```

Or run it directly:

```bash
export DV_KRC_HOME="/Users/chaberle/Documents/GitHab/KRC"
export DV_SCRIPT_FILES="/Users/chaberle/Documents/GitHab/KRC/krc_python/pykrc/data"
/Applications/davinci.app/Contents/Resources/bin/davinci -f test_davinci_krc.dv
```

### Run the comprehensive tests

The original test suite is available at:

```davinci
load("krc_python/docs/davinci/test_KRC.dv")
```

## Environment Variables Explained

### `DV_KRC_HOME`
Points to the root KRC directory. The davinci interface expects:
- KRC binary at: `$DV_KRC_HOME/krc` (or `$DV_KRC_HOME/src/krc` or `$DV_KRC_HOME/src/krcd`)
- Run directory at: `$DV_KRC_HOME/run/`

### `DV_SCRIPT_FILES`
Points to the directory containing the `krc_support/` subdirectory with:
- `ti_map2ppd_v4.vicar` - Thermal inertia map
- `albedo_2ppd.vicar` - Albedo map
- `mola_2ppd.vicar` - Elevation map
- `standish.tab` - Planetary ephemeris data
- `spinaxis.tab` - Spin axis data
- `planetary_params3.csv` - Planetary parameters
- `small_bodies.hdf` - Small body data
- `comets.hdf` - Comet data
- `porb_master.hdf` - Planetary orbit master file
- `porb_defaults/` - Default orbit files for various bodies
- `fake_krc344` - Test input file

## Troubleshooting

### If KRC binary is missing

Rebuild it:
```bash
cd /Users/chaberle/Documents/GitHab/KRC
make clean
make krc
```

### If you get SIGKILL when running KRC

This is exactly the issue you're debugging. If you see SIGKILL with the davinci interface too, the problem is in the KRC build or how it's being executed, not in the Python wrapper.

### If davinci can't find support files

Check that:
1. `$DV_SCRIPT_FILES/krc_support/` exists
2. All required files are in that directory (run `ls -la $DV_SCRIPT_FILES/krc_support/`)

### Verbose output

Set verbose mode in davinci:
```davinci
verbose = 1
result = krc(lat=12., v=1)
```

## Comparing with Python Interface

Once you confirm the davinci interface works, you can compare:

1. **Input files**: The davinci interface writes `.inp` files to a temp directory. You can save these to compare with what the Python interface generates.

2. **Binary execution**: Both interfaces create a working directory, write an input file, and execute the KRC binary with input redirection.

3. **Output parsing**: Both read the binary `.bin5` output files.

Key differences to investigate:
- How inputs are formatted
- Working directory setup
- Environment or ulimits
- How the binary is executed (shell vs. subprocess)

## Next Steps for Debugging

1. ✅ Run davinci tests to confirm KRC binary works
2. Compare davinci-generated `.inp` files with Python-generated ones
3. Try running KRC manually with both types of input files
4. Add instrumentation to see where SIGKILL occurs (before/during/after KRC execution)
5. Check system limits (`ulimit -a`) that might cause SIGKILL
