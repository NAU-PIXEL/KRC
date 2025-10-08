# KRC Documentation Review Summary

**Date**: 2025-10-07
**Reviewer**: Claude Code
**Purpose**: Prepare for Python interface implementation

## Files Reviewed

1. ✅ **aaa/AAREADME** - Distribution build process
2. ✅ **2012je004164readme.txt** - Overview of auxiliary files
3. ✅ **2012je004164ds01.txt** - Architecture documentation
4. ✅ **2012je004164ds07.txt** - Helplist (parameter definitions)
5. ✅ **2012je004164ds08.txt** - Sample input files
6. ✅ **2012je004164ds10.txt** - One-point mode examples
7. ✅ **2012je004164ds11.txt** - Bin5 format specification
8. ✅ **krc8.f** - Main program source (from previous sessions)
9. ✅ **Makefile** - Compilation settings (from previous sessions)
10. ✅ **Example input files** - Mone.inp, master.inp

## Key Discoveries

### 1. Input Method (CRITICAL)
KRC is **interactive** and expects two filenames via stdin:
```bash
echo -e "krc\nkrc" | ./krc
```
- Line 1: Input filename (adds `.inp` extension)
- Line 2: Output filename (adds `.prt` extension)

This explains why previous execution attempts failed - we were running KRC without providing these filenames.

### 2. Required Data Files
KRC requires these files in the working directory:
- `standish.tab` - Planetary ephemeris
- `spinaxis.tab` - Spin axis data
- `PORBCM.mat` - Binary orbital parameters

Located in `/krc_support/` directory.

### 3. Runtime Expectations
- **Full seasonal run** (19 lats, 120 seasons): 5-30 minutes
- **One-point mode**: Seconds per calculation
- **Spin-up**: First 3 seasons most expensive

Previous SIGKILL issues were likely due to:
1. Missing stdin input (KRC hung waiting)
2. Timeout too short for full runs
3. Missing required data files

### 4. Input File Complexity
KRC input files contain ~160 parameters in specific Fortran formats:
- 96 real parameters (8F10.0 format, 12 lines)
- 40 integer parameters (8I10 format, 5 lines)
- 20 logical parameters (10L7 format, 2 lines)
- N4 latitudes (10F7.2 format)
- N4 elevations (10F7.2 format)
- Orbital parameters (39 values if LPORB=T)
- Change cards for parameter variations

### 5. Output Formats
Six main binary output formats (K4OUT parameter):

**Bin5 files** (K4OUT > 50):
- **Type 52** (RECOMMENDED): (nhours, 7, nlats, nseasons, ncases)
  - 7 items: TSF, TPF, TAF, DOWNVIS, DOWNIR, TIN, FROST/HEAT
  - Most comprehensive output
  - Includes packed subsurface temperatures
  - Prefix with seasonal parameters

- Types 51, 54, 55, 56: Various subsets of data

**Direct-access Fortran** (K4OUT ≤ 50):
- Less flexible, harder to read in Python
- Used for legacy compatibility

### 6. Two Execution Modes

**Full Seasonal Mode**:
- Runs multiple Mars years to thermal equilibrium
- Loops: seasons → latitudes → days → times → layers
- Uses convergence prediction (TAU = X²/2κ)
- Expensive but comprehensive

**One-Point Mode**:
- Single point-in-time calculations
- No seasonal iteration
- Fast (seconds)
- Ideal for parameter studies
- Activated by change card: `10 1 0.1 'oneA.one'`

### 7. Architecture Hierarchy

```
KRC (main)
└── TSEAS (seasons loop)
    └── TLATS (latitudes loop)
        └── TDAY (days loop, time-of-day loop, layers)
```

**Common blocks** (shared memory):
- KRCCOM: Main parameters
- LATCOM: Latitude-specific arrays
- DAYCOM: Diurnal arrays
- HATCOM: Catch-all for new items
- PORBCM: Orbital parameters

### 8. Convergence Tests
Three convergence criteria must be met:
- **DDT** < 0.002: RMS 2nd differences
- **GGT** < 0.1: Surface boundary iteration
- **DTMAX** < 0.1: RMS layer T changes per day

### 9. Change Cards System
Powerful parameter modification system:
```
TYPE INDEX VALUE 'Comment'
0/  End of changes
```

Types:
- 1: Real parameter
- 2: Integer parameter
- 3: Logical parameter
- 7: New title
- 8: New output filename
- 10: Switch to one-point mode

### 10. Bin5 Format Details
- 512-byte aligned ASCII header
- Architecture string 5 chars **before** `C_END` (critical!)
- Data in Fortran column-major order
- Supports byte-swapping for cross-platform use

## Critical Implementation Details

### Parameter Validation
Must enforce these limits:
- N1 ≤ 30 (max layers)
- N2 ≤ 1536 (max times per day, must be even)
- N3 ≤ 100 (max days to iterate)
- N4 ≤ 37 (max latitudes)
- N5 ≤ 161 (max seasons)
- CONVF ≥ 0.8 (numerical stability)

### Input File Formatting
Fortran formats are **strict**:
- Real: 8 values per line, F10.0 format (10 chars each)
- Integer: 8 values per line, I10 format (10 chars each)
- Logical: 10 values per line, L7 format (7 chars each)
- Latitudes/Elevations: 10 values per line, F7.2 format

### Minimal Test Configuration
For fast testing:
```
N1  = 13      # Fewer layers
N2  = 384     # Standard resolution
N3  = 10      # Fewer days
N4  = 1       # Single latitude
N5  = 3       # Just spin-up seasons
NRSET = 3     # Reset after 3 days
```

Runtime: ~1-2 minutes

### Standard Mars Parameters
```
ALBEDO  = 0.25       # Surface albedo
INERTIA = 200.0      # Thermal inertia [J/m²/K/s^0.5]
PERIOD  = 1.0275     # Mars sol length [Earth days]
PTOTAL  = 546.0      # Mean pressure [Pa]
TAUD    = 0.3        # Visible opacity
GRAV    = 3.727      # Mars gravity [m/s²]
SOLCON  = 1368.0     # Solar constant [W/m²]
LPORB   = T          # Use orbital calculations
```

## Python Interface Requirements

### Must Have
1. **Input file generator**: Format parameters correctly
2. **Stdin handler**: Provide filenames to KRC
3. **Working directory setup**: Copy required data files
4. **Process executor**: Run KRC with proper timeout
5. **Bin5 reader**: Already implemented ✓
6. **Result parser**: Extract data from outputs

### Should Have
1. **Parameter validation**: Check ranges and types
2. **One-point mode support**: Fast calculations
3. **Error parsing**: Read .prt file for errors
4. **Progress monitoring**: Track convergence
5. **Result dataclass**: Structured output

### Could Have
1. **All bin5 types**: Support types 51-56
2. **Continuation runs**: KOLD/KEEP support
3. **Change card generator**: Parameter variations
4. **Parallel execution**: Multiple KRC instances
5. **Plotting utilities**: Visualize results

## Comparison with Davinci Interface

From `krc.dvrc` line 1149:
```davinci
system("cd "+workdir+"; ln -s "+$DV_KRC_HOME+"/src/"+krcprog+" .; "+
       cmd+" ./"+krcprog+" < "+$DV_SCRIPT_FILES+"/krc_support/fake_krc344 2>&1 |
       tee -a "+workdir+"/krc.log")
```

Davinci approach:
1. Creates working directory
2. Symlinks KRC executable
3. Feeds stdin from `fake_krc344` (contains "krc\nkrc")
4. Captures output to krc.log

Our Python approach should be similar:
1. Create temp directory
2. Copy/link KRC executable (or use absolute path)
3. Copy required data files
4. Generate input files
5. Execute with stdin input
6. Parse outputs
7. Clean up (optional)

## Resolved Mysteries

### Why SIGKILL?
1. ❌ Not a compilation issue (Makefile correct)
2. ❌ Not a library issue (fixed with codesign)
3. ✅ Missing stdin input (KRC hung waiting)
4. ✅ Timeout too short for full runs

### Bin5 Architecture String
- ❌ NOT after C_END
- ✅ 5 characters **before** C_END
- Already fixed in our bin5_reader.py ✓

### Why "fake_krc344"?
- KRC expects interactive input (IOKEY=5 = stdin)
- Davinci pre-generates filenames
- "fake_krc344" contains: "krc\nkrc\n"

## Test Plan

### Phase 1: One-Point Mode (FAST)
1. Create simple one-point input file
2. Execute KRC with stdin input
3. Parse .prt output
4. Verify temperatures reasonable
5. **Success metric**: Get valid output in <10 seconds

### Phase 2: Minimal Full Run
1. Generate full input file (N1=13, N4=1, N5=3)
2. Copy required data files
3. Execute with 5-minute timeout
4. Parse .prt for convergence
5. **Success metric**: Complete in <2 minutes

### Phase 3: Standard Full Run
1. Generate standard input (N1=20, N4=19, N5=120)
2. Execute with 30-minute timeout
3. Read bin5 Type 52 output
4. Compare with Davinci results
5. **Success metric**: Match within 1K

### Phase 4: Integration
1. Wrap in high-level API
2. Add parameter validation
3. Add error handling
4. Write comprehensive tests
5. **Success metric**: All tests pass

## Documentation Created

1. ✅ **KRC_ARCHITECTURE_REVIEW.md** (12 sections, ~400 lines)
   - Complete parameter reference
   - Input/output format details
   - Convergence and runtime info
   - Common pitfalls
   - Example workflows

2. ✅ **IMPLEMENTATION_PLAN.md** (7 phases, timeline estimate)
   - Phase-by-phase breakdown
   - File structure
   - Testing strategy
   - Success criteria
   - Risk mitigation

3. ✅ **REVIEW_SUMMARY.md** (this file)
   - Key discoveries
   - Critical details
   - Test plan
   - Resolved mysteries

## Ready for Implementation

All prerequisite research complete. Ready to proceed with:
- **Phase 1**: Parameter management
- **Phase 2**: Input file generation
- **Phase 3**: Execution management

Total estimated time: **25-30 hours** of focused development.

## Questions for User

1. Should we start with one-point mode (fast testing) or full seasonal mode?
2. Priority on Type 52 bin5 output, or support all 6 types?
3. Bundle required data files (standish.tab, etc.) in package?
4. Match Davinci interface exactly, or create more Pythonic API?
5. Support Windows, or Unix-only sufficient?
