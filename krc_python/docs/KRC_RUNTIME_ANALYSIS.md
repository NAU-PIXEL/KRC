# KRC Runtime Environment and Input Management - Comprehensive Analysis

**Date**: 2025-10-07
**Purpose**: Analysis of KRC documentation to determine proper runtime environment setup and input file management for Python interface

---

## Executive Summary

Based on thorough analysis of the KRC documentation in `docs/KRC_docs/`, the following critical requirements have been identified for successful KRC execution:

### Key Findings

1. **KRC is Interactive**: Expects two filenames via stdin (without extensions)
2. **Required Data Files**: Must have `standish.tab`, `spinaxis.tab`, and `PORBCM.mat` in working directory when `LPORB=T`
3. **Computational Intensity**: Default parameters cause very long runtimes (minutes to hours)
4. **Stability Requirements**: `CONVF >= 0.8` for numerical stability; unstable parameters cause crashes
5. **Format Requirements**: Input files must follow precise formatting (8F10.0, 8I10, 10L7 formats)

---

## 1. Execution Method

### 1.1 Interactive Input (from ds01.txt, ds07.txt)

KRC expects **two filenames** via stdin:

```bash
echo -e "input_basename\noutput_basename" | ./krc
```

**Important**:
- Filenames are provided **without extensions**
- KRC automatically adds `.inp` and `.prt` extensions
- Files must exist in the **current working directory**
- KRC reads from IOKEY=5 (stdin) using FORTRAN READ statements

### 1.2 Davinci Method (Reference Implementation)

From `krc.dvrc` line 1149:
```davinci
system("cd "+workdir+"; ln -s "+$DV_KRC_HOME+"/src/"+krcprog+" .; "+
       cmd+" ./"+krcprog+" < "+$DV_SCRIPT_FILES+"/krc_support/fake_krc344 2>&1 | tee -a "+workdir+"/krc.log")
```

Where `fake_krc344` contains:
```
krc
krc
```

This tells KRC to:
1. Read input from `krc.inp` in current directory
2. Write print output to `krc.prt`
3. Write binary output to path specified **inside** the .inp file

---

## 2. Required Data Files

### 2.1 Orbital Parameter Files (from ds07.txt, ds04.txt)

When `LPORB=T` (orbital calculations enabled), KRC **requires** these files in the working directory:

| File | Purpose | Size | Location |
|------|---------|------|----------|
| `standish.tab` | Planetary ephemeris data (from JPL) | ~2.7KB | `/run/` or `/pykrc/data/krc_support/` |
| `spinaxis.tab` | Spin axis orientation parameters | ~1.8KB | `/run/` or `/pykrc/data/krc_support/` |
| `PORBCM.mat` | Binary orbital common block (precomputed) | ~8.4KB | **ONLY in `/run/`** |

**Critical**: These files are read by `PORB0` and `PORB1` subroutines. Without them, KRC will:
- Hang waiting for keyboard input
- Crash silently
- Produce incorrect orbital calculations

### 2.2 File Location Strategy

From analysis:
- Files must be in the **current working directory** where KRC executes
- **NOT** in KRC source directory
- **NOT** in subdirectories
- Direct file access via `OPEN(UNIT=...)` statements

**Recommended approach**:
1. Create temporary working directory
2. Copy all three files to working directory
3. Create `.inp` file in working directory
4. Execute KRC from working directory
5. Read output files from working directory

---

## 3. Input File Format (from ds07.txt, ds08.txt)

### 3.1 File Structure

```
Line 1: KOLD KEEP [IDB1-6]       (I*, optional debug flags)
Line 2: Title (80 characters)    (20A4)
Lines 3-10: Real parameters      (8F10.0) - 8 values per line, 96 total
Lines 11-15: Integer parameters  (8I10)  - 8 values per line, 40 total
Lines 16-17: Logical flags       (10L7)  - 10 values per line, 20 total
Line 18+: Latitudes              (10F7.2)
Line 19+: Elevations             (10F7.2)
Lines 20-29: PORB orbital data   (5G15.7) - IF LPORB=T
Lines 30+: Change cards          (various formats)
Final: 0/ (end case), 0/ (end run)
```

### 3.2 Critical Formatting Rules

From helplist (ds07.txt lines 89-92):

> "Parameter values are listed below their titles, which are in many cases identical to the code name, and last character of the title is above the last location in the field. Thus, integer values MUST be aligned."

**Format Examples**:
```fortran
    ALBEDO     EMISS   INERTIA     COND2     DENS2    PERIOD SPEC_HEAT   DENSITY
       .25      1.00     200.0      2.77     928.0    1.0275      647.     1600.
```

- Real: `F10.0` format (10 characters wide, right-aligned)
- Integer: `I10` format (10 characters wide, right-aligned)
- Logical: `L7` format (7 characters, 'T' or 'F')

**Critical**: Alignment errors cause:
- Silent read failures
- Wrong parameter values
- Crashes in numerical routines

---

## 4. Computational Parameters and Runtime

### 4.1 Key Parameters Affecting Runtime (from ds07.txt)

| Parameter | Description | Default | Fast Test | Impact |
|-----------|-------------|---------|-----------|--------|
| N1 | Number of layers | 28 | 10-15 | Linear |
| N2 | Times per day | 1536 | 96-288 | Linear |
| N3 | Max days to iterate | 15 | 3-5 | Linear |
| N4 | Number of latitudes | 19 | 1 | Linear |
| N5 | Number of seasons | 120 | 3-10 | Linear |
| NRSET | Days before reset | 3 | 1 | Affects convergence |

**Runtime formula** (approximate):
```
Time ≈ N1 × N2 × N3 × N4 × N5 × (10-50 µs per iteration)
```

**Default run**:
```
28 × 1536 × 15 × 19 × 120 = 1.5 billion iterations ≈ 5-30 minutes
```

**Fast test run**:
```
10 × 96 × 3 × 1 × 3 = 8,640 iterations ≈ 0.1-1 seconds
```

### 4.2 Stability Requirements (from ds07.txt lines 171-173)

> "35 CONVF Safety factor for classical numerical convergence
>         0 for no binary time division of lower layers
>         >0.8 for binary time division. Larger is more conservative"

**Critical**: `CONVF < 0.8` causes:
- Error: "Parameter error in TDAY(1): Instability anticipated"
- Numerical blowup
- Temperature divergence
- SIGKILL from system

**Recommendation**: Use `CONVF = 2.0` to `4.0` for stability

### 4.3 Convergence Parameters (from ds07.txt lines 75-82)

```
DDT    = 0.002  # RMS 2nd differences convergence limit
GGT    = 0.1    # Surface iteration limit
DTMAX  = 0.1    # RMS layer T change per day
```

**For faster testing** (from ds07.txt line 530):
```
GGT = 99.0      # Avoid iteration waiting for convergence
NRSET = 999     # Avoid reset of layers
N3 = 1          # Turn off prediction
```

---

## 5. Binary Output Specification

### 5.1 Disk File Specification (from ds07.txt)

Binary output path is specified **inside the .inp file** with a "change card":

```
8 5 0 './output/filename.t52' / Disk file for Run 1
```

Format: `TYPE INDEX VALUE 'FILENAME' / comment`

- Type 8: Change disk filename
- Index 5: (unused but required)
- Value 0: (unused but required)
- Filename: Path in single quotes

### 5.2 Output Formats (K4OUT parameter)

| K4OUT | Format | Content | Use Case |
|-------|--------|---------|----------|
| 0 | Direct access | KRCCOM + hourly TSF/TPF per season | Standard databases |
| 52 | bin5 | (N24, 7, N4, seasons, cases) | **Recommended** - Most comprehensive |
| 51 | bin5 | (N24, 2, N4, seasons, cases) | Basic surface/planck temps |
| 54 | bin5 | (seasons, 5, N4, cases) | Daily summaries |
| 55 | bin5 | (seasons, 9, cases) | Single latitude, many seasons |
| 56 | bin5 | (vectors, N4, seasons, cases) | Full layer data |

**Type 52 contents** (from ds07.txt lines 487-490):
```
1) TSF      - Surface temperature
2) TPF      - Planck temperature
3) TAF      - Atmospheric temperature
4) DOWNVIS  - Downwelling visible radiation
5) DOWNIR   - Downwelling IR radiation
6) TIN      - Packed layer temperatures
7) FROST4   - Packed frost/heat/albedo
```

---

## 6. Common Failure Modes and Solutions

### 6.1 SIGKILL (-9) Issues

**Symptom**: KRC killed with signal 9, no output

**Causes**:
1. **Long runtime mistaken for hang** - Default runs take 5-30 minutes
2. **Unstable parameters** - CONVF < 0.8 causes numerical blowup
3. **Memory issues** - KOMMON array too small for requested output
4. **Missing data files** - LPORB=T without standish.tab/spinaxis.tab/PORBCM.mat

**Solutions**:
1. Use minimal test parameters (see Section 4.1)
2. Ensure CONVF >= 2.0
3. Check KOMMON size vs. output requirements
4. Verify all data files present

### 6.2 No Output Files Created

**Symptom**: KRC returns 0 but no .t52 or .prt files

**Causes**:
1. **K4OUT = 0** - No disk output requested
2. **JDISK > N5** - Disk output starts after run ends
3. **Wrong working directory** - Files created elsewhere
4. **Filename in .inp incorrect** - Path doesn't exist or isn't writable

**Solutions**:
1. Set K4OUT = 52 for bin5 output
2. Set JDISK = 1 or 3 (start output immediately or after spin-up)
3. Ensure CWD is working directory with .inp file
4. Use relative paths like `./output.t52`

### 6.3 Incorrect Results

**Symptom**: KRC runs but results seem wrong

**Causes**:
1. **Missing LPORB=T** - Not using orbital calculations
2. **Wrong PORB data** - Orbital parameters for wrong planet
3. **Misaligned input** - Integer parameters shifted by format error
4. **N3 too small** - Not converged (check LP4 output)

**Solutions**:
1. Always set LPORB=T for planetary calculations
2. Verify PORB cards match target body (line says "Mars:Mars", etc.)
3. Carefully check column alignment in .inp file
4. Increase N3 and check convergence in .prt file

---

## 7. Recommended Python Interface Architecture

### 7.1 Working Directory Management

```python
def create_krc_workdir(base_path=None):
    """Create and populate KRC working directory."""
    if base_path is None:
        workdir = Path(tempfile.mkdtemp(prefix="krc_"))
    else:
        workdir = Path(base_path)
        workdir.mkdir(parents=True, exist_ok=True)

    # Copy required data files
    krc_home = Path(os.environ.get("KRC_HOME", "."))
    data_files = ["standish.tab", "spinaxis.tab", "PORBCM.mat"]

    for fname in data_files:
        src = krc_home / "run" / fname
        if not src.exists():
            # Try alternate location
            src = krc_home / "krc_python/pykrc/data/krc_support" / fname

        if src.exists():
            shutil.copy(src, workdir / fname)
        else:
            raise FileNotFoundError(f"Required file not found: {fname}")

    return workdir
```

### 7.2 Input File Generation

```python
def generate_inp_file(workdir, params, basename="krc"):
    """Generate properly formatted .inp file."""
    inp_path = workdir / f"{basename}.inp"

    with open(inp_path, 'w') as f:
        # Line 1: KOLD KEEP
        f.write(f"{params.get('KOLD', 0):3d} {params.get('KEEP', 0):3d} / KOLD KEEP\n")

        # Line 2: Title
        title = params.get('TITLE', 'Python KRC Run')[:80]
        f.write(f"{title}\n")

        # Lines 3-10: Real parameters (8F10.2)
        real_params = [
            ['ALBEDO', 'EMISS', 'INERTIA', 'COND2', 'DENS2', 'PERIOD', 'SPEC_HEAT', 'DENSITY'],
            ['CABR', 'AMW', 'SatPrA', 'PTOTAL', 'FANON', 'TATM', 'TDEEP', 'SpHeat2'],
            # ... (6 more lines)
        ]

        for line_params in real_params:
            # Write header
            header = "    " + "".join(f"{p:>10}" for p in line_params)
            f.write(header + "\n")

            # Write values (RIGHT-ALIGNED in 10-char fields)
            values = "    " + "".join(f"{params.get(p, 0.0):>10.2f}" for p in line_params)
            f.write(values + "\n")

        # ... (similar for integer and logical parameters)

        # Orbital data (if LPORB=T)
        if params.get('LPORB', True):
            # Write PORB cards
            pass

        # Binary output specification
        outfile = f"./{basename}.t52"
        f.write(f"8 5 0 '{outfile}' / Disk file\n")
        f.write("0/\n")
        f.write("0/\n")

    return inp_path
```

### 7.3 Execution

```python
def execute_krc(workdir, basename="krc", timeout=300):
    """Execute KRC with proper stdin handling."""
    krc_exe = find_krc_executable()

    # Prepare stdin input
    stdin_input = f"{basename}\n{basename}\n"

    # Execute from working directory
    result = subprocess.run(
        [str(krc_exe)],
        input=stdin_input,
        capture_output=True,
        text=True,
        cwd=str(workdir),
        timeout=timeout
    )

    # Check for output files
    prt_file = workdir / f"{basename}.prt"
    out_file = workdir / f"{basename}.t52"

    if not out_file.exists():
        raise RuntimeError(f"KRC failed to produce output:\n{result.stdout}\n{result.stderr}")

    return {
        'returncode': result.returncode,
        'stdout': result.stdout,
        'stderr': result.stderr,
        'prt_file': prt_file,
        'output_file': out_file
    }
```

### 7.4 Parameter Validation

```python
def validate_params(params):
    """Validate KRC parameters before execution."""
    errors = []

    # Check dimensional limits (from krccom.inc)
    if params.get('N1', 0) > 30:
        errors.append(f"N1={params['N1']} exceeds MAXN1=30")
    if params.get('N2', 0) > 1536:
        errors.append(f"N2={params['N2']} exceeds MAXN2=1536")
    if params.get('N4', 0) > 37:
        errors.append(f"N4={params['N4']} exceeds MAXN4=37")
    if params.get('N5', 0) > 161:
        errors.append(f"N5={params['N5']} exceeds MAXN5=161")

    # Check stability requirement
    if params.get('CONVF', 0) < 0.8 and params.get('CONVF', 0) != 0:
        errors.append(f"CONVF={params['CONVF']} < 0.8 will cause instability")

    # Check N2 is even
    if params.get('N2', 0) % 2 != 0:
        errors.append(f"N2={params['N2']} must be even")

    # Check N24 divides N2
    if params.get('N2', 0) % params.get('N24', 24) != 0:
        errors.append(f"N24={params['N24']} must divide N2={params['N2']}")

    # Warn about runtime
    runtime_estimate = (params.get('N1', 20) * params.get('N2', 384) *
                       params.get('N3', 15) * params.get('N4', 1) *
                       params.get('N5', 120)) * 20e-6  # 20 µs per iteration

    if runtime_estimate > 60:
        errors.append(f"Estimated runtime: {runtime_estimate:.1f}s - consider reducing parameters")

    if errors:
        raise ValueError("Parameter validation failed:\n" + "\n".join(errors))
```

---

## 8. Recommended Test Configuration

### 8.1 Minimal Fast Test

For testing Python interface (< 1 second):

```python
fast_test_params = {
    'N1': 10,        # 10 layers
    'N2': 96,        # 4 times per hour
    'N3': 3,         # 3 days iteration
    'N4': 1,         # Single latitude
    'N5': 3,         # 3 seasons
    'NRSET': 1,      # Reset after 1 day
    'N24': 24,       # 24 hours output
    'K4OUT': 52,     # bin5 type 52
    'JDISK': 1,      # Start output immediately
    'CONVF': 3.0,    # Very stable
    'LP2': True,     # Print parameters
    'LP3': False,    # Don't print hourly
    'LP4': True,     # Print convergence
    'LPORB': True,   # Use orbital calculations
}
```

### 8.2 Standard Production Run

For science-quality results (5-10 minutes):

```python
standard_params = {
    'N1': 20,        # 20 layers (adequate depth)
    'N2': 384,       # 16 times per hour (standard)
    'N3': 15,        # 15 days iteration
    'N4': 19,        # 19 latitudes (10° spacing)
    'N5': 120,       # Full Mars year
    'NRSET': 3,      # 3-season spin-up
    'N24': 48,       # High time resolution
    'K4OUT': 52,     # Full output
    'JDISK': 81,     # Save last Martian year
    'CONVF': 3.0,    # Stable
}
```

---

## 9. Critical Takeaways

1. **Always copy data files** (`standish.tab`, `spinaxis.tab`, `PORBCM.mat`) to working directory
2. **Always provide stdin input** (two filenames without extensions)
3. **Start with minimal parameters** for testing (N1=10, N2=96, N3=3, N4=1, N5=3)
4. **Set CONVF >= 2.0** for stability
5. **Set K4OUT=52** for comprehensive output
6. **Verify format alignment** in generated .inp files (critical!)
7. **Allow sufficient timeout** (60s for tests, 600s for production)
8. **Check .prt file** for convergence and error messages
9. **Execute from working directory** where .inp file exists
10. **Clean up** temp directories unless debugging

---

## 10. References

- **2012je004164ds01.txt** - Architecture overview
- **2012je004164ds07.txt** - Helplist (parameter definitions)
- **2012je004164ds08.txt** - Sample input files
- **2012je004164ds10.txt** - One-point mode examples
- **krc.dvrc** - Davinci reference implementation
- **master.inp** - Default parameter template

---

**Next Steps**:
1. Implement working directory manager
2. Implement input file generator with proper formatting
3. Implement executor with stdin handling
4. Test with minimal parameters first
5. Validate output reading with existing bin5_reader
6. Refactor core.py to use new components
