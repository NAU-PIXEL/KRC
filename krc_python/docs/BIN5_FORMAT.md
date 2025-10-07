## Bin5/Bin52 File Format Documentation

### Overview

The bin5 format is a binary file format used by Davinci and KRC for storing multidimensional arrays. The bin52 format is a specific variant used by KRC for output data.

### Implementation

The Python implementation is based on the C code in `ff_bin5.c` and provides:

1. **bin5_reader.py** - Core bin5 format reader
2. **bin52_complete.py** - KRC-specific bin52 parser
3. **bin52_parser.py** - Alternative bin52 implementation

### Bin5 Format Structure

#### Header Format

The bin5 header is variable-length ASCII text ending with a marker:

```
<ndim> <dim1> <dim2> ... <dimN> <word_type> <nel> [>> <text>] C_END<arch>
```

Components:
- `ndim`: Number of dimensions (integer)
- `dim1, dim2, ...`: Size of each dimension (integers)
- `word_type`: Data type code (see Word Types below)
- `nel`: Total number of elements (product of all dimensions)
- `text`: Optional description text (after `>>`)
- `C_END`: End marker
- `arch`: 5-character architecture identifier (`x86  ` or `sun  `)

The header is padded to 512-byte blocks.

#### Word Types

Defined in ff_bin5.c:

| Code | Type | Size (bytes) | NumPy dtype |
|------|------|--------------|-------------|
| 1 | BYTE | 1 | uint8 |
| 2 | INTEGER | 2 | int16 |
| 3 | LONG | 4 | int32 |
| 4 | FLOAT | 4 | float32 |
| 5 | DOUBLE | 8 | float64 |
| 12 | UINTEGER | 2 | uint16 |
| 13 | ULONG | 4 | uint32 |
| 14 | LONGLONG | 8 | int64 |
| 15 | ULONGLONG | 8 | uint64 |

#### Binary Data

- Data follows immediately after header
- Stored in architecture-specific byte order
- May require byte swapping based on architecture field

### Bin52 Format (KRC Output)

Bin52 is bin5 with additional structure for KRC thermal model output.

#### File Structure

1. **512-byte header** with version string (e.g., "KRCv3.5.6")
2. **4 metadata values** (float64 or float32):
   - NWKRC: Number of words in KRCCOM
   - IDX: Dimension index with extra values
   - NDX: Number of extra seasons
   - NSOUT: Number of output seasons

3. **KRCCOM block** - KRC parameters:

   Version 3 format:
   ```
   - fd[96]: float64 array (64 input + 32 calculated parameters)
   - lats[37]: float64 array (latitudes)
   - elevs[37]: float64 array (elevations)
   - id[40]: uint32 array (integer parameters)
   - ld[20]: uint32 array (boolean flags)
   - title: 80-char string
   - runtime: 24-char string
   ```

   Version 2 format (different order):
   ```
   - fd[96]: float32 array
   - id[40]: uint32 array
   - ld[20]: uint32 array
   - title: 80-char string
   - runtime: 20-char string
   - lats[37]: float32 array
   - elevs[37]: float32 array
   ```

4. **Data arrays** organized as:
   ```
   data[hour][field][lat, season, case]
   ```

   Fields:
   - 1: Surface temperature (tsurf)
   - 2: Bolometer temperature (tbol)
   - 3: Atmospheric temperature (tatm)
   - 4: Downwelling visible flux (down_vis)
   - 5: Downwelling IR flux (down_ir)
   - 6: Special field (varies by hour)
   - 7: Special field (varies by hour)

   Special fields 6 & 7 contain:
   - Hour 1: converge_days, frost
   - Hour 2: delta_t_rms, frost_alb
   - Hour 3: tatm_predict, avg_heat_flow
   - Hour 4+: layer tmin, layer tmax

### KRCCOM Parameters

Key indices in KRCCOM arrays:

**fd (float array, 0-indexed):**
- [2]: INERTIA - Thermal inertia
- [3]: COND2 - Layer 2 conductivity
- [4]: DENS2 - Layer 2 density
- [5]: PERIOD - Orbital period
- [6]: SPEC_HEAT - Specific heat
- [7]: DENSITY - Density
- [15]: SpHeat2 - Layer 2 specific heat
- [32]: RLAY - Layer ratio
- [33]: FLAY - First layer size
- [34]: CONVF - Convection factor

**id (integer array, 0-indexed):**
- [0]: N1 - Number of layers
- [1]: N2 - Calculations per day
- [2]: N3 - Number of latitudes
- [5]: N24 - Output timesteps per day
- [7]: IC2 - Layer 2 start index

**ld (boolean array, 0-indexed):**
- [14]: LOCAL - Local time flag

### Python Usage

#### Basic bin5 reading:

```python
from pykrc.bin5_reader import load_bin5

data = load_bin5('file.bin5')
print(data.shape)  # NumPy array
```

#### With metadata:

```python
from pykrc.bin5_reader import load_bin5_with_metadata

data, header = load_bin5_with_metadata('file.bin5')
print(f"Dimensions: {header.dims}")
print(f"Type: {header.word_type}")
print(f"Architecture: {header.arch}")
```

#### Bin52 parsing:

```python
from pykrc.bin52_complete import parse_bin52

result = parse_bin52('outdata.bin.52')

# Access data
surf_temp = result['surf']  # [hours, lats, seasons]
time_axis = result['time']
latitudes = result['lat']
layer_info = result['layer']
```

### Byte Order Handling

The reader automatically detects and handles byte swapping:

1. Read architecture field from header (`x86` or `sun`)
2. Check system byte order
3. Swap if:
   - Big-endian system reading x86 data, OR
   - Little-endian system reading sun data

### Dimension Ordering

The C code uses column-major (Fortran) ordering while NumPy uses row-major. The reader handles this by:

1. Reversing dimension order when reading
2. Transposing the result array

### Testing

Run tests with:

```bash
pytest tests/test_bin5_reader.py -v
```

Tests cover:
- Header parsing
- Various data types (byte, int, float, double)
- Different array dimensions (1D through 4D)
- Byte swapping
- Metadata extraction

### References

1. **ff_bin5.c** - Original C implementation
2. **parser.h** - Type definitions and constants
3. **krc.dvrc** (lines 3346-3750) - Davinci bin52 processing
4. **KRC Fortran source** - Binary output generation

### Known Limitations

1. **Incomplete bin52 data extraction**: The current implementation reads KRCCOM but doesn't fully parse the nested data arrays. This requires matching the exact Fortran write statements.

2. **Structure nesting**: For >3D arrays, the C code creates nested structures. The Python version returns flat arrays with metadata.

3. **TUN8 modes**: Additional output modes (TUN8=101, etc.) are not yet implemented.

### Next Steps

To complete the bin52 parser:

1. Map exact Fortran write statements to data layout
2. Extract temperature/flux arrays from raw bin5 data
3. Handle special fields 6 & 7 properly
4. Implement seasonal axis extraction
5. Add TUN8 mode support
6. Test with actual KRC output files
