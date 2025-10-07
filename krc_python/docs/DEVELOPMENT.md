# KRC Python Interface - Development Guide

## Project Status

This is an initial implementation of a Python interface for the KRC thermal model, designed to replicate the functionality of the existing Davinci interface.

### Completed Components

#### Phase 1: Foundation ✓
- [x] Project structure with setuptools
- [x] Configuration module (paths to KRC_HOME, support files)
- [x] Input file parser for master.inp
- [x] Parameter dataclasses for type safety

#### Phase 2: Data Loading ✓
- [x] HDF5, CSV, ASCII table loaders
- [x] Data cache system (lazy loading)
- [x] Planetary database access structure

#### Phase 3: Core Functions ✓
- [x] Material property calculator (Mat_Prop equivalent)
- [x] Thermal property calculations (conductivity models: Mars, Moon, Bulk)
- [x] Orbital mechanics functions (simplified porb, generic_porb)
- [x] Basic body parameter loading

#### Phase 4: Execution Engine ✓
- [x] KRC input file generator
- [x] Subprocess execution with proper working directory
- [x] Logging and error handling
- [x] Cross-platform support (Unix/Windows)

#### Phase 5: Output Processing ✓
- [x] Bin52 binary format structure definitions
- [x] Layer property calculations
- [x] Output data structures (partial)

#### Phase 6: Main Interface ✓
- [x] Main krc() function with parameter validation
- [x] Smart defaults system
- [x] Error handling
- [x] Integration of all components

#### Phase 7: Documentation & Testing (In Progress)
- [x] Basic unit tests (config, materials)
- [x] Usage examples
- [x] README and setup files
- [ ] Complete test coverage
- [ ] API documentation
- [ ] User guide

## TODO: Critical Items

### High Priority

1. **Complete Bin52 Parser**
   - The binary parser in `output_parser.py` is a placeholder
   - Need to implement full binary format reading
   - Match exact structure from Davinci's `process_bin52` function
   - Handle multi-case outputs
   - Parse data arrays correctly based on N1, N2, N3, N4, N5

2. **Enhance Orbital Mechanics**
   - Current implementation in `orbital.py` is simplified
   - Need full integration with:
     - standish.tab parsing
     - spinaxis.tab parsing
     - porbmn executable execution
     - Rotation matrix calculations
     - Eclipse calculations
   - Add support for all body types (planets, satellites, asteroids, comets)

3. **VICAR Image Support**
   - Implement VICAR format reader or integrate library (pvl, gdal, rasterio)
   - Load thermal inertia maps
   - Load albedo maps
   - Load elevation maps
   - Enable spatial queries for lat/lon

4. **Complete Data Loaders**
   - Verify HDF5 structure matches KRC files
   - Test with actual small_bodies.hdf, comets.hdf
   - Implement porb_defaults loading
   - Add Ls/Date conversion tables

### Medium Priority

5. **Input File Generation**
   - Verify exact format matching with KRC Fortran expectations
   - Test multi-latitude runs
   - Add support for:
     - Dynamic ALBEDO/TAUD arrays
     - Planetary flux parameters
     - Eclipse parameters
     - Frost parameters

6. **One-Point Mode**
   - Implement TI derivation mode
   - Temperature table lookup
   - Proximity criterion checking
   - Error codes (-500, -400, -300, -200, -100)

7. **Advanced Features**
   - Eclipse modeling (krc_eclipse function)
   - Planetary flux (krc_planetary_flux_porb, krc_planetary_flux_table)
   - CROCUS function
   - Mutual events
   - Exoplanet support

### Low Priority

8. **Performance**
   - Optimize binary reading
   - Cache porb calculations
   - Parallel execution for parameter sweeps

9. **Additional Utilities**
   - Date/Ls conversions
   - LTST/LMST conversions
   - Coordinate transformations
   - Material property database expansion

## Development Workflow

### Setup Development Environment

```bash
cd krc_python
pip install -e ".[dev]"
```

### Running Tests

```bash
pytest tests/
pytest tests/test_materials.py -v
pytest --cov=krc tests/
```

### Code Quality

```bash
# Format code
black krc/ tests/ examples/

# Type checking
mypy krc/

# Linting
flake8 krc/
```

## Architecture Notes

### Data Flow

1. User calls `krc(lat=0, lon=0, ...)`
2. Load master.inp defaults
3. Load orbital parameters for body
4. Calculate material properties
5. Merge all parameters
6. Generate KRC input file
7. Execute KRC Fortran binary
8. Parse bin52 output
9. Return structured results

### Key Design Patterns

- **Singleton Cache**: `DataCache` for loaded files
- **Builder Pattern**: Input file construction
- **Factory Pattern**: Material properties
- **Strategy Pattern**: Conductivity models (k_style)

### File Format Notes

#### master.inp Format
- Line-based, fixed format
- 8 values per parameter line
- Logical flags as T/F
- Special handling for latitudes/elevations

#### bin52 Format
- Binary header (512 bytes) with version
- Version-dependent float/double precision
- Complex multi-dimensional arrays
- KRCCOM parameter block
- Multiple cases supported

## Testing Strategy

1. **Unit Tests**: Individual functions (materials, config, parsing)
2. **Integration Tests**: Full krc() execution (requires KRC build)
3. **Validation Tests**: Compare with Davinci results
4. **Regression Tests**: Known good outputs

## Known Limitations

1. **VICAR Support**: Not yet implemented, maps cannot be loaded
2. **Bin52 Parsing**: Placeholder only, needs full implementation
3. **Orbital Mechanics**: Simplified, doesn't run porbmn
4. **Eclipse/Flux**: Not implemented
5. **Multi-case Outputs**: Parser structure exists but not tested

## Contributing

When adding features:

1. Follow existing code structure
2. Add type hints
3. Write docstrings (NumPy format)
4. Add unit tests
5. Update this document

## References

- Davinci interface: `/krc.dvrc` (4379 lines)
- KRC Fortran source: `src/` directory
- Support files: `krc_support/` directory
- Original paper: Piqueux et al. (2018) LPSC Abstract #1027

## Contact

For questions about this Python interface implementation:
- Check existing Davinci interface for reference
- Review KRC Fortran source code
- Consult KRC documentation files
