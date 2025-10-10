# KRC Python Interface Implementation Plan

**Date**: 2025-10-07
**Status**: Ready to implement
**Priority**: High

## Overview

Based on comprehensive review of KRC documentation, this plan outlines the steps to create a fully functional Python interface for the KRC thermal model.

## Key Findings from Documentation Review

1. **KRC expects two filenames via stdin** (without extensions)
2. **Input files are complex**: ~96 real, ~40 integer, ~20 logical parameters in specific Fortran formats
3. **Runtime is substantial**: 5-30 minutes for full seasonal runs
4. **Six binary output formats**: Type 52 bin5 recommended (most comprehensive)
5. **Two execution modes**: Full seasonal and one-point (fast single calculations)
6. **Required data files**: standish.tab, spinaxis.tab, PORBCM.mat must be in working directory

## Implementation Phases

### Phase 1: Parameter Management ✓ NEXT
**Goal**: Create robust parameter handling system

**Tasks**:
1. Create `pykrc/parameters.py` with:
   - `KRCParameters` dataclass with all 96 reals, 40 ints, 20 logicals
   - Default values from documentation
   - Type validation and range checking
   - Parameter documentation as docstrings

2. Create validation functions:
   - Check N1 ≤ MAXN1 (30)
   - Check N2 ≤ MAXN2 (1536)
   - Check N4 ≤ MAXN4 (37)
   - Check N5 ≤ MAXN5 (161)
   - Check CONVF ≥ 0.8 for stability
   - Cross-parameter validation (e.g., N2 must be even)

**Files to create**:
- `pykrc/parameters.py` (~500 lines)
- `tests/test_parameters.py` (~200 lines)

### Phase 2: Input File Generation
**Goal**: Generate properly formatted KRC .inp files

**Tasks**:
1. Create `pykrc/input_generator.py`:
   - `write_inp_file()` function
   - Proper Fortran formatting (8F10.0, 8I10, 10L7)
   - Handle latitudes/elevations
   - Handle orbital parameters (if LPORB=T)
   - Generate change cards

2. Support both modes:
   - Full seasonal mode
   - One-point mode (with separate one-point file)

**Files to create**:
- `pykrc/input_generator.py` (~400 lines)
- `tests/test_input_generator.py` (~150 lines)

### Phase 3: Execution Management
**Goal**: Run KRC executable with proper setup and error handling

**Tasks**:
1. Create `pykrc/executor.py`:
   - `KRCExecutor` class
   - Working directory management
   - Copy required data files (standish.tab, etc.)
   - Generate unique temp directories
   - Execute KRC with stdin input
   - Handle timeouts (default 30 minutes)
   - Capture stdout/stderr
   - Return paths to output files

2. Error handling:
   - Non-zero return codes
   - Missing output files
   - Timeout handling
   - Parse .prt for error messages

**Files to create**:
- `pykrc/executor.py` (~300 lines)
- `tests/test_executor.py` (~200 lines)

### Phase 4: Output Parsing
**Goal**: Parse KRC output files into Python structures

**Tasks**:
1. Extend `pykrc/bin5_reader.py`:
   - `extract_krccom()` function for bin5 prefix
   - Parse packed arrays (TIN, TAX, FROST4)
   - Handle all 6 bin5 types (51-56)

2. Create `pykrc/output_parser.py`:
   - `parse_prt()` for print files
   - Extract convergence info
   - Extract error messages
   - `KRCResult` dataclass for structured output

3. Create result structures:
   - Named arrays (tsf, tpf, taf, downvis, downir)
   - Coordinate arrays (hours, lats, seasons)
   - Metadata (parameters used, convergence status)

**Files to create**:
- `pykrc/output_parser.py` (~400 lines)
- `tests/test_output_parser.py` (~150 lines)

### Phase 5: High-Level API
**Goal**: User-friendly interface for running KRC

**Tasks**:
1. Update `pykrc/core.py`:
   - `krc.run()` for full seasonal mode
   - `krc.run_onepoint()` for one-point mode
   - Convenient parameter passing (dicts or kwargs)
   - Return structured results

2. Example usage:
```python
from pykrc import krc

# Full seasonal run
result = krc.run(
    albedo=0.25,
    inertia=200.0,
    n4=19,
    n5=120,
    k4out=52,
    lats=[-87.5, -80, ..., 87.5],
    elevs=[3.51, 2.01, ..., -2.57],
    timeout=1800
)

# One-point mode
results = krc.run_onepoint([
    {'ls': 100, 'lat': 22.3, 'hour': 13.5, 'inertia': 200},
    {'ls': 180, 'lat': 0.0, 'hour': 13.0, 'inertia': 100},
])
```

**Files to modify**:
- `pykrc/core.py` (rewrite, ~300 lines)
- `tests/test_core.py` (rewrite, ~200 lines)

### Phase 6: Testing and Validation
**Goal**: Ensure reliability and correctness

**Tasks**:
1. Create integration tests:
   - Test against known KRC outputs
   - Compare with Davinci results
   - Test all bin5 output types
   - Test change cards
   - Test continuation runs (KOLD/KEEP)

2. Create example notebooks:
   - Basic usage
   - Parameter sensitivity studies
   - Comparison with observations
   - Seasonal cycles

3. Performance testing:
   - Measure overhead
   - Test parallel execution (multiple KRC instances)
   - Memory usage

**Files to create**:
- `tests/test_integration.py` (~300 lines)
- `examples/basic_usage.ipynb`
- `examples/parameter_study.ipynb`

### Phase 7: Documentation
**Goal**: Complete user and developer documentation

**Tasks**:
1. User documentation:
   - Installation guide
   - Quickstart tutorial
   - Parameter reference
   - Output format guide
   - Troubleshooting

2. Developer documentation:
   - Architecture overview
   - Adding new features
   - Testing guide
   - Release process

3. API documentation:
   - Docstrings for all public functions
   - Type hints throughout
   - Sphinx documentation

**Files to create**:
- `docs/user_guide.md`
- `docs/developer_guide.md`
- `docs/api_reference.md`
- `docs/conf.py` (Sphinx config)

## Testing Strategy

### Unit Tests
- Each module has corresponding test file
- Mock KRC executable for fast testing
- Test edge cases and error conditions
- Aim for >80% code coverage

### Integration Tests
- Use real KRC executable
- Test against reference outputs
- Include in CI/CD (with timeout handling)
- Mark as slow tests (skip in quick runs)

### Validation Tests
- Compare with published results (Kieffer 2013)
- Compare with Davinci outputs (from test_KRC.dv)
- Verify physical consistency (energy balance, etc.)

## File Structure After Implementation

```
krc_python/
├── pykrc/
│   ├── __init__.py              (existing)
│   ├── core.py                  (rewrite)
│   ├── config.py                (existing)
│   ├── bin5_reader.py           (existing, extend)
│   ├── parameters.py            (new)
│   ├── input_generator.py       (new)
│   ├── executor.py              (new)
│   ├── output_parser.py         (new)
│   └── utils.py                 (new)
├── tests/
│   ├── test_parameters.py       (new)
│   ├── test_input_generator.py  (new)
│   ├── test_executor.py         (new)
│   ├── test_output_parser.py    (new)
│   ├── test_core.py             (rewrite)
│   ├── test_integration.py      (new)
│   └── fixtures/                (test data)
├── examples/
│   ├── basic_usage.ipynb        (new)
│   └── parameter_study.ipynb    (new)
├── docs/
│   ├── KRC_ARCHITECTURE_REVIEW.md  (existing)
│   ├── IMPLEMENTATION_PLAN.md      (this file)
│   ├── user_guide.md               (new)
│   ├── developer_guide.md          (new)
│   └── api_reference.md            (new)
└── README.md                    (update)
```

## Dependencies

Current dependencies (from setup.py):
- numpy>=1.20.0
- pandas>=1.3.0
- h5py>=3.0.0

Additional dependencies needed:
- None (use stdlib for subprocess, tempfile, etc.)

Development dependencies:
- pytest>=7.0.0 (existing)
- pytest-cov>=3.0.0 (existing)
- black>=22.0.0 (existing)
- mypy>=0.950 (existing)

## Success Criteria

Phase 1-7 complete when:
- [ ] All unit tests pass
- [ ] Integration tests reproduce Davinci results
- [ ] Documentation complete and reviewed
- [ ] Example notebooks run without errors
- [ ] API is intuitive (reviewed by user)
- [ ] Package installable via pip
- [ ] CI/CD pipeline working

## Timeline Estimate

- Phase 1: 2-3 hours
- Phase 2: 3-4 hours
- Phase 3: 3-4 hours
- Phase 4: 4-5 hours
- Phase 5: 2-3 hours
- Phase 6: 4-6 hours
- Phase 7: 3-4 hours

**Total**: ~25-30 hours of focused development

## Risk Mitigation

### Risk: KRC crashes with generated input files
**Mitigation**: Start with minimal test case (one-point mode), then incrementally add complexity

### Risk: Timeout issues on slow machines
**Mitigation**: Provide configurable timeout, document expected runtimes, use one-point mode for testing

### Risk: Binary format incompatibilities
**Mitigation**: Bin5 reader already handles byte swapping, test on multiple platforms

### Risk: Parameter validation too strict
**Mitigation**: Warn rather than error for non-critical issues, document all validation rules

## Next Immediate Steps

1. **Start Phase 1**: Create `pykrc/parameters.py`
   - Define `KRCParameters` dataclass
   - Add validation methods
   - Write unit tests

2. **Create minimal test case**:
   - One-point mode input file
   - Single test case
   - Verify KRC executes successfully

3. **Iterate rapidly**:
   - Test each component as it's built
   - Keep integration tests running
   - Adjust plan based on findings

## Questions for User

Before proceeding with implementation:

1. **Scope**: Should we implement all 6 bin5 output types, or focus on Type 52?
2. **Priority**: Is one-point mode or full seasonal mode more important initially?
3. **Platform**: Need to support Windows, or just Unix-like systems?
4. **Data files**: Should we bundle standish.tab, etc., or require user to provide?
5. **Backward compatibility**: Need to match Davinci interface exactly, or can we improve?
