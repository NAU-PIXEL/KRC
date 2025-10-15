# KRC Python Refactoring: Executive Summary

## Overview

This document provides a comprehensive summary of the analysis and recommended refactoring for the `krc_python` package based on:
1. Code quality analysis
2. Parameter flow analysis
3. Official KRC documentation (2012je004164ds07.txt)
4. Comparison between core.py and executor.py

---

## Current State Assessment

### Code Statistics

| Metric | Value | Status |
|--------|-------|--------|
| Total Python files | 42 | Moderate |
| Total lines of code | ~9,575 | Large |
| core.py | 997 lines | ⚠️ **Too complex** |
| executor.py | 747 lines | ⚠️ **Needs refactoring** |
| Main krc() function | 963 lines | 🔴 **Critical** |
| krc() parameters | 140+ | 🔴 **Excessive** |
| Default assignment locations | 4 | ⚠️ **Fragmented** |

### Quality Grades

| Category | Grade | Notes |
|----------|-------|-------|
| Documentation | A | Excellent numpy-style docstrings |
| Dead Code | B | Minimal commented code, some debug scripts |
| **Complexity** | **C+** | **core.py too complex, needs refactoring** |
| **Organization** | **B** | **Multiple sources of truth for defaults** |
| Type Hints | A- | Excellent coverage, minor gaps |
| Test Coverage | B | Good tests, needs consolidation |

**Overall: B (Good, but critical refactoring needed)**

---

## Critical Issues Discovered

### 1. Multiple Sources of Truth for Defaults

**Problem:** Defaults are defined in 4 different locations:

1. **Function signature** (core.py lines 34-167)
2. **core.py early defaults** (lines 270-331)
3. **PORB defaults** (lines 390-484)
4. **MASTER_INP_DEFAULTS** (executor.py lines 36-168)

**Impact:** Conflicts, maintenance burden, confusion about which values are actually used.

### 2. Major Default Conflicts

Five parameters have **conflicting defaults** between core.py and executor.py:

| Parameter | core.py (PORB) | executor.py (master.inp) | Impact |
|-----------|----------------|-------------------------|--------|
| **SLOAZI** | 0.0° | 90.0° | **90° difference in slope azimuth!** |
| **DJUL** | 0.1 | -1222.69 | **Different Julian date epoch** |
| **IIB** | -1 | 0 | **Different numerical algorithm** |
| **LKofT** | True | False | **Different physics model** |
| **TUN_Flx15** | 0 | 65 | **Different flux tuning** |

**Why this works:** Changecards override master.inp header values, so the code produces correct results despite the conflicts. However, the code is confusing and error-prone.

### 3. Misleading Comments

Five parameters had **incorrect comments** in core.py:

```python
# WRONG COMMENTS (now identified):
FLAY: Optional[float] = None,  # Default: 2.0  ❌ (actual: 0.10)
RLAY: Optional[float] = None,  # Default: 1.08 ❌ (actual: 1.15)
IIB: Optional[int] = None,     # Default: 2    ❌ (actual: -1)
N3: Optional[int] = None,      # Default: 10   ❌ (actual: 1)
TAURAT: Optional[float] = None,# Default: 2.0  ❌ (actual: 0.25)
```

### 4. 963-Line Function

The `krc()` function handles everything:
- Parameter validation
- Default assignment
- PORB loading
- Ancillary data lookup
- Material property calculation
- Numerical parameter calculation
- Two-layer configuration
- Frost configuration
- Parameter dictionary building
- Execution
- Output parsing

**Cyclomatic complexity:** ~40+ (threshold for "unmaintainable" is 10)

---

## Understanding KRC Architecture

### From Official Documentation

Based on the KRC helplist (2012je004164ds07.txt), KRC expects:

1. **Input File Header** (lines 1-28)
   - Uses master.inp format with specific values
   - **These are just placeholders!**
   - 64 real parameters (8 × 8F10.2 lines)
   - 20 integer parameters (2 × 8I10 lines + 1 × 4I10 line)
   - 20 logical parameters (2 × 10L7 lines)

2. **Changecards** (after header)
   - Type 1: Real parameters (index 1-64)
   - Type 2: Integer parameters (index 1-20)
   - Type 3: Logical parameters (index 1-20)
   - Type 8: File specifications
   - **These override the header values**

3. **Why Changecards Exist**
   - From documentation: "punch card convention from KRC's early days"
   - Allows runtime parameter modification
   - **Critical:** KRC reads header, then applies changecards

### Current pykrc Implementation (Correct!)

```
User calls krc()
    ↓
core.py sets defaults + PORB
    ↓
executor.py writes input file:
    - Header: MASTER_INP_DEFAULTS (placeholders)
    - Changecards: Actual values
    ↓
KRC Fortran:
    - Reads header
    - Applies changecards (OVERRIDES header)
    - Runs simulation
```

**Key insight:** The conflicts don't cause bugs because changecards win! But the code is confusing.

---

## Refactoring Goals

### Primary Goals

1. **Single source of truth** for all default values
2. **Reduce complexity** of core.py:krc() from 963 lines to ~200 lines
3. **Eliminate conflicts** between core.py and executor.py defaults
4. **Fix misleading comments**
5. **Maintain 100% backward compatibility** (no API changes)
6. **Preserve Davinci parity** (identical numerical output)

### Secondary Goals

7. Improve testability (unit test helper functions)
8. Improve maintainability (clear separation of concerns)
9. Improve documentation (architecture diagrams, parameter flow)
10. Set up foundation for future enhancements

---

## Proposed Solution

### New Architecture

```
krc_python/
├── defaults.py          ← NEW: Canonical defaults (single source of truth)
├── porb_handler.py      ← NEW: PORB-specific logic extraction
├── core.py              ← REFACTORED: Orchestration only (~200 lines)
├── executor.py          ← REFACTORED: I/O only (~400 lines)
├── materials.py         ← EXISTING: No changes
├── numerical.py         ← EXISTING: No changes
├── validation.py        ← EXISTING: No changes
├── orbital.py           ← EXISTING: No changes
└── [other modules]      ← EXISTING: No changes
```

### defaults.py Structure

Three levels of defaults, clearly documented:

```python
# USER_DEFAULTS: Initial values when krc() is called
USER_DEFAULTS = {
    'DELLS': 1.0,
    'spinup_years': 2.0,
    'LKEY': 'T',
    'LKofT': True,
    # ... ~30 parameters
}

# PORB_DEFAULTS: Values set when PORB is loaded (override USER_DEFAULTS)
PORB_DEFAULTS = {
    'EMISS': 1.0,
    'SLOPE': 0.0,
    'SLOAZI': 0.0,      # Overrides master.inp 90.0
    'DJUL': 0.1,        # Overrides master.inp -1222.69
    'IIB': -1,          # Overrides master.inp 0
    'LKofT': True,      # Overrides master.inp False
    # ... ~19 parameters
}

# PORB_TOUCHED_PARAMS: Always write changecards for these
PORB_TOUCHED_PARAMS = {
    'EMISS', 'TDEEP', 'SLOPE', 'SLOAZI', 'DJUL',
    'IIB', 'LKofT', # ... (19 total)
}

# MASTER_INP_HEADER_DEFAULTS: Used ONLY for input file header
# (actual values come from changecards!)
MASTER_INP_HEADER_DEFAULTS = {
    # All 64 real + 20 integer + 20 logical parameters
    # Matches master.inp format expected by Fortran
}
```

### Default Precedence (Clearly Defined)

```
Highest Priority
    ↑
    │  1. User-specified parameters (passed to krc())
    │
    │  2. PORB-derived parameters (PORB_DEFAULTS + HDF data)
    │
    │  3. USER_DEFAULTS (initial defaults)
    │
    │  4. MASTER_INP_HEADER_DEFAULTS (header only!)
    ↓
Lowest Priority
```

### core.py Refactoring (8 Helper Functions)

```python
def krc(...140 parameters...) -> Dict[str, Any]:
    """Main entry point - now just orchestration."""

    # 1. Extract user params
    user_params = _extract_user_params(**locals())

    # 2. Apply defaults
    defaults = _apply_default_parameters(...)

    # 3. Validate required
    if lat is None:
        raise ValueError("lat required")

    # 4. Setup PORB
    body_params, porb_params, porb_touched = porb_handler.setup_orbital_parameters(...)

    # 5. Load ancillary data
    ALBEDO, ELEV, INERTIA = _load_ancillary_data(...)

    # 6. Calculate material properties
    upper_props, lower_props, using_direct, INERTIA = _calculate_material_properties(...)

    # 7. Calculate numerical params (N1, N2, N3)
    N1, N2, N3 = _calculate_numerical_parameters(...)

    # 8. Validate two-layer & frost
    validate_two_layer_config(...)
    validate_frost_config(...)

    # 9. Build parameter dict
    params = _build_krc_parameter_dict(...)

    # 10. Setup lat/elev
    latitudes, elevations, N4 = _setup_latitude_elevation(...)

    # 11. Execute KRC
    result = executor.run_krc(params, ...)

    # 12. Parse output
    output = parse_bin52(result["output_file"], ...)

    # 13. Add metadata
    output.update({"body": body, "porb": body_params, ...})

    # 14. Cleanup
    if not keep_files:
        shutil.rmtree(result["workdir"])

    return output
```

**Target:** ~180-200 lines (down from 963!)

### executor.py Changes

```python
# OLD:
MASTER_INP_DEFAULTS = { ... 130 lines ... }

# NEW:
from .defaults import MASTER_INP_HEADER_DEFAULTS as MASTER_INP_DEFAULTS
from .defaults import PORB_TOUCHED_PARAMS

# Rest of executor.py unchanged (just imports from defaults.py)
```

---

## Implementation Plan

### Phase 0: Create New Modules (1 hour)
- Create `defaults.py` with all three levels of defaults
- Create `porb_handler.py` with PORB extraction logic
- Add comprehensive documentation

### Phase 1: Refactor core.py (3 hours)
- Extract 8 helper functions (see CORE_REFACTOR.md)
- Update all comments to be accurate
- Import from defaults.py
- Test after each extraction

### Phase 2: Refactor executor.py (2 hours)
- Import from defaults.py
- Remove duplicate default definitions
- Update documentation

### Phase 3: Fix Comments (15 min)
- Update 5 misleading parameter comments
- Add references to defaults.py

### Phase 4: Testing (2 hours)
- Unit tests for new modules
- Regression tests (compare before/after)
- Changecard generation validation
- Davinci parity validation

### Phase 5: Documentation (1 hour)
- Update module docstrings
- Create architecture diagram
- Update CHANGELOG

**Total Time:** ~9.25 hours

---

## Risk Mitigation

### Risks

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Break numerical parity | Medium | High | Test after each extraction, validate against Davinci |
| Introduce parameter bugs | Medium | High | Unit test each helper, regression tests |
| Miss edge cases | Low | Medium | Comprehensive test suite |
| Performance regression | Very Low | Low | Benchmark before/after |
| Break user code | Very Low | Critical | **No changes to public API** |

### Rollback Strategy

```bash
# All work on feature branch
git checkout -b refactor/unified-krc

# Test after each phase
pytest tests/ -v

# If any issues:
git checkout main
git branch -D refactor/unified-krc
```

---

## Success Criteria

✅ All existing tests pass without modification
✅ New unit tests for defaults.py (>90% coverage)
✅ New unit tests for porb_handler.py (>85% coverage)
✅ core.py:krc() reduced to <200 lines
✅ Single source of truth for all defaults
✅ No default conflicts
✅ All comments accurate
✅ Numerical output identical to pre-refactoring
✅ Changecard generation preserved
✅ Davinci parity maintained
✅ Documentation complete (architecture + parameter flow)

---

## Benefits

### Immediate Benefits

1. **Clarity:** Single source of truth eliminates confusion
2. **Maintainability:** Small, focused functions easier to modify
3. **Testability:** Can unit test each helper function
4. **Debuggability:** Easier to isolate issues
5. **Correctness:** No more misleading comments

### Long-term Benefits

6. **Extensibility:** Adding new bodies/features is clearer
7. **Onboarding:** New developers can understand flow
8. **Confidence:** Tests validate refactoring preserves behavior
9. **Foundation:** Clean architecture for future work
10. **Documentation:** Clear parameter flow for users

---

## Next Steps

1. **Review this summary** with team/stakeholders
2. **Get approval** for refactoring approach
3. **Create feature branch** `refactor/unified-krc`
4. **Execute Phase 0** (create new modules)
5. **Test Phase 0** (import, run tests)
6. **Execute Phase 1** (refactor core.py)
7. **Test Phase 1** (full test suite)
8. **Continue through Phase 5**
9. **Final review** and merge to main

---

## References

- [PARAMETER_FLOW_ANALYSIS.md](PARAMETER_FLOW_ANALYSIS.md) - Complete parameter tracing
- [CORE_REFACTOR.md](CORE_REFACTOR.md) - Detailed core.py refactoring plan
- [UNIFIED_REFACTOR_PLAN.md](UNIFIED_REFACTOR_PLAN.md) - Combined refactoring strategy
- [docs/JGR_docs/2012je004164ds07.txt](docs/JGR_docs/2012je004164ds07.txt) - Official KRC documentation

---

## Conclusion

The krc_python package is **well-designed overall** but suffers from:
1. **Complexity** in the main krc() function (963 lines)
2. **Fragmentation** of default values across modules
3. **Conflicts** between default sources
4. **Misleading** inline documentation

**The refactoring is necessary and feasible:**
- Clear plan with 5 phases
- Preserves backward compatibility
- Reduces code by ~40% through helper extraction
- Establishes single source of truth
- Improves maintainability and clarity

**Recommendation:** Proceed with refactoring following the phased approach.
