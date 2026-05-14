# Debugging Plan for PyKRC Tests 64-68

**Date:** 2025-10-27
**Author:** Claude AI Assistant
**Purpose:** Comprehensive debugging plan for failing PyKRC integration tests 64-68

---

## Executive Summary

This document outlines the debugging strategy for five failing PyKRC integration tests (64-68) that prevent achieving full parity with Davinci KRC. The analysis is based on:
- Test failure analysis from `test_summary_shortened_names_20251024_153616.md`
- Davinci source code review (`/Applications/davinci.app/Contents/Resources/library/krc/krc.dvrc`)
- PyKRC implementation review (core.py, executor.py, layers.py)

### Test Status Overview

| Test # | Description | Primary Issue | Severity |
|--------|-------------|---------------|----------|
| 64 | Mode 4: Basic zone table | Zone file handling failure | High |
| 65 | Mode 4: Complex zone table | Zone file handling failure | High |
| 66 | Thick with dust over rock | Missing material parameter support | High |
| 67 | Europa ice layers (thick=2.0) | Input/output mismatch | Medium |
| 68 | Thick with lbound heat flow | Input mismatch only | Low |

---

## Detailed Analysis by Test

### Tests 64-65: Zone Table File Handling

#### Problem Description
- **Error:** "PyKRC failed - KRC execution failed: KRC failed to produce output file"
- **Root Cause:** Zone file is not being written, formatted, or referenced correctly in changecards

#### Davinci Implementation Details
Based on analysis of `krc.dvrc`:

1. **Zone File Activation** (lines 897-905):
   ```davinci
   if(LZONE=="T") {
     write(N1struct.Zone.inp, workdir+"/zonefile.tab", type=ascii, force=1)
     params=cat(params, sprintf("8 25 0 'zonefile.tab' /"), y)
   }
   ```

2. **Zone File Format**:
   ```
   C_END
   0.0010 1600.0 0.0500 650.0
   0.0015 1600.0 0.0500 650.0
   ...
   0 0 0 0
   ```
   - Header: `C_END`
   - Data: `<thickness> <density> <conductivity> <specific_heat>`
   - Terminator: `0 0 0 0`

3. **Changecard Details**:
   - Type: 8 (special type for file references)
   - Index: 25 (hardcoded for zone files)
   - Value: 0 (constant)
   - Format: `8 25 0 'zonefile.tab' /`

#### Current PyKRC Implementation Issues

From `executor.py` lines 242-257:
- Zone file copying logic exists but may have path issues
- Changecard is written but timing may be wrong
- File format validation may be incomplete

#### Required Fixes

1. **Verify zone file format**:
   - Ensure "C_END" header is present
   - Validate 4-column format with proper spacing
   - Include "0 0 0 0" terminator

2. **Fix changecard ordering**:
   - Zone file changecard must come after orbital parameters
   - Must be written before other Type 3 boolean changecards

3. **Path handling**:
   - Ensure zone file is in working directory
   - Use relative path "zonefile.tab" in changecard, not absolute path

4. **N1 validation**:
   - Ensure N1 matches the number of layers in zone file
   - For test_zone_37.tab: N1 should be 37

---

### Test 66: Missing Material Parameter Support

#### Problem Description
- **Error:** "Two-layer regolith requires different properties for upper and lower layers"
- **Root Cause:** PyKRC doesn't support `material` and `material2` parameters

#### Davinci Implementation Details

From `krc.dvrc` lines 612-664:

1. **Material Parameters**:
   - `Mat1`: Material of upper layers (default="basalt")
   - `Mat2`: Material of lower layers (default=Mat1)
   - `Por1`: Porosity of upper layers (default=0.4)
   - `Por2`: Porosity of lower layers (default=Por1)

2. **Material Property Function** (`Mat_Prop`, lines 3860-4200):
   ```davinci
   define Mat_Prop(Mat, PLOT) {
     # Returns structure with:
     # - S.Cp (specific heat coefficients)
     # - S.k (conductivity coefficients)
     # - S.Dens (density coefficients)
   }
   ```

3. **Supported Materials**:
   - Ices: "H2O", "CO2", "N2", "CH4", "SO2"
   - Rocks: "basalt"
   - Special: "dust", "rock" (via material database)

4. **Property Application Pattern**:
   ```davinci
   if(HasValue(SPEC_HEAT)==0) SPEC_HEAT = <calculated_from_material>
   ```

#### Required Fixes

1. **Add parameters to krc() signature**:
   ```python
   def krc(
       ...
       material: Optional[str] = None,  # Maps to Mat1
       material2: Optional[str] = None,  # Maps to Mat2
       ...
   )
   ```

2. **Parameter mapping**:
   ```python
   # In krc() function
   if material is not None:
       Mat1 = material
   if material2 is not None:
       Mat2 = material2
   ```

3. **Update validation**:
   - In `layers.py` validation already checks `Mat1 != Mat2`
   - Just need to ensure parameters flow through correctly

---

### Test 67: Europa Ice Layers (thick=2.0)

#### Problem Description
- **Error:** Input files differ, temperatures differ by 0.0644K
- **Root Cause:** Complex interaction between Europa defaults and thick parameter

#### Analysis

1. **Test Parameters**:
   ```python
   body="Europa"
   INERTIA=100
   INERTIA2=2000
   thick=2.0
   ```

2. **Europa-Specific Issues**:
   - Default material is H2O ice (not basalt)
   - Different thermal properties than Mars
   - Extreme INERTIA contrast (100 vs 2000)
   - Deep interface (thick=2.0m)

3. **Potential Problems**:
   - Material property calculations for Europa
   - Changecard generation with body-specific defaults
   - N1/N2 calculation for extreme contrasts

#### Required Fixes

1. **Verify Europa defaults**:
   - Check Mat1 defaults to "H2O" for Europa
   - Verify porosity calculations for ice

2. **Check INERTIA2 handling**:
   - Ensure proper calculation of lower layer properties
   - Verify IC2 calculation for large contrasts

3. **Review changecard generation**:
   - Compare changecard order between PyKRC and Davinci
   - Check for Europa-specific changecard requirements

---

### Test 68: Thick with lbound Heat Flow

#### Problem Description
- **Error:** Input files differ but temperatures match perfectly
- **Root Cause:** Non-critical formatting or changecard ordering issue

#### Analysis

1. **Test Parameters**:
   ```python
   body="Europa"
   INERTIA=100
   INERTIA2=500
   thick=0.5
   lbound=25.0  # Heat flux in mW
   ```

2. **Interesting Observation**:
   - Temperature arrays are identical (success!)
   - Input files differ (formatting issue?)
   - Suggests KRC is interpreting parameters correctly

3. **Likely Issues**:
   - Changecard formatting (spacing, precision)
   - Changecard ordering
   - IIB value calculation from lbound

#### Required Fixes

1. **Compare input files**:
   - Identify exact differences
   - Check decimal precision
   - Verify changecard order

2. **Review lbound handling**:
   - Ensure IIB is set correctly from lbound
   - Check interaction with thick parameter

---

## Implementation Strategy

### Phase 1: Quick Wins (Test 66)
**Timeline:** 1-2 hours

1. Add `material` and `material2` parameters to krc() signature
2. Map to existing `Mat1` and `Mat2` internally
3. Test with dust/rock combination

### Phase 2: Zone File Fixes (Tests 64-65)
**Timeline:** 2-3 hours

1. Debug zone file writing in `executor._write_zone_file()`
2. Verify changecard ordering in `executor._write_changecards()`
3. Test with provided zone files (test_zone_37.tab)

### Phase 3: Europa Edge Cases (Tests 67-68)
**Timeline:** 2-3 hours

1. Analyze Europa-specific parameter handling
2. Compare input files for exact differences
3. Adjust changecard generation as needed

---

## Testing Protocol

### Individual Test Commands
```bash
cd krc_python

# Test 64: Basic zone table
pytest tests/test_integration_parity.py::TestIntegrationParity::test_mode4_zone_table_basic -xvs --keep-files

# Test 65: Complex zone table
pytest tests/test_integration_parity.py::TestIntegrationParity::test_mode4_zone_table_complex -xvs --keep-files

# Test 66: Material parameters
pytest tests/test_integration_parity.py::TestIntegrationParity::test_thick_with_different_materials -xvs --keep-files

# Test 67: Europa ice layers
pytest tests/test_integration_parity.py::TestIntegrationParity::test_thick_europa_ice_layers -xvs --keep-files

# Test 68: Thick with lbound
pytest tests/test_integration_parity.py::TestIntegrationParity::test_thick_with_lbound -xvs --keep-files
```

### Full Suite Validation
```bash
# Run all tests with summary report
python -m pytest tests/test_integration_parity.py -v --summary-report=test_summary_after_fixes.md

# Verify no regression in passing tests
python -m pytest tests/test_integration_parity.py -k "not mode4 and not thick" -v
```

---

## Success Criteria

Per `CLAUDE.md` testing philosophy:

### Primary Goal: Input File Parity
- Input files must match Davinci byte-for-byte
- Use line-by-line comparison
- Check formatting, spacing, and precision

### Secondary Goal: Output Temperature Parity
- Temperature arrays must match in size
- Element-wise values must be nearly identical
- Tolerance: 0.01K absolute difference

### No Regression Policy
- All currently passing tests must continue to pass
- No changes to tests that are already working
- Maintain backward compatibility

---

## Risk Assessment

### High Risk Areas
1. **Zone file handling**: Complex file I/O and formatting requirements
2. **Changecard ordering**: Critical for KRC parsing
3. **Body-specific defaults**: Different behavior per planetary body

### Mitigation Strategies
1. Use `--keep-files` flag to preserve test artifacts
2. Compare input files line-by-line during debugging
3. Test each fix in isolation before combining
4. Maintain git commits after each successful fix

---

## References

### Source Files
- Davinci: `/Applications/davinci.app/Contents/Resources/library/krc/krc.dvrc`
- PyKRC core: `krc_python/pykrc/core.py`
- PyKRC executor: `krc_python/pykrc/executor.py`
- PyKRC layers: `krc_python/pykrc/layers.py`
- Test file: `krc_python/tests/test_integration_parity.py`

### Key Davinci Line Numbers
- Zone file writing: lines 900-905
- Material handling: lines 612-664
- Mat_Prop function: lines 3860-4200
- krc_evalN1 (zone generation): lines 1785-2112

### Test Data Files
- `krc_python/tests/test_zone_37.tab`: 37-layer zone file
- `krc_python/tests/test_zone_basic.tab`: Basic zone configuration
- `krc_python/tests/test_zone_multi.tab`: Multi-layer zone configuration

---

## Appendix: Code Snippets

### Zone File Format Example
```
C_END
0.0010 1600.0 0.0500 650.0
0.0015 1600.0 0.0500 650.0
0.0022 1600.0 0.0500 650.0
...
1441.2 1600.0 0.0500 650.0
2161.9 1600.0 0.0500 650.0
0 0 0 0
```

### Changecard Type 8 Format
```
8 25 0 'zonefile.tab' /
```

### Material Property Polynomial
```python
# Temperature normalization
X = (T - 220) * 0.01

# Property calculation
property = coeff0 + coeff1*X + coeff2*X**2 + coeff3*X**3
```

---

**End of Document**