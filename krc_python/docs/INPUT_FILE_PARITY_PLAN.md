# Input File Parity Implementation Plan

**Goal:** Achieve 100% input file matching between PyKRC and Davinci krc.dvrc

**Date:** 2025-10-09
**Status:** Phase 1 - Reconnaissance

---

## Executive Summary

### Current State
Based on exploration and documentation review:
- ✅ PyKRC has **100% parameter coverage** (all 88 parameters from krc.dvrc are exposed)
- ✅ Input file **format is identical** (field widths, structure, PORB handling)
- ⚠️ **Changecard generation logic** needs verification for exact matching
- ⚠️ **Parameter default resolution** may differ (master.inp vs calculated values)
- ⚠️ **User parameter filtering** for changecards needs validation

### Key Files
- **Validation script**: `krc_python/scripts/validate_against_davinci.py`
- **Validation framework**: `krc_python/pykrc/interface_validator.py`
- **PyKRC executor**: `krc_python/pykrc/executor.py`
- **Davinci reference**: `krc_python/docs/davinci/krc.dvrc`
- **Comparison doc**: `krc_python/docs/pykrc_comparison.md`

---

## Implementation Strategy

### **Modified Hybrid Approach**

Based on codebase analysis, using a **streamlined strategy** that balances speed with thoroughness:

1. **Phase 1: Reconnaissance (Direct)** - Run validation, document mismatches
2. **Phase 2-3: Architecture Analysis (Parallel Sub-Agents)** - Deep-dive both implementations
3. **Phase 4: Gap Analysis (Direct)** - Compare and create fix specs
4. **Phase 5: Implementation (Iterative Direct)** - Fast fix-validate-debug cycles
5. **Phase 6: Final Validation (Sub-Agent)** - Comprehensive test suite

**Rationale for Hybrid:**
- Sub-agents for deep analysis (stateless OK for one-time research)
- Direct execution for iterative debugging (speed critical)
- Manager (me) coordinates and synthesizes findings

---

## Phase 1: Reconnaissance

### Objectives
1. Run existing validation script successfully
2. Document current input file differences (if any)
3. Categorize mismatch types
4. Establish baseline metrics

### Setup Requirements
```bash
# Environment
export KRC_HOME="/Users/chaberle/Documents/GitHab/KRC"
DAVINCI_BIN="/Applications/davinci.app/Contents/Resources/bin/davinci"
DAVINCI_KRC="./krc_python/docs/davinci/krc.dvrc"

# Validation command
cd krc_python
python scripts/validate_against_davinci.py \
    --krc-home $KRC_HOME \
    --davinci-krc $DAVINCI_KRC \
    --suite basic \
    --tolerance 1e-6 \
    --keep-files \
    --verbose
```

### Test Cases (from ValidationTestSuite)
**Basic Suite:**
1. Mars default run
2. Mars with Ls
3. Mars with thermal inertia
4. Europa no flux
5. Phobos

**Advanced Suite:**
6. Two-layer regolith
7. Eclipse modeling
8. Planetary flux

### Expected Issues (Hypotheses)

#### **Category A: Changecard Filtering**
- **Issue**: PyKRC may generate changecards for ALL parameters vs only user-modified ones
- **Impact**: Extra changecards (functionally identical but not byte-identical)
- **Example**: ConUp0-3 written even when using default material
- **Fix**: Implement user_params filtering correctly

#### **Category B: Parameter Defaults**
- **Issue**: MASTER_INP_DEFAULTS vs actual davinci defaults may differ
- **Impact**: Different values in header section
- **Example**: FANON (0.055 vs 0.3), TAURAT (0.25 vs 2.0)
- **Fix**: Audit MASTER_INP_DEFAULTS against krc.dvrc actual defaults

#### **Category C: Formatting Precision**
- **Issue**: Floating-point formatting may differ slightly
- **Impact**: Rounding differences in changecards
- **Example**: "1.080E+00" vs "1.08E+00"
- **Fix**: Match exact format strings

#### **Category D: Boolean/Integer Handling**
- **Issue**: Conversion of Python bool/int to Fortran T/F/int
- **Impact**: Changecard values may differ
- **Example**: LKEY=True → "1" vs LKEY="T" → "1"
- **Fix**: Ensure consistent type handling

### Deliverables
1. **Validation report**: Pass/fail for each test case
2. **Mismatch catalog**: Specific line-by-line differences
3. **Issue categorization**: Group by root cause
4. **Priority matrix**: Critical vs minor differences

---

## Phase 2 & 3: Architecture Deep-Dive

### Sub-Agent A: Davinci Architecture Analysis

**Task:**
```
Analyze the complete input file generation flow in krc.dvrc (davinci).

**Focus Areas:**
1. Parameter transformation pipeline
   - How user parameters are captured (lines ~500-700)
   - Default value resolution logic
   - Material property calculation (Mat_Prop function)
   - Numerical parameter auto-calculation (krc_evalN1, krc_evalN2)

2. Input file writing (KRC_input_write function, lines ~3500-4000)
   - Header section formatting (KOLD, KEEP line)
   - Parameter block formatting (8 values per line, field widths)
   - Boolean flag formatting (T/F conversion)
   - Latitude/elevation formatting
   - PORB parameter writing

3. Changecard generation (lines ~1000-1100)
   - Types: 1 (float), 2 (int), 3 (bool), 8 (file)
   - Index calculation (part1, part2, part3 arrays)
   - Filtering logic (which params get changecards)
   - Formatting rules (scientific notation thresholds)

4. Master.inp handling
   - Default values loaded
   - Override logic
   - Which defaults are written to header vs changecards

5. Special cases
   - Time-varying arrays (ALBEDO, TAUD)
   - Multi-latitude runs (N4 > 1)
   - Eclipse/PFlux parameters
   - Two-layer regolith (IC2 calculation)

**Deliverable:**
Comprehensive architecture document with:
- Call graph showing function flow
- Parameter transformation table
- Changecard generation algorithm (pseudo-code)
- Formatting specifications (exact field widths, precision, alignment)
- Default value resolution rules
- Edge case handling

**Critical Details to Capture:**
- Exact format strings for floats/ints in changecards
- Logic for when to use scientific notation
- Index numbering for each parameter type
- Default value sources (master.inp vs hardcoded)
```

### Sub-Agent B: PyKRC Architecture Analysis

**Task:**
```
Analyze the complete input file generation flow in PyKRC.

**Focus Areas:**
1. Parameter transformation pipeline (core.py)
   - _extract_user_params() function (line 21)
   - Default application logic (lines 270-349)
   - Material property calculation
   - Numerical parameter auto-calculation

2. Input file writing (executor.py)
   - KRCExecutor.create_input_file() (lines 200-326)
   - _write_param_line() (lines 328-353)
   - _write_logical_flags() (lines 355-376)
   - _write_latitudes() and _write_elevations()
   - PORB parameter writing

3. Changecard generation (executor.py)
   - _write_changecards() (lines 400-504)
   - Type 1/2/3/8 handling
   - Index calculation
   - Filtering logic with user_params
   - Formatting rules

4. MASTER_INP_DEFAULTS
   - Values defined (lines 15-147)
   - How they're used vs actual master.inp file
   - Comparison with davinci defaults

5. Special cases
   - Time-varying arrays
   - Multi-latitude
   - Eclipse/PFlux
   - Two-layer regolith

**Deliverable:**
Comprehensive architecture document with:
- Call graph showing function flow
- Parameter transformation table
- Changecard generation algorithm (actual Python code flow)
- Formatting specifications (actual format strings used)
- Default value resolution rules
- Edge case handling

**Critical Details to Capture:**
- Exact format strings in _write_changecards (lines 499-503)
- user_params filtering logic (lines 414-419)
- Boolean conversion logic
- Index numbering system
- MASTER_INP_DEFAULTS vs parse_master_inp() discrepancies
```

### Execution
Launch **both agents in parallel** (single message with two Task tool calls):
```python
Task(
    description="Analyze Davinci krc.dvrc input generation",
    prompt=[Agent A task above],
    subagent_type="general-purpose"
)

Task(
    description="Analyze PyKRC executor.py input generation",
    prompt=[Agent B task above],
    subagent_type="general-purpose"
)
```

### Estimated Time
30-45 minutes (parallel execution)

---

## Phase 4: Gap Analysis

### Objectives
1. Compare architecture documents from Agents A & B
2. Identify specific code-level mismatches
3. Create prioritized fix list with line numbers
4. Generate test cases for each fix

### Analysis Framework

#### **Comparison Matrix**

| Aspect | Davinci | PyKRC | Match? | Priority | Fix Complexity |
|--------|---------|-------|--------|----------|----------------|
| Header format | TBD | TBD | ? | High | Low |
| Float formatting | TBD | TBD | ? | High | Low |
| Changecard indices | TBD | TBD | ? | Critical | Medium |
| Filtering logic | TBD | TBD | ? | Critical | Medium |
| Default values | TBD | TBD | ? | High | Low |
| Special cases | TBD | TBD | ? | Medium | High |

#### **Fix Specification Template**

For each mismatch:
```markdown
### Fix #N: [Brief Description]

**Category:** [Formatting / Logic / Defaults / Special Case]
**Priority:** [Critical / High / Medium / Low]
**Complexity:** [Low / Medium / High]

**Issue:**
- What's different between Davinci and PyKRC
- Specific line numbers in both files

**Expected Behavior:**
- How Davinci does it (with examples)

**Current PyKRC Behavior:**
- How PyKRC currently does it (with examples)

**Proposed Fix:**
- Specific code changes with line numbers
- New logic/formatting to implement

**Test Case:**
- How to verify the fix works
- Example parameters that trigger this code path

**Dependencies:**
- Other fixes that must be done first
```

### Deliverables
1. **Comparison report**: Side-by-side architecture analysis
2. **Fix specifications**: Detailed plan for each mismatch (10-20 expected)
3. **Test plan**: How to validate each fix
4. **Dependency graph**: Fix ordering

### Estimated Time
15-20 minutes (direct execution)

---

## Phase 5: Implementation

### Strategy
**Iterative direct execution** for speed:
1. Implement fixes in priority order
2. Run validation after each category
3. Debug mismatches immediately
4. Only launch sub-agent if deep research needed

### Fix Categories (Priority Order)

#### **Priority 1: Critical Formatting** (30 min)
- Field widths in changecards
- Scientific notation thresholds
- Float precision

#### **Priority 2: Changecard Logic** (1 hour)
- Index calculation verification
- Filtering with user_params
- Type conversion (bool/int/float)

#### **Priority 3: Parameter Defaults** (30 min)
- Audit MASTER_INP_DEFAULTS
- Fix discrepancies with davinci
- Ensure header section matches

#### **Priority 4: Special Cases** (1 hour)
- Time-varying arrays
- Multi-latitude
- Eclipse/PFlux parameters
- Two-layer regolith IC2

#### **Priority 5: Edge Cases** (30 min)
- Boundary conditions
- Error handling
- Null/None handling

### Implementation Workflow

```
For each fix category:
  1. Implement changes in executor.py or core.py
  2. Run validation test for affected test case
  3. If pass: move to next fix
  4. If fail:
     - Examine diff output
     - Debug issue
     - Repeat
  5. Document changes in commit message
```

### Testing Approach

**After each fix:**
```bash
# Run specific test case
python test_inp_validation.py --test "Mars default"

# Or run basic suite
python scripts/validate_against_davinci.py \
    --krc-home $KRC_HOME \
    --davinci-krc $DAVINCI_KRC \
    --suite basic \
    --keep-files
```

### Estimated Time
1-3 hours depending on complexity

---

## Phase 6: Final Validation

### Sub-Agent Task

**Task:**
```
Run comprehensive validation of PyKRC input file generation.

**Objectives:**
1. Run full validation test suite (basic + advanced)
2. Compare input files byte-by-byte and line-by-line
3. Document any remaining differences
4. Test edge cases
5. Generate final pass/fail report

**Test Suite:**
Run validation script with all test cases:
- Basic: 5 test cases (Mars, Europa, Phobos, etc.)
- Advanced: 3+ test cases (two-layer, eclipse, pflux)

**For each test:**
1. Run PyKRC with test parameters
2. Run Davinci with identical parameters
3. Compare generated .inp files:
   - Byte-identical check
   - Line-by-line diff
   - Normalized numerical comparison
4. Document results

**Specific Checks:**
- Header section (KOLD, KEEP, parameter blocks)
- Parameter values (floats, ints, bools)
- Changecard count and contents
- Changecard indices and values
- Changecard formatting
- Latitude/elevation arrays
- PORB parameters
- Special cases (arrays, multi-lat, eclipse)

**Edge Cases to Test:**
- Extreme thermal inertias (TI=50, TI=2000)
- Non-standard bodies (asteroids, comets)
- Custom time resolution (DELLS=0.1, DELLS=10)
- Two-layer with extreme contrasts
- Time-varying ALBEDO/TAUD arrays

**Deliverable:**
Comprehensive validation report with:
- Overall pass rate (X/Y tests passing)
- Detailed results for each test case
- Byte-identical status
- Line-by-line differences (if any)
- Specific mismatches with line numbers
- Recommendations for remaining issues

**Success Criteria:**
- 100% of tests passing
- Byte-identical .inp files (excluding comments)
- No functional differences in changecards
```

### Execution
```python
Task(
    description="Comprehensive validation testing",
    prompt=[Task above],
    subagent_type="general-purpose"
)
```

### Estimated Time
20-30 minutes

---

## Success Metrics

### Quantitative Goals
- ✅ **100% test pass rate** (all validation tests passing)
- ✅ **Byte-identical files** (excluding timestamps/comments)
- ✅ **Zero functional differences** (changecards produce identical KRC behavior)
- ✅ **All edge cases covered** (extreme parameters, special bodies)

### Qualitative Goals
- ✅ **Maintainable code** (clear comments explaining changecard logic)
- ✅ **Documented differences** (any intentional deviations from davinci)
- ✅ **Robust validation** (easy to verify parity in future)

---

## Known Challenges

### Challenge 1: MASTER_INP_DEFAULTS Discrepancies
**Issue:** PyKRC MASTER_INP_DEFAULTS may not match actual master.inp or davinci defaults
**Evidence:** FANON=0.055 in defaults vs 0.3 in documentation
**Solution:** Audit line-by-line against davinci krc.dvrc defaults

### Challenge 2: Changecard Filtering
**Issue:** user_params filtering may be too aggressive or too lenient
**Evidence:** Need to verify only user-set params generate changecards
**Solution:** Trace through _write_changecards() logic with test cases

### Challenge 3: Float Formatting
**Issue:** Scientific notation thresholds may differ
**Evidence:** Need to check exact format strings
**Solution:** Match davinci's format logic precisely

### Challenge 4: Special Case Handling
**Issue:** Eclipse, PFlux, time-varying arrays have complex logic
**Evidence:** These features were added in Phase 4 (recent)
**Solution:** Careful testing of these code paths

---

## Tools & Resources

### Validation Tools
- **validate_against_davinci.py**: Main validation script
- **interface_validator.py**: Comparison classes
- **test_inp_validation.py**: Lightweight test script (created today)

### Reference Files
- **krc.dvrc**: Davinci reference implementation
- **test_KRC.dv**: Comprehensive test suite
- **pykrc_comparison.md**: Parameter coverage analysis

### Development Files
- **executor.py**: PyKRC input generation
- **core.py**: Parameter processing
- **numerical.py**: N1/N2 calculation
- **materials.py**: Material properties

---

## Risk Mitigation

### Risk 1: Fortran Precision Quirks
**Probability:** Medium
**Impact:** High (byte-identical requirement)
**Mitigation:** Test with extreme values, compare with davinci output

### Risk 2: Undocumented Davinci Behavior
**Probability:** Low
**Impact:** High (can't reproduce without understanding)
**Mitigation:** Use sub-agent for deep krc.dvrc analysis

### Risk 3: Time Overrun
**Probability:** Medium
**Impact:** Medium
**Mitigation:** Prioritize fixes, accept minor differences if needed

---

## Next Steps

### Immediate (Phase 1)
1. ✅ Create implementation plan (this document)
2. ⏭️ Fix test script environment issues
3. ⏭️ Run validation and capture baseline
4. ⏭️ Document current mismatches

### Near-term (Phases 2-4)
1. Launch parallel sub-agents for architecture analysis
2. Compare findings and create fix specifications
3. Prioritize fixes by impact

### Long-term (Phases 5-6)
1. Implement fixes iteratively
2. Run comprehensive validation
3. Document final results

---

## Conclusion

This plan provides a structured approach to achieving 100% input file parity between PyKRC and Davinci. The hybrid strategy balances thoroughness (sub-agents for deep analysis) with speed (direct execution for iterative debugging).

**Estimated Total Time:** 3-6 hours
**Expected Outcome:** Byte-identical input files for all standard test cases

**Key Success Factor:** Systematic analysis before implementation to avoid false starts and wasted debugging effort.
