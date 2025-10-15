# Integration Test Suite - PyKRC vs Davinci Parity

## Overview

The integration test suite (`test_integration_parity.py`) performs end-to-end validation of PyKRC against the reference davinci krc.dvrc implementation. Unlike the unit tests in `test_davinci_parity.py` (which test internal logic), these integration tests:

1. Generate KRC input files with both PyKRC and davinci
2. Run the KRC Fortran binary with both inputs
3. Compare binary output files and numerical results

## Test Coverage

The integration tests mirror all scenarios from `test_davinci_parity.py`:

### Test Categories (15 tests total)

1. **Default Values** (2 tests)
   - PORB defaults for Mars
   - User defaults for Europa

2. **Material Properties** (3 tests)
   - Basalt thermal properties (INERTIA=250)
   - k_style='Moon' (T³ conductivity)
   - k_style='Bulk' (direct polynomial)

3. **Parameter Resolution** (2 tests)
   - DELLS blocks PORB DELJUL
   - User parameter precedence

4. **Edge Cases** (3 tests)
   - PTOTAL<1 forces TAUD=0
   - TPREDICT=0 stability overrides
   - Two-layer regolith

5. **Advanced Changecards** (2 tests)
   - Type 14: Eclipse (Style 1.0)
   - Type 15: Planetary Flux

6. **Comprehensive Scenarios** (3 tests)
   - Mars with Ls + INERTIA
   - Phobos defaults
   - High INERTIA + low ALBEDO

## Setup Requirements

### 1. Set Environment Variables

```bash
# Path to KRC installation (add to ~/.bash_profile or ~/.zshrc)
export KRC_HOME=/Users/chaberle/Documents/GitHab/KRC
```

After adding, reload your shell:
```bash
source ~/.bash_profile  # or source ~/.zshrc
```

### 2. Verify davinci is accessible

The tests require davinci to be installed and accessible via:
```bash
/Applications/davinci.app/Contents/Resources/bin/davinci
```

The `krc.dvrc` file is automatically loaded from your `~/.dvrc` configuration (no additional environment variable needed).

## Running the Tests

### Run all integration tests
```bash
pytest tests/test_integration_parity.py -v
```

### Run with custom tolerance
```bash
pytest tests/test_integration_parity.py -v --tolerance 1e-5
```

### Keep temporary files for debugging
```bash
pytest tests/test_integration_parity.py -v --keep-files
```

### Run specific test class
```bash
pytest tests/test_integration_parity.py::TestMaterialPropertiesIntegration -v
```

### Run specific test
```bash
pytest tests/test_integration_parity.py::TestEdgeCasesIntegration::test_ptotal_forces_taud_zero -v
```

## Expected Output

When environment variables are set:
```
tests/test_integration_parity.py::TestDefaultValuesIntegration::test_porb_defaults_mars PASSED
tests/test_integration_parity.py::TestDefaultValuesIntegration::test_user_defaults_europa PASSED
...
============================== 15 passed in 45.23s ==============================
```

When environment variables are NOT set:
```
tests/test_integration_parity.py::TestDefaultValuesIntegration::test_porb_defaults_mars SKIPPED
...
============================== 15 skipped in 0.42s ==============================
```

## What is Validated

For each test, the validator:

1. **Runs PyKRC** with specified parameters
2. **Runs davinci krc.dvrc** with equivalent command
3. **Compares input files** (.inp format)
4. **Compares binary output** (byte-by-byte)
5. **Compares float arrays** (with tolerance)

### Success Criteria

A test passes when:
- Both PyKRC and davinci runs succeed
- Binary output arrays match within tolerance (default: 1e-6 relative error)

Note: Input files may differ slightly (changecard formatting) but output arrays must match.

## Debugging Failed Tests

### 1. View detailed failure information
```bash
pytest tests/test_integration_parity.py -v -s --keep-files
```

### 2. Check the error messages
The test output shows:
- PyKRC errors (if any)
- Davinci errors (if any)
- Maximum relative difference in output arrays
- Mean relative difference

### 3. Inspect temporary files
With `--keep-files`, temporary files are preserved in `/tmp/pytest-*` directories:
```
/tmp/pytest-xxx/
├── pykrc/
│   ├── krc_input.inp      # PyKRC input file
│   ├── krc.t52            # PyKRC binary output
│   └── krc.prt            # PyKRC print file
└── davinci/
    ├── test_krc.dv        # Davinci script
    └── /tmp/dv_*/krc_*/   # Davinci output files
        ├── krc_input.inp
        └── outdata.bin.52
```

### 4. Manual comparison
```bash
# Compare input files
diff /tmp/pytest-xxx/pykrc/krc_input.inp /tmp/dv_xxx/krc_yyy/krc_input.inp

# Compare binary outputs (as text)
od -f /tmp/pytest-xxx/pykrc/krc.t52 | head -20
od -f /tmp/dv_xxx/krc_yyy/outdata.bin.52 | head -20
```

## Continuous Integration

For CI/CD pipelines, the tests will automatically skip if environment variables are not set. To run in CI:

```yaml
# Example GitHub Actions workflow
- name: Setup KRC
  run: |
    export KRC_HOME=/opt/krc
    export DAVINCI_KRC=/opt/krc.dvrc

- name: Run integration tests
  run: pytest tests/test_integration_parity.py -v --tolerance 1e-6
```

## Validation Framework

The integration tests use the `KRCValidator` class from `pykrc/interface_validator.py`:

```python
from pykrc.interface_validator import KRCValidator

validator = KRCValidator(
    krc_home="/path/to/krc",
    davinci_krc_path="/path/to/krc.dvrc"
)

result = validator.compare_run(
    pykrc_params={"lat": 25.0, "KEEP": "T"},
    davinci_cmd='krc(lat=25.,KEEP="T")',
    tolerance=1e-6
)

assert result["summary"]["overall_match"]
```

This framework can also be used for custom validation scripts beyond the standard test suite.

## See Also

- [test_davinci_parity.py](test_davinci_parity.py) - Unit tests for internal logic
- [pykrc/interface_validator.py](../pykrc/interface_validator.py) - Validation framework
- [scripts/validate_against_davinci.py](../scripts/validate_against_davinci.py) - Standalone validation tool
