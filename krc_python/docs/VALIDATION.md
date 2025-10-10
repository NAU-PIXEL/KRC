### PyKRC Validation Framework

This document describes the validation framework for ensuring PyKRC produces identical results to the reference krc.dvrc (Davinci) implementation.

## Overview

The validation framework compares PyKRC and krc.dvrc at three levels:

1. **Input Files (.inp)**: Ensures both interfaces generate identical KRC input files
2. **Binary Output Files (t52/bin52)**: Compares binary output files byte-by-byte and numerically
3. **Parsed Output Data**: Validates that parsed temperature/depth arrays match

## Components

### 1. Interface Validator Module (`pykrc/interface_validator.py`)

The core validation module provides:

- **InputFileComparator**: Compares KRC .inp files with floating-point normalization
- **BinaryFileComparator**: Byte-by-byte and numerical comparison of binary files
- **OutputDataComparator**: Compares parsed output arrays and metadata
- **KRCValidator**: Main validation class that orchestrates comparisons
- **ValidationTestSuite**: Pre-defined test cases from test_KRC.dv

### 2. Validation Script (`scripts/validate_against_davinci.py`)

Command-line tool for running validation tests.

## Usage

### Basic Usage

```bash
# Run basic validation suite
python scripts/validate_against_davinci.py \
    --krc-home ~/Applications/krc_3_6_5 \
    --davinci-krc ~/krc.dvrc

# Run advanced validation suite
python scripts/validate_against_davinci.py \
    --krc-home ~/Applications/krc_3_6_5 \
    --davinci-krc ~/krc.dvrc \
    --suite advanced

# Run all tests with custom tolerance
python scripts/validate_against_davinci.py \
    --krc-home ~/Applications/krc_3_6_5 \
    --davinci-krc ~/krc.dvrc \
    --suite all \
    --tolerance 1e-5

# Save detailed results to JSON
python scripts/validate_against_davinci.py \
    --krc-home ~/Applications/krc_3_6_5 \
    --davinci-krc ~/krc.dvrc \
    --output validation_results.json \
    --verbose
```

### Programmatic Usage

```python
from pykrc.interface_validator import KRCValidator, run_validation_suite

# Initialize validator
validator = KRCValidator(
    krc_home="~/Applications/krc_3_6_5",
    davinci_krc_path="~/krc.dvrc"
)

# Run a single comparison
result = validator.compare_run(
    pykrc_params={"lat": 12.0, "ls": 23.0, "INERTIA": 100.0},
    davinci_cmd='krc(lat=12.,ls=23.,INERTIA=100.,KEEP="T")',
    tolerance=1e-6
)

# Check results
assert result["summary"]["overall_match"], "Validation failed!"

# Or run a full test suite
results = run_validation_suite(validator, test_suite="basic")
print(f"{results['passed']}/{results['total_tests']} tests passed")
```

## Test Suites

### Basic Test Suite

The basic suite covers fundamental KRC functionality:

1. **Mars default run**: Default parameters
2. **Mars with Ls**: Seasonal specification
3. **Mars with thermal inertia**: Material properties
4. **Europa no flux**: Airless body without planetary flux
5. **Phobos**: Satellite thermal modeling

### Advanced Test Suite

The advanced suite covers specialized features:

1. **Two-layer regolith**: Subsurface layer configuration
2. **Eclipse modeling**: Satellite eclipse calculations
3. **Planetary flux**: Thermal emission from parent planet

## Validation Criteria

### 1. Input File Comparison

Input files are considered identical if:
- All parameter values match within 6 significant figures
- Floating-point values are normalized for comparison
- Comments and whitespace differences are ignored

### 2. Binary File Comparison

Binary files are compared at two levels:

**Byte-identical**: Files are exactly the same byte-by-byte (strictest)

**Numerically equivalent**: Floating-point values match within tolerance:
- Default tolerance: 1e-6 (0.0001% relative difference)
- Configurable via `--tolerance` flag

### 3. Output Data Comparison

Parsed output arrays are compared:
- Array shapes must match exactly
- Values must be within relative tolerance
- NaN/Inf patterns must match

## Comparison Results

Each comparison returns a detailed result dictionary:

```python
{
    "pykrc": {
        "success": True,
        "error": None,
        "files": {"inp": "...", "bin52": "..."}
    },
    "davinci": {
        "success": True,
        "error": None,
        "files": {"inp": "...", "bin52": "..."}
    },
    "input_file_comparison": {
        "identical": True,
        "differences": [],
        "line_count_file1": 150,
        "line_count_file2": 150
    },
    "binary_file_comparison": {
        "identical": True,
        "size_match": True,
        "byte_identical": True,
        "size": 12345
    },
    "float_array_comparison": {
        "identical": True,
        "num_values": 3088,
        "max_absolute_diff": 2.3e-8,
        "max_relative_diff": 1.2e-7,
        "mean_absolute_diff": 5.1e-9,
        "mean_relative_diff": 3.4e-8
    },
    "summary": {
        "both_succeeded": True,
        "input_files_match": True,
        "binary_files_match": True,
        "float_arrays_match": True,
        "overall_match": True
    }
}
```

## Adding New Test Cases

### 1. Add to ValidationTestSuite

Edit `pykrc/interface_validator.py`:

```python
class ValidationTestSuite:
    @staticmethod
    def get_custom_tests() -> List[Dict[str, Any]]:
        return [
            {
                "name": "My custom test",
                "pykrc": {"lat": 0.0, "INERTIA": 200.0, "body": "Moon"},
                "davinci": 'krc(lat=0.,INERTIA=200.,body="Moon",KEEP="T")'
            },
            # Add more tests...
        ]
```

### 2. Run Custom Suite

```python
from pykrc.interface_validator import KRCValidator, ValidationTestSuite

validator = KRCValidator(krc_home="...", davinci_krc_path="...")

# Run custom tests
for test in ValidationTestSuite.get_custom_tests():
    result = validator.compare_run(
        pykrc_params=test["pykrc"],
        davinci_cmd=test["davinci"]
    )
    print(f"{test['name']}: {'PASS' if result['summary']['overall_match'] else 'FAIL'}")
```

## Troubleshooting

### Common Issues

1. **Davinci not found**
   - Ensure davinci is in your PATH
   - Or use full path: `/Applications/davinci.app/Contents/Resources/bin/davinci`

2. **krc.dvrc not found**
   - Provide absolute path to krc.dvrc file
   - Ensure file is readable

3. **KRC_HOME not set**
   - Ensure `--krc-home` points to valid KRC installation
   - Directory should contain `krc` executable and data files

4. **Input files differ**
   - Check for parameter translation issues
   - Review normalized diff output for details
   - Some parameters may have different defaults

5. **Binary files differ**
   - Check tolerance setting (increase if needed)
   - Review numerical differences (max/mean relative diff)
   - Some numerical differences are expected due to floating-point operations

### Debugging

Enable verbose output to see detailed comparison results:

```bash
python scripts/validate_against_davinci.py \
    --krc-home ~/krc \
    --davinci-krc ~/krc.dvrc \
    --verbose \
    --keep-files
```

The `--keep-files` flag preserves temporary directories for manual inspection.

## Continuous Validation

For ongoing development, run validation as part of your workflow:

```bash
# Quick validation before commits
python scripts/validate_against_davinci.py \
    --krc-home ~/krc \
    --davinci-krc ~/krc.dvrc \
    --suite basic

# Comprehensive validation before releases
python scripts/validate_against_davinci.py \
    --krc-home ~/krc \
    --davinci-krc ~/krc.dvrc \
    --suite all \
    --tolerance 1e-7 \
    --output validation_report.json
```

## Reference Test Cases

The validation framework is based on comprehensive test cases from `docs/davinci/test_KRC.dv`, which covers:

- Basic runs (Mars, Europa, Phobos, Bennu, Moon)
- Material properties (thermal inertia, direct specification)
- Two-layer regolith configurations
- Temperature-dependent thermal properties
- Frost/condensation modeling
- Eclipse calculations
- Planetary flux contributions
- Time specification methods (Ls, JD, GD)
- Orbital parameter overrides
- Output control parameters

## Future Enhancements

Potential additions to the validation framework:

1. **Automated regression testing**: CI/CD integration
2. **Performance comparison**: Execution time benchmarks
3. **Memory usage validation**: Compare resource consumption
4. **Extended test coverage**: More edge cases and parameter combinations
5. **Visual comparison tools**: Plot temperature profiles side-by-side
6. **Tolerance analysis**: Determine optimal tolerance values per test case
