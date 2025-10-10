#!/usr/bin/env python
"""
Example of using the PyKRC validation framework.

This demonstrates how to compare PyKRC output with krc.dvrc
to ensure identical results.
"""

import sys
from pathlib import Path

# Add parent to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from pykrc.interface_validator import KRCValidator


def example_single_comparison():
    """Example: Compare a single PyKRC run with krc.dvrc."""
    print("=" * 70)
    print("Example 1: Single Comparison")
    print("=" * 70)

    # Initialize validator
    # NOTE: Update these paths for your system
    validator = KRCValidator(
        krc_home="~/Applications/krc_3_6_5",
        davinci_krc_path="~/krc.dvrc"
    )

    # Run comparison
    result = validator.compare_run(
        pykrc_params={
            "lat": 12.0,
            "ls": 23.0,
            "INERTIA": 100.0
        },
        davinci_cmd='krc(lat=12.,ls=23.,INERTIA=100.,KEEP="T")',
        tolerance=1e-6
    )

    # Display results
    print(f"\nPyKRC Success: {result['pykrc']['success']}")
    print(f"Davinci Success: {result['davinci']['success']}")
    print(f"Input Files Match: {result['summary']['input_files_match']}")
    print(f"Binary Files Match: {result['summary']['binary_files_match']}")
    print(f"Overall Match: {result['summary']['overall_match']}")

    if result["float_array_comparison"]:
        fa = result["float_array_comparison"]
        print(f"\nNumerical Comparison:")
        print(f"  Values: {fa['num_values']}")
        print(f"  Max Relative Diff: {fa['max_relative_diff']:.2e}")
        print(f"  Mean Relative Diff: {fa['mean_relative_diff']:.2e}")

    return result["summary"]["overall_match"]


def example_europa_comparison():
    """Example: Compare Europa with planetary flux."""
    print("\n" + "=" * 70)
    print("Example 2: Europa with Planetary Flux")
    print("=" * 70)

    validator = KRCValidator(
        krc_home="~/Applications/krc_3_6_5",
        davinci_krc_path="~/krc.dvrc"
    )

    result = validator.compare_run(
        pykrc_params={
            "lat": 0.0,
            "INERTIA": 50.0,
            "body": "Europa",
            "ALBEDO": 0.55,
            "PFlux": "T",
            "Lon_Hr": 12.0,
            "LKofT": False
        },
        davinci_cmd='krc(lat=0.,INERTIA=50.,body="Europa",ALBEDO=0.55,PFlux="T",Lon_Hr=12.,LKofT="F",KEEP="T")',
    )

    print(f"\nOverall Match: {result['summary']['overall_match']}")

    return result["summary"]["overall_match"]


def example_batch_validation():
    """Example: Run multiple validation tests."""
    print("\n" + "=" * 70)
    print("Example 3: Batch Validation")
    print("=" * 70)

    validator = KRCValidator(
        krc_home="~/Applications/krc_3_6_5",
        davinci_krc_path="~/krc.dvrc"
    )

    test_cases = [
        {
            "name": "Mars default",
            "pykrc": {"lat": 25.0},
            "davinci": 'krc(lat=25.,KEEP="T")'
        },
        {
            "name": "Mars with TI",
            "pykrc": {"lat": 12.0, "INERTIA": 200.0},
            "davinci": 'krc(lat=12.,INERTIA=200.,KEEP="T")'
        },
        {
            "name": "Two-layer",
            "pykrc": {"lat": 0.0, "thick": 0.3, "INERTIA": 200.0, "INERTIA2": 1200.0},
            "davinci": 'krc(lat=0.,thick=0.3,INERTIA=200.,INERTIA2=1200.,KEEP="T")'
        },
    ]

    results = []
    for test in test_cases:
        print(f"\nTesting: {test['name']}")
        result = validator.compare_run(
            pykrc_params=test["pykrc"],
            davinci_cmd=test["davinci"]
        )
        passed = result["summary"]["overall_match"]
        results.append(passed)
        print(f"  {'✓ PASSED' if passed else '✗ FAILED'}")

    print(f"\n{'='*70}")
    print(f"Summary: {sum(results)}/{len(results)} tests passed")
    print(f"{'='*70}")

    return all(results)


def main():
    """Run all examples."""
    print("PyKRC Validation Framework Examples\n")

    try:
        # Run examples
        pass1 = example_single_comparison()
        pass2 = example_europa_comparison()
        pass3 = example_batch_validation()

        # Summary
        print("\n" + "=" * 70)
        print("ALL EXAMPLES SUMMARY")
        print("=" * 70)
        print(f"Example 1 (Single): {'PASSED' if pass1 else 'FAILED'}")
        print(f"Example 2 (Europa): {'PASSED' if pass2 else 'FAILED'}")
        print(f"Example 3 (Batch): {'PASSED' if pass3 else 'FAILED'}")

        if all([pass1, pass2, pass3]):
            print("\n✓ All validation examples passed!")
            return 0
        else:
            print("\n✗ Some validation examples failed")
            return 1

    except Exception as e:
        print(f"\nError running examples: {e}")
        print("\nNote: Update paths in this script to match your system:")
        print("  - krc_home: Path to KRC installation")
        print("  - davinci_krc_path: Path to krc.dvrc file")
        return 1


if __name__ == "__main__":
    sys.exit(main())
