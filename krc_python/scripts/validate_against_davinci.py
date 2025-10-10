#!/usr/bin/env python
"""
Validation script for comparing PyKRC output with krc.dvrc (Davinci).

This script runs comprehensive validation tests to ensure PyKRC produces
identical results to the reference krc.dvrc implementation.

Usage:
    # Run basic validation suite
    python validate_against_davinci.py --krc-home ~/krc --davinci-krc ~/krc.dvrc

    # Run advanced validation suite
    python validate_against_davinci.py --krc-home ~/krc --davinci-krc ~/krc.dvrc --suite advanced

    # Run with custom tolerance and keep files
    python validate_against_davinci.py --krc-home ~/krc --davinci-krc ~/krc.dvrc \
        --tolerance 1e-5 --keep-files
"""

import argparse
import sys
import json
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from pykrc.interface_validator import (
    KRCValidator,
    ValidationTestSuite,
    run_validation_suite
)


def main():
    parser = argparse.ArgumentParser(
        description="Validate PyKRC against krc.dvrc reference implementation"
    )
    parser.add_argument(
        "--krc-home",
        required=True,
        help="Path to KRC installation directory"
    )
    parser.add_argument(
        "--davinci-krc",
        required=True,
        help="Path to krc.dvrc file"
    )
    parser.add_argument(
        "--suite",
        choices=["basic", "advanced", "all"],
        default="basic",
        help="Test suite to run (default: basic)"
    )
    parser.add_argument(
        "--tolerance",
        type=float,
        default=1e-6,
        help="Numerical comparison tolerance (default: 1e-6)"
    )
    parser.add_argument(
        "--keep-files",
        action="store_true",
        help="Keep temporary files after validation"
    )
    parser.add_argument(
        "--output",
        help="Save detailed results to JSON file"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Print detailed comparison results"
    )

    args = parser.parse_args()

    # Initialize validator
    print(f"Initializing validator...")
    print(f"  KRC Home: {args.krc_home}")
    print(f"  Davinci KRC: {args.davinci_krc}")
    print()

    try:
        validator = KRCValidator(
            krc_home=args.krc_home,
            davinci_krc_path=args.davinci_krc
        )
    except Exception as e:
        print(f"Error initializing validator: {e}")
        return 1

    # Run validation suites
    all_results = []

    if args.suite in ["basic", "all"]:
        print("=" * 70)
        print("RUNNING BASIC VALIDATION SUITE")
        print("=" * 70)
        basic_results = run_validation_suite(
            validator,
            test_suite="basic",
            tolerance=args.tolerance
        )
        all_results.append(basic_results)

    if args.suite in ["advanced", "all"]:
        print("\n" + "=" * 70)
        print("RUNNING ADVANCED VALIDATION SUITE")
        print("=" * 70)
        advanced_results = run_validation_suite(
            validator,
            test_suite="advanced",
            tolerance=args.tolerance
        )
        all_results.append(advanced_results)

    # Summarize results
    total_passed = sum(r["passed"] for r in all_results)
    total_failed = sum(r["failed"] for r in all_results)
    total_tests = sum(r["total_tests"] for r in all_results)

    print("\n" + "=" * 70)
    print("FINAL SUMMARY")
    print("=" * 70)
    print(f"Total Tests: {total_tests}")
    print(f"Passed: {total_passed} ({100*total_passed/total_tests:.1f}%)")
    print(f"Failed: {total_failed} ({100*total_failed/total_tests:.1f}%)")
    print()

    # Show detailed failures if verbose
    if args.verbose and total_failed > 0:
        print("\nFAILED TESTS DETAILS:")
        print("-" * 70)
        for result_set in all_results:
            for test in result_set["tests"]:
                if not test["passed"]:
                    print(f"\n{test['name']}:")
                    r = test["result"]
                    if r["pykrc"]["error"]:
                        print(f"  PyKRC Error: {r['pykrc']['error']}")
                    if r["davinci"]["error"]:
                        print(f"  Davinci Error: {r['davinci']['error']}")
                    if r["input_file_comparison"]:
                        inp = r["input_file_comparison"]
                        if not inp["identical"]:
                            print(f"  Input files differ:")
                            print(f"    Lines: {inp['line_count_file1']} vs {inp['line_count_file2']}")
                            if inp["differences"]:
                                print(f"    First difference: {inp['differences'][0]}")
                    if r["float_array_comparison"]:
                        flt = r["float_array_comparison"]
                        if not flt.get("identical", False):
                            print(f"  Output arrays differ:")
                            print(f"    Max relative diff: {flt.get('max_relative_diff', 'N/A')}")
                            print(f"    Mean relative diff: {flt.get('mean_relative_diff', 'N/A')}")

    # Save results to file if requested
    if args.output:
        output_data = {
            "validation_parameters": {
                "krc_home": args.krc_home,
                "davinci_krc": args.davinci_krc,
                "tolerance": args.tolerance,
                "suite": args.suite
            },
            "summary": {
                "total_tests": total_tests,
                "passed": total_passed,
                "failed": total_failed,
                "pass_rate": total_passed / total_tests if total_tests > 0 else 0
            },
            "results": all_results
        }

        with open(args.output, 'w') as f:
            json.dump(output_data, f, indent=2)
        print(f"\nDetailed results saved to: {args.output}")

    # Return exit code
    return 0 if total_failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
