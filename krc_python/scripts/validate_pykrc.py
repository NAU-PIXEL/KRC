#!/usr/bin/env python
"""
Validation script for comparing PyKRC vs Davinci KRC output.

Usage:
    python validate_pykrc.py [--lat LAT] [--keep] [--tolerance TOL] [--save-files]
"""

import argparse
import sys
from pathlib import Path
import numpy as np

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from pykrc.interface_validator import KRCValidator


def print_section(title):
    """Print a formatted section header."""
    print(f"\n{'=' * 70}")
    print(f"  {title}")
    print('=' * 70)


def main():
    parser = argparse.ArgumentParser(
        description='Validate PyKRC against Davinci KRC',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument('--lat', type=float, default=25.0,
                        help='Latitude to test (default: 25.0)')
    parser.add_argument('--lon', type=float, default=None,
                        help='Longitude to test (default: None)')
    parser.add_argument('--keep', action='store_true',
                        help='Keep temporary files (KEEP=T)')
    parser.add_argument('--tolerance', type=float, default=1e-6,
                        help='Tolerance for float comparison (default: 1e-6)')
    parser.add_argument('--save-files', action='store_true',
                        help='Save input/output files for inspection')
    parser.add_argument('--krc-home', type=str,
                        default='/Users/chaberle/Documents/GitHab/KRC',
                        help='KRC home directory')
    parser.add_argument('--davinci-path', type=str,
                        default='/Users/chaberle/.dvrc',
                        help='Davinci KRC path')

    args = parser.parse_args()

    # Build PyKRC parameters
    pykrc_params = {'lat': args.lat}
    if args.lon is not None:
        pykrc_params['lon'] = args.lon
    if args.keep:
        pykrc_params['KEEP'] = 'T'

    # Build Davinci command
    davinci_cmd = f'krc(lat={args.lat}'
    if args.lon is not None:
        davinci_cmd += f',lon={args.lon}'
    if args.keep:
        davinci_cmd += ',KEEP="T"'
    davinci_cmd += ')'

    print_section('PyKRC vs Davinci KRC Validation')
    print(f'PyKRC params: {pykrc_params}')
    print(f'Davinci cmd:  {davinci_cmd}')
    print(f'Tolerance:    {args.tolerance}')

    # Create validator
    validator = KRCValidator(
        krc_home=args.krc_home,
        davinci_krc_path=args.davinci_path
    )

    # Run comparison
    print_section('Running Comparison')
    try:
        result = validator.compare_run(
            pykrc_params=pykrc_params,
            davinci_cmd=davinci_cmd,
            tolerance=args.tolerance,
            keep_files=args.save_files
        )
    except Exception as e:
        print(f'ERROR: Comparison failed: {e}')
        import traceback
        traceback.print_exc()
        return 1

    # Print results
    print_section('Execution Status')
    print(f"PyKRC succeeded:   {result['pykrc']['success']}")
    print(f"Davinci succeeded: {result['davinci']['success']}")

    if not result['pykrc']['success']:
        print(f"\nPyKRC error: {result['pykrc']['error']}")
    if not result['davinci']['success']:
        print(f"\nDavinci error: {result['davinci']['error']}")

    if not (result['pykrc']['success'] and result['davinci']['success']):
        return 1

    # Input file comparison
    print_section('Input File Comparison')
    inp_cmp = result['input_file_comparison']
    print(f"Identical:      {inp_cmp['identical']}")
    print(f"PyKRC lines:    {inp_cmp['line_count_file1']}")
    print(f"Davinci lines:  {inp_cmp['line_count_file2']}")

    if not inp_cmp['identical']:
        print(f"\nDifferences found:")
        diff_lines = inp_cmp.get('diff_text', '').split('\n')
        # Show first 20 diff lines
        for line in diff_lines[:20]:
            if line.startswith('@@') or line.startswith('+') or line.startswith('-'):
                print(f"  {line}")
        if len(diff_lines) > 20:
            print(f"  ... ({len(diff_lines) - 20} more lines)")

    # Binary file comparison
    print_section('Binary File Comparison')
    bin_cmp = result['binary_file_comparison']
    print(f"Size match:     {bin_cmp['size_match']}")
    print(f"File size:      {bin_cmp['size']:,} bytes")
    print(f"Byte identical: {bin_cmp['byte_identical']}")

    if not bin_cmp['byte_identical']:
        print(f"Byte diffs:     {bin_cmp['num_byte_differences']}")
        print(f"\nFirst differences:")
        for diff in bin_cmp.get('first_differences', [])[:5]:
            print(f"  Offset {diff['offset']:6d}: {diff['byte1']:3d} vs {diff['byte2']:3d}")

    # Float array comparison
    print_section('Float Array Comparison')
    flt_cmp = result['float_array_comparison']
    print(f"Num values:         {flt_cmp['num_values']:,}")
    print(f"Max absolute diff:  {flt_cmp['max_absolute_diff']:.6e} K")
    print(f"Max relative diff:  {flt_cmp['max_relative_diff']:.6e}")
    print(f"Mean absolute diff: {flt_cmp['mean_absolute_diff']:.6e} K")
    print(f"Mean relative diff: {flt_cmp['mean_relative_diff']:.6e}")
    print(f"Within tolerance:   {flt_cmp['values_within_tolerance']}")

    # Overall summary
    print_section('Overall Summary')
    summary = result['summary']
    print(f"Both succeeded:      {summary['both_succeeded']}")
    print(f"Input files match:   {summary['input_files_match']}")
    print(f"Binary files match:  {summary['binary_files_match']}")
    print(f"Float arrays match:  {summary['float_arrays_match']}")
    print(f"Overall match:       {summary['overall_match']}")

    # Save files if requested
    if args.save_files:
        print_section('Saved Files')
        print(f"PyKRC input:   {result['pykrc']['files']['inp']}")
        print(f"PyKRC binary:  {result['pykrc']['files'].get('bin52', 'N/A')}")
        print(f"Davinci input: {result['davinci']['files']['inp']}")
        print(f"Davinci binary: {result['davinci']['files'].get('bin52', 'N/A')}")

    # Return exit code
    if summary['overall_match']:
        print("\n✓ VALIDATION PASSED")
        return 0
    else:
        print("\n✗ VALIDATION FAILED")
        return 1


if __name__ == '__main__':
    sys.exit(main())
