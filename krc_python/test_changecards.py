#!/usr/bin/env python3
"""
Test the complete pykrc system with changecard implementation.
Verifies that changecards are written, KRC applies them, and parser reads correct data.
"""

from pykrc import krc, set_krc_home
from pathlib import Path

# Set KRC home
set_krc_home('/Users/chaberle/Documents/GitHab/KRC')

print("Testing pykrc with changecard system...")
print("=" * 60)

# Run KRC for Mars at 25°N latitude with persistent workdir
import tempfile
workdir = Path(tempfile.mkdtemp(prefix="krc_changecard_test_"))
print(f"\nWorking directory: {workdir}")

print("\nRunning KRC for Mars at lat=25.0...")
result = krc(lat=25.0, workdir=workdir)

# Extract surface temperature array
tsurf = result['surf']

print(f"\nResult keys: {list(result.keys())}")
print(f"Surface temp shape: {tsurf.shape}")
print(f"Min temperature: {tsurf.min():.2f} K")
print(f"Max temperature: {tsurf.max():.2f} K")
print(f"Mean temperature: {tsurf.mean():.2f} K")

# Check if dimensions are correct (should be 96 hours x 360 seasons)
expected_hours = 96
expected_seasons = 360

if tsurf.shape[0] == expected_hours and tsurf.shape[1] == expected_seasons:
    print(f"\n✓ Dimensions correct: ({expected_hours}, {expected_seasons})")
else:
    print(f"\n✗ Dimensions wrong: expected ({expected_hours}, {expected_seasons}), got {tsurf.shape}")

# Check if temperature range is reasonable (150-290K for Mars)
if 150 < tsurf.min() < 200 and 250 < tsurf.max() < 300:
    print(f"✓ Temperature range realistic: {tsurf.min():.2f}-{tsurf.max():.2f} K")
else:
    print(f"✗ Temperature range unrealistic: {tsurf.min():.2f}-{tsurf.max():.2f} K")

# Show sample temperatures from first day of last season
print(f"\nSample temperatures (first 10 hours of season 360):")
print(tsurf[:10, -1])

print("\n" + "=" * 60)
print("Test complete!")
