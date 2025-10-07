#!/usr/bin/env python3
"""
Basic usage examples for KRC Python interface.

This script demonstrates simple thermal model runs for Mars.
"""

import os
from pykrc import krc, set_krc_home


def example_basic_mars_run():
    """Run a basic Mars thermal model."""
    print("=" * 60)
    print("Example 1: Basic Mars Surface Temperature")
    print("=" * 60)

    # Run KRC for Mars equator at Ls=270 (southern summer solstice)
    result = krc(
        lat=0.0,           # Equator
        lon=0.0,           # Prime meridian
        body="Mars",
        ls=270,            # Southern summer solstice
        INERTIA=200,       # Thermal inertia (SI units)
        ALBEDO=0.25,       # Surface albedo
        verbose=True
    )

    print("\nResults:")
    print(f"  Body: {result['body']}")
    print(f"  Latitude: {result['lat']}°")
    print(f"  Longitude: {result['lon']}°")
    print(f"  Albedo: {result['alb']}")
    # print(f"  Surface temp: {result['surf']} K")


def example_high_latitude():
    """Run model for high latitude location."""
    print("\n" + "=" * 60)
    print("Example 2: Mars High Latitude (60°N)")
    print("=" * 60)

    result = krc(
        lat=60.0,          # 60°N
        lon=180.0,
        body="Mars",
        ls=90,             # Northern summer solstice
        INERTIA=300,       # Higher thermal inertia
        ALBEDO=0.20,
        SLOPE=10.0,        # 10° slope
        SLOAZI=180.0,      # South-facing slope
        verbose=True
    )

    print("\nHigh latitude results generated.")


def example_different_materials():
    """Compare different surface materials."""
    print("\n" + "=" * 60)
    print("Example 3: Different Material Properties")
    print("=" * 60)

    # Basalt surface
    result_basalt = krc(
        lat=0.0,
        lon=0.0,
        body="Mars",
        ls=0,
        INERTIA=400,
        ALBEDO=0.25,
        Mat1="basalt",
        Mat2="basalt",
        verbose=False
    )
    print("Basalt surface model complete")

    # H2O ice surface (for polar regions)
    result_ice = krc(
        lat=80.0,
        lon=0.0,
        body="Mars",
        ls=0,
        INERTIA=2000,
        ALBEDO=0.65,
        Mat1="H2O",
        Mat2="H2O",
        verbose=False
    )
    print("H2O ice surface model complete")


def example_seasonal_cycle():
    """Model seasonal temperature variations."""
    print("\n" + "=" * 60)
    print("Example 4: Seasonal Cycle")
    print("=" * 60)

    seasons = {
        "Northern Spring Equinox": 0,
        "Northern Summer Solstice": 90,
        "Northern Autumn Equinox": 180,
        "Northern Winter Solstice": 270,
    }

    lat = 45.0  # Mid-latitude

    for season_name, ls_value in seasons.items():
        print(f"\n  Running {season_name} (Ls={ls_value}°)...")
        result = krc(
            lat=lat,
            lon=0.0,
            body="Mars",
            ls=ls_value,
            INERTIA=250,
            ALBEDO=0.25,
            verbose=False
        )
        print(f"    Complete")


def example_custom_parameters():
    """Use custom atmospheric and subsurface parameters."""
    print("\n" + "=" * 60)
    print("Example 5: Custom Parameters")
    print("=" * 60)

    result = krc(
        lat=0.0,
        lon=0.0,
        body="Mars",
        ls=180,
        INERTIA=200,
        ALBEDO=0.25,
        TDEEP=210.0,        # Deep subsurface temperature
        TAUD=0.5,           # Dust optical depth
        k_style="Moon",     # Use lunar conductivity model
        T_user=250.0,       # Reference temperature
        verbose=True
    )

    print("\nCustom parameter run complete.")


def main():
    """Run all examples."""
    # Set KRC_HOME if not already set
    if "KRC_HOME" not in os.environ:
        print("Please set KRC_HOME environment variable or use:")
        print("  set_krc_home('/path/to/krc')")
        print("\nFor this example, assuming KRC is in parent directory...")
        # Uncomment and modify this line:
        # set_krc_home("/path/to/your/KRC")
        return

    print("KRC Python Interface - Basic Examples")
    print("=" * 60)

    # Run examples
    try:
        example_basic_mars_run()
        example_high_latitude()
        example_different_materials()
        example_seasonal_cycle()
        example_custom_parameters()

        print("\n" + "=" * 60)
        print("All examples completed successfully!")
        print("=" * 60)

    except Exception as e:
        print(f"\nError running examples: {e}")
        print("\nMake sure:")
        print("  1. KRC_HOME is set correctly")
        print("  2. KRC has been built (run 'make' in src/)")
        print("  3. Support files exist in krc_support/")


if __name__ == "__main__":
    main()
