"""
Integration tests for PyKRC vs Davinci end-to-end validation.

This test suite runs actual KRC simulations with both PyKRC and davinci krc.dvrc,
then compares input files, binary outputs, and numerical results to verify
complete parity.

These tests mirror the scenarios in test_davinci_parity.py but perform actual
simulation runs rather than just checking internal logic.

Usage:
    # Run all integration tests
    pytest tests/test_integration_parity.py -v

    # Run with custom tolerance
    pytest tests/test_integration_parity.py -v --tolerance 1e-5

    # Keep temporary files for debugging
    pytest tests/test_integration_parity.py -v --keep-files
"""

import pytest
import os
from pathlib import Path
from typing import Dict, Any

from pykrc.interface_validator import KRCValidator


# Pytest fixtures for configuration
@pytest.fixture(scope="session")
def krc_home():
    """Get KRC_HOME from environment."""
    home = os.environ.get("KRC_HOME")
    if not home:
        pytest.skip("KRC_HOME not set - skipping integration tests")
    return home


@pytest.fixture(scope="session")
def tolerance(request):
    """Get numerical tolerance from command line or use default."""
    return request.config.getoption("--tolerance", default=1e-6)


@pytest.fixture(scope="session")
def keep_files(request):
    """Get keep-files flag from command line."""
    return request.config.getoption("--keep-files", default=False)


@pytest.fixture(scope="session")
def validator(krc_home):
    """Create KRCValidator instance."""
    # davinci_krc_path is optional - davinci loads krc.dvrc automatically from ~/.dvrc
    return KRCValidator(krc_home=krc_home, davinci_krc_path=None)


# Helper function for test assertions
def assert_run_matches(result: Dict[str, Any], test_name: str):
    """Assert that PyKRC and davinci runs match."""
    # Check both runs succeeded
    assert result["pykrc"]["success"], f"{test_name}: PyKRC failed - {result['pykrc']['error']}"
    assert result["davinci"]["success"], f"{test_name}: Davinci failed - {result['davinci']['error']}"

    # Check output arrays match (primary validation metric)
    float_comp = result["float_array_comparison"]
    assert float_comp is not None, f"{test_name}: No float array comparison available"
    assert float_comp["identical"], (
        f"{test_name}: Output arrays differ - "
        f"max_rel_diff={float_comp.get('max_relative_diff', 'N/A')}, "
        f"mean_rel_diff={float_comp.get('mean_relative_diff', 'N/A')}"
    )

    # Print summary
    print(f"✓ {test_name}: max_rel_diff={float_comp['max_relative_diff']:.2e}")


# ==============================================================================
# Test Class 1: Default Values Edge Cases
# ==============================================================================

class TestDefaultValuesIntegration:
    """Test that default values produce identical simulation results."""

    def test_porb_defaults_mars(self, validator, tolerance, keep_files):
        """Test PORB defaults (FLAY, RLAY, etc.) for Mars."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 25.0,
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=25.,KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="porb_defaults_mars"
        )
        assert_run_matches(result, "PORB defaults (Mars)")

    def test_user_defaults_europa(self, validator, tolerance, keep_files):
        """Test user defaults (FANON, N3, etc.) for Europa."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 0.0,
                "body": "Europa",
                "INERTIA": 50.0,
                "ALBEDO": 0.55,
                "LKofT": False,
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=0.,body="Europa",INERTIA=50.,ALBEDO=.55,LKofT="F",KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="user_defaults_europa"
        )
        assert_run_matches(result, "User defaults (Europa)")


# ==============================================================================
# Test Class 2: Material Properties
# ==============================================================================

class TestMaterialPropertiesIntegration:
    """Test material property calculations through simulation results."""

    def test_basalt_properties(self, validator, tolerance, keep_files):
        """Test basalt thermal properties with INERTIA=250."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 0.0,
                "INERTIA": 250.0,
                "material": "Basalt",
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=0.,INERTIA=250.,material="Basalt",KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="basalt_properties"
        )
        assert_run_matches(result, "Basalt INERTIA=250")

    def test_k_style_moon(self, validator, tolerance, keep_files):
        """Test k_style='Moon' (T³ conductivity trend)."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 12.0,
                "INERTIA": 100.0,
                "k_style": "Moon",
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=12.,INERTIA=100.,k_style="Moon",KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="k_style_moon"
        )
        assert_run_matches(result, "k_style=Moon")

    def test_k_style_bulk(self, validator, tolerance, keep_files):
        """Test k_style='Bulk' (direct polynomial, no normalization)."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 12.0,
                "INERTIA": 100.0,
                "k_style": "Bulk",
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=12.,INERTIA=100.,k_style="Bulk",KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="k_style_bulk"
        )
        assert_run_matches(result, "k_style=Bulk")


# ==============================================================================
# Test Class 3: Parameter Resolution Order
# ==============================================================================

class TestParameterResolutionIntegration:
    """Test parameter resolution precedence through simulation results."""

    def test_dells_blocks_deljul(self, validator, tolerance, keep_files):
        """Test that user-set DELLS blocks PORB DELJUL."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 25.0,
                "DELLS": 2.0,  # User sets DELLS
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=25.,DELLS=2.,KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="dells_blocks_deljul"
        )
        assert_run_matches(result, "DELLS blocks DELJUL")

    def test_user_param_precedence(self, validator, tolerance, keep_files):
        """Test user parameter takes precedence over PORB."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 25.0,
                "EMISS": 0.95,  # User overrides PORB default
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=25.,EMISS=0.95,KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="user_param_precedence"
        )
        assert_run_matches(result, "User overrides PORB")


# ==============================================================================
# Test Class 4: Edge Cases and Constraints
# ==============================================================================

class TestEdgeCasesIntegration:
    """Test edge cases and physical constraints."""

    def test_ptotal_forces_taud_zero(self, validator, tolerance, keep_files):
        """Test PTOTAL < 1.0 forces TAUD = 0."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 0.0,
                "PTOTAL": 0.5,  # Less than 1 Pa
                "TAUD": 0.1,    # Should be forced to 0
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=0.,PTOTAL=0.5,TAUD=0.1,KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="ptotal_forces_taud_zero"
        )
        assert_run_matches(result, "PTOTAL<1 forces TAUD=0")

    def test_tpredict_stability_override(self, validator, tolerance, keep_files):
        """Test TPREDICT=0.0 triggers stability overrides."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 25.0,
                "TPREDICT": 0.0,  # Should trigger GGT=99, N3=1, NRSET=999
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=25.,TPREDICT=0.0,KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="tpredict_stability_override"
        )
        assert_run_matches(result, "TPREDICT=0 stability")

    def test_two_layer_regolith(self, validator, tolerance, keep_files):
        """Test two-layer regolith configuration."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 0.0,
                "lon": 0.0,
                "thick": 0.3,
                "INERTIA": 200.0,
                "INERTIA2": 1200.0,
                "LKofT": False,
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=0.,lon=0.,thick=0.3,INERTIA=200.,INERTIA2=1200.,LKofT="F",KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="two_layer_regolith"
        )
        assert_run_matches(result, "Two-layer regolith")


# ==============================================================================
# Test Class 5: Advanced Changecard Features
# ==============================================================================

class TestAdvancedChangecardsIntegration:
    """Test Type 14/15 changecards (Eclipse, PFlux)."""

    def test_type14_eclipse_daily(self, validator, tolerance, keep_files):
        """Test Type 14 changecard - Eclipse Style 1.0 (daily)."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 0.0,
                "INERTIA": 45.0,
                "N1": 32,
                "body": "Europa",
                "Eclipse": "T",
                "Eclipser": "Jupiter",
                "Eclipse_Style": 1.0,
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=0.,INERTIA=045.,N1=32,body="Europa",Eclipse="T",Eclipser="Jupiter",Eclipse_Style=1.,KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="type14_eclipse_daily"
        )
        assert_run_matches(result, "Eclipse Style 1.0 (daily)")

    def test_type15_pflux(self, validator, tolerance, keep_files):
        """Test Type 15 changecard - Planetary Flux."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 0.0,
                "INERTIA": 50.0,
                "body": "Europa",
                "ALBEDO": 0.55,
                "PFlux": "T",
                "Lon_Hr": 12.0,
                "LKofT": False,
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=0.,INERTIA=50.,body="Europa",ALBEDO=0.55,PFlux="T",Lon_Hr=12.,LKofT="F",KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="type15_pflux"
        )
        assert_run_matches(result, "PFlux (Type 15)")


# ==============================================================================
# Test Class 6: Comprehensive Scenarios
# ==============================================================================

class TestComprehensiveScenariosIntegration:
    """Test complex multi-parameter scenarios."""

    def test_mars_with_ls_and_inertia(self, validator, tolerance, keep_files):
        """Test Mars with Ls and thermal inertia."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 12.0,
                "ls": 123.0,
                "INERTIA": 250.0,
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=12.,ls=123.,INERTIA=250.,KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="mars_with_ls_and_inertia"
        )
        assert_run_matches(result, "Mars Ls + INERTIA")

    def test_phobos_default(self, validator, tolerance, keep_files):
        """Test Phobos with defaults."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 12.0,
                "body": "Phobos",
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=12.,body="Phobos",KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="phobos_default"
        )
        assert_run_matches(result, "Phobos default")

    def test_high_inertia_low_albedo(self, validator, tolerance, keep_files):
        """Test high thermal inertia with low albedo."""
        result = validator.compare_run(
            pykrc_params={
                "lat": 0.0,
                "INERTIA": 1200.0,
                "ALBEDO": 0.05,
                "KEEP": "T"
            },
            davinci_cmd='krc(lat=0.,INERTIA=1200.,ALBEDO=0.05,KEEP="T")',
            tolerance=tolerance,
            keep_files=keep_files,
            test_name="high_inertia_low_albedo"
        )
        assert_run_matches(result, "High INERTIA + low ALBEDO")


# ==============================================================================
# Pytest configuration
# ==============================================================================

def pytest_addoption(parser):
    """Add custom command line options."""
    parser.addoption(
        "--tolerance",
        type=float,
        default=1e-6,
        help="Numerical comparison tolerance (default: 1e-6)"
    )
    parser.addoption(
        "--keep-files",
        action="store_true",
        default=False,
        help="Keep temporary files after tests"
    )


def pytest_configure(config):
    """Configure pytest with custom markers."""
    config.addinivalue_line(
        "markers",
        "integration: mark test as integration test (requires KRC_HOME and DAVINCI_KRC)"
    )


# Mark all tests in this module as integration tests
pytestmark = pytest.mark.integration
