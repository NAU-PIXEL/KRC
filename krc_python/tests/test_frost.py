"""Tests for frost and condensation functionality."""

import pytest
from pykrc.frost import (
    get_frost_params_for_body,
    validate_frost_config,
    get_frost_info
)


class TestGetFrostParamsForBody:
    """Test automatic frost parameter determination."""

    def test_mars_co2_frost(self):
        """Test Mars CO2 frost parameters."""
        TFROST, CFROST, AFROST = get_frost_params_for_body("Mars", PTOTAL=600.0)

        # Mars CO2 frost at 600 Pa should be ~148K
        assert 145 <= TFROST <= 151
        assert CFROST == pytest.approx(590000.0, rel=0.01)  # Latent heat J/kg
        assert AFROST == pytest.approx(0.65, rel=0.01)  # Frost albedo

    def test_mars_pressure_dependence(self):
        """Test that Mars frost parameters are returned for different pressures."""
        # Note: In this simplified implementation, we return constant values
        # Full pressure-dependent calculation is done inside KRC Fortran code
        T_low = get_frost_params_for_body("Mars", PTOTAL=400.0)[0]
        T_mid = get_frost_params_for_body("Mars", PTOTAL=600.0)[0]
        T_high = get_frost_params_for_body("Mars", PTOTAL=800.0)[0]

        # All should return nominal frost temperature (full calc in KRC)
        assert T_low == T_mid == T_high == 148.0

    def test_titan_n2_frost(self):
        """Test Titan N2 frost parameters."""
        result = get_frost_params_for_body("Titan", PTOTAL=140000.0)
        assert result is not None
        TFROST, CFROST, AFROST = result
        assert TFROST == pytest.approx(80.0, rel=0.1)

    def test_pluto_n2_frost(self):
        """Test Pluto N2 frost parameters."""
        result = get_frost_params_for_body("Pluto", PTOTAL=10.0)
        assert result is not None
        TFROST, CFROST, AFROST = result
        assert TFROST == pytest.approx(37.0, rel=0.1)

    def test_airless_body_no_frost(self):
        """Test that airless bodies return None."""
        result = get_frost_params_for_body("Moon", PTOTAL=0.0)
        assert result is None

    def test_unknown_body_no_frost(self):
        """Test that unknown bodies return None."""
        result = get_frost_params_for_body("Europa", PTOTAL=1e-10)
        assert result is None


class TestValidateFrostConfig:
    """Test frost configuration validation."""

    def test_frost_disabled_passes(self):
        """Test that LVFT=False always passes."""
        # Should not raise
        validate_frost_config(
            LVFT=False,
            PTOTAL=None,
            TFROST=None,
            body="Mars"
        )

    def test_frost_without_pressure_fails(self):
        """Test that frost without atmosphere fails."""
        with pytest.raises(ValueError, match="requires atmospheric pressure"):
            validate_frost_config(
                LVFT=True,
                PTOTAL=None,
                TFROST=148.0,
                body="Mars"
            )

    def test_frost_zero_pressure_fails(self):
        """Test that frost with zero pressure fails."""
        with pytest.raises(ValueError, match="requires atmospheric pressure"):
            validate_frost_config(
                LVFT=True,
                PTOTAL=0.0,
                TFROST=148.0,
                body="Mars"
            )

    def test_frost_without_tfrost_but_known_body(self):
        """Test frost without TFROST but known body (should pass)."""
        # Mars frost can be auto-calculated
        validate_frost_config(
            LVFT=True,
            PTOTAL=600.0,
            TFROST=None,
            body="Mars"
        )

    def test_frost_without_tfrost_unknown_body_fails(self):
        """Test frost without TFROST for unknown body fails."""
        with pytest.raises(ValueError, match="requires TFROST"):
            validate_frost_config(
                LVFT=True,
                PTOTAL=1e5,
                TFROST=None,
                body="Europa"
            )


class TestGetFrostInfo:
    """Test frost information retrieval."""

    def test_frost_disabled(self):
        """Test info when frost is disabled."""
        info = get_frost_info(
            LVFT=False,
            TFROST=None,
            CFROST=None,
            AFROST=None,
            body="Mars",
            PTOTAL=None
        )
        assert info["enabled"] is False
        assert info["frost_type"] is None

    def test_mars_frost_info(self):
        """Test Mars frost info."""
        info = get_frost_info(
            LVFT=True,
            TFROST=148.0,
            CFROST=590000.0,
            AFROST=0.65,
            body="Mars",
            PTOTAL=600.0
        )
        assert info["enabled"] is True
        assert info["frost_type"] == "CO2"
        assert info["TFROST"] == 148.0

    def test_titan_frost_info(self):
        """Test Titan frost info."""
        info = get_frost_info(
            LVFT=True,
            TFROST=80.0,
            CFROST=580.0,
            AFROST=16.0,
            body="Titan",
            PTOTAL=140000.0
        )
        assert info["enabled"] is True
        assert info["frost_type"] == "N2"

    def test_custom_frost_info(self):
        """Test custom frost parameters."""
        info = get_frost_info(
            LVFT=True,
            TFROST=100.0,
            CFROST=1000.0,
            AFROST=20.0,
            body="CustomWorld",
            PTOTAL=50000.0
        )
        assert info["enabled"] is True
        assert info["frost_type"] == "custom"
