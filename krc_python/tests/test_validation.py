"""Tests for parameter validation."""

import pytest
import numpy as np
from pykrc.validation import (
    validate_location,
    validate_material_properties,
    validate_time_parameters,
    validate_numerical_parameters,
    validate_atmospheric_parameters,
    KRCValidationError
)


class TestValidateLocation:
    """Test location parameter validation."""

    def test_valid_location(self):
        """Test valid lat/lon passes."""
        validate_location(lat=0.0, lon=180.0, ELEV=0.0)

    def test_latitude_bounds(self):
        """Test latitude must be in [-90, 90]."""
        with pytest.raises(KRCValidationError, match="Latitude"):
            validate_location(lat=91.0, lon=0.0)

        with pytest.raises(KRCValidationError, match="Latitude"):
            validate_location(lat=-91.0, lon=0.0)

    def test_longitude_bounds(self):
        """Test longitude must be in [0, 360]."""
        with pytest.raises(KRCValidationError, match="Longitude"):
            validate_location(lat=0.0, lon=-1.0)

        with pytest.raises(KRCValidationError, match="Longitude"):
            validate_location(lat=0.0, lon=361.0)

    def test_elevation_bounds(self):
        """Test elevation must be reasonable."""
        with pytest.raises(KRCValidationError, match="Elevation"):
            validate_location(lat=0.0, lon=0.0, ELEV=-15.0)

        with pytest.raises(KRCValidationError, match="Elevation"):
            validate_location(lat=0.0, lon=0.0, ELEV=35.0)


class TestValidateMaterialProperties:
    """Test material property validation."""

    def test_valid_inertia(self):
        """Test valid INERTIA passes."""
        validate_material_properties(INERTIA=200.0, ALBEDO=0.25, EMISS=1.0)

    def test_valid_direct_properties(self):
        """Test valid direct properties pass."""
        validate_material_properties(
            COND=0.05,
            DENSITY=1500.0,
            SPEC_HEAT=800.0,
            ALBEDO=0.25,
            EMISS=1.0
        )

    def test_missing_properties_fails(self):
        """Test that missing properties fails."""
        with pytest.raises(KRCValidationError, match="Must specify either"):
            validate_material_properties(ALBEDO=0.25, EMISS=1.0)

    def test_incomplete_direct_properties_fails(self):
        """Test that incomplete direct properties fails."""
        with pytest.raises(KRCValidationError, match="Must specify either"):
            validate_material_properties(COND=0.05, DENSITY=1500.0)

    def test_inertia_bounds(self):
        """Test INERTIA must be in reasonable range."""
        with pytest.raises(KRCValidationError, match="INERTIA"):
            validate_material_properties(INERTIA=1.0)  # Too low

        with pytest.raises(KRCValidationError, match="INERTIA"):
            validate_material_properties(INERTIA=10000.0)  # Too high

    def test_cond_bounds(self):
        """Test COND must be in reasonable range."""
        with pytest.raises(KRCValidationError, match="COND"):
            validate_material_properties(COND=0.0001, DENSITY=1500.0, SPEC_HEAT=800.0)

    def test_density_bounds(self):
        """Test DENSITY must be in reasonable range."""
        with pytest.raises(KRCValidationError, match="DENSITY"):
            validate_material_properties(COND=0.05, DENSITY=50.0, SPEC_HEAT=800.0)

    def test_albedo_bounds(self):
        """Test ALBEDO must be in [0, 1]."""
        with pytest.raises(KRCValidationError, match="ALBEDO"):
            validate_material_properties(INERTIA=200.0, ALBEDO=1.5)

    def test_emiss_bounds(self):
        """Test EMISS must be in [0.1, 1]."""
        with pytest.raises(KRCValidationError, match="EMISS"):
            validate_material_properties(INERTIA=200.0, EMISS=0.05)


class TestValidateTimeParameters:
    """Test time control validation."""

    def test_valid_time_params(self):
        """Test valid time parameters pass."""
        validate_time_parameters(DELLS=1.0, N5=1080, JDISK=721, PERIOD=1.0275)

    def test_dells_bounds(self):
        """Test DELLS must be in reasonable range."""
        with pytest.raises(KRCValidationError, match="DELLS"):
            validate_time_parameters(DELLS=0.0, N5=1080, JDISK=721, PERIOD=1.0)

    def test_n5_bounds(self):
        """Test N5 must be positive."""
        with pytest.raises(KRCValidationError, match="N5"):
            validate_time_parameters(DELLS=1.0, N5=0, JDISK=721, PERIOD=1.0)

    def test_jdisk_less_than_n5(self):
        """Test JDISK must be < N5."""
        with pytest.raises(KRCValidationError, match="JDISK.*must be < N5"):
            validate_time_parameters(DELLS=1.0, N5=1000, JDISK=1000, PERIOD=1.0)


class TestValidateNumericalParameters:
    """Test numerical control validation."""

    def test_valid_numerical_params(self):
        """Test valid numerical parameters pass."""
        validate_numerical_parameters(N1=50, N2=288, N3=10, NRSET=0)

    def test_n1_bounds(self):
        """Test N1 must be in reasonable range."""
        with pytest.raises(KRCValidationError, match="N1"):
            validate_numerical_parameters(N1=5, N2=288, N3=10, NRSET=0)

        with pytest.raises(KRCValidationError, match="N1"):
            validate_numerical_parameters(N1=300, N2=288, N3=10, NRSET=0)

    def test_n2_bounds(self):
        """Test N2 must be in reasonable range."""
        with pytest.raises(KRCValidationError, match="N2"):
            validate_numerical_parameters(N1=50, N2=10, N3=10, NRSET=0)


class TestValidateAtmosphericParameters:
    """Test atmospheric parameter validation."""

    def test_valid_atm_params(self):
        """Test valid atmospheric parameters pass."""
        validate_atmospheric_parameters(TAUD=0.3, PTOTAL=600.0, TATM=200.0)

    def test_taud_bounds(self):
        """Test TAUD must be non-negative."""
        with pytest.raises(KRCValidationError, match="TAUD"):
            validate_atmospheric_parameters(TAUD=-0.1, PTOTAL=600.0, TATM=200.0)

    def test_ptotal_bounds(self):
        """Test PTOTAL must be non-negative."""
        with pytest.raises(KRCValidationError, match="PTOTAL"):
            validate_atmospheric_parameters(TAUD=0.3, PTOTAL=-100.0, TATM=200.0)

    def test_tatm_bounds(self):
        """Test TATM must be in reasonable range."""
        with pytest.raises(KRCValidationError, match="TATM"):
            validate_atmospheric_parameters(TAUD=0.3, PTOTAL=600.0, TATM=5.0)
