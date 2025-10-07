"""Tests for material property calculations."""

import pytest
import numpy as np

from pykrc.materials import (
    get_material_properties,
    calculate_specific_heat,
    calculate_thermal_conductivity,
    calculate_density,
    calculate_porosity,
    calculate_thermal_properties,
    MaterialCoefficients,
)


def test_get_material_properties():
    """Test getting material property coefficients."""
    # Test basalt
    basalt = get_material_properties("basalt")
    assert isinstance(basalt, MaterialCoefficients)
    assert basalt.Sph0 == 609.906
    assert basalt.Cp_Tmin == 90.0

    # Test H2O
    h2o = get_material_properties("H2O")
    assert h2o.Sph0 == 1704.57

    # Test CO2
    co2 = get_material_properties("CO2")
    assert co2.Sph0 == 1399.88


def test_get_unknown_material():
    """Test that unknown material raises error."""
    with pytest.raises(ValueError, match="Unknown material"):
        get_material_properties("unknown_material")


def test_calculate_specific_heat():
    """Test specific heat calculation."""
    coeffs = get_material_properties("basalt")

    # Test at 220K (reference temperature, X=0)
    Cp = calculate_specific_heat(220.0, coeffs)
    assert np.isclose(Cp, coeffs.Sph0)

    # Test at 250K
    Cp = calculate_specific_heat(250.0, coeffs)
    assert Cp > coeffs.Sph0  # Should increase with T for basalt

    # Test out of range
    Cp = calculate_specific_heat(50.0, coeffs)
    assert np.isnan(Cp)


def test_calculate_thermal_conductivity():
    """Test thermal conductivity calculation."""
    coeffs = get_material_properties("H2O")

    # Test at 220K
    k = calculate_thermal_conductivity(220.0, coeffs)
    assert k > 0

    # Test in valid range
    k = calculate_thermal_conductivity(150.0, coeffs)
    assert k > 0

    # Test out of range
    k = calculate_thermal_conductivity(50.0, coeffs)
    assert np.isnan(k)


def test_calculate_density():
    """Test density calculation."""
    coeffs = get_material_properties("basalt")

    # Test at 220K
    rho = calculate_density(220.0, coeffs)
    assert np.isclose(rho, coeffs.Dens0)

    # Test valid range
    rho = calculate_density(200.0, coeffs)
    assert rho > 0


def test_calculate_porosity():
    """Test porosity calculation."""
    # Low thermal inertia = high porosity
    por_high = calculate_porosity(100.0)
    assert 0 < por_high < 1
    assert por_high > 0.5

    # High thermal inertia = low porosity
    por_low = calculate_porosity(2000.0)
    assert 0 < por_low < 1
    assert por_low < 0.1

    # Zero thermal inertia
    por_max = calculate_porosity(0.0)
    assert por_max == 0.6


def test_calculate_thermal_properties():
    """Test full thermal property calculation."""
    props = calculate_thermal_properties(
        material="basalt",
        thermal_inertia=200.0,
        T_user=220.0,
        k_style="Mars"
    )

    # Check structure
    assert "INERTIA" in props
    assert "SPEC_HEAT" in props
    assert "DENSITY" in props
    assert "COND" in props
    assert "Porosity" in props
    assert "ConUp0" in props
    assert "SphUp0" in props
    assert "composition" in props

    # Check values
    assert props["INERTIA"] == 200.0
    assert props["SPEC_HEAT"] > 0
    assert props["DENSITY"] > 0
    assert props["COND"] > 0
    assert 0 < props["Porosity"] < 1
    assert props["composition"] == "basalt"

    # Check thermal inertia relation
    TI_check = np.sqrt(props["COND"] * props["DENSITY"] * props["SPEC_HEAT"])
    assert np.isclose(TI_check, 200.0, rtol=0.01)


def test_different_k_styles():
    """Test different conductivity models."""
    # Mars style
    props_mars = calculate_thermal_properties(
        "basalt", 200.0, 220.0, "Mars"
    )

    # Moon style
    props_moon = calculate_thermal_properties(
        "basalt", 200.0, 220.0, "Moon"
    )

    # Bulk style
    props_bulk = calculate_thermal_properties(
        "basalt", 200.0, 220.0, "Bulk"
    )

    # All should have same basic properties at T_user
    assert props_mars["COND"] == props_moon["COND"] == props_bulk["COND"]

    # But different polynomial coefficients
    assert props_mars["ConUp0"] != props_moon["ConUp0"]


def test_invalid_k_style():
    """Test that invalid k_style raises error."""
    with pytest.raises(ValueError, match="Unknown k_style"):
        calculate_thermal_properties("basalt", 200.0, 220.0, "invalid")
