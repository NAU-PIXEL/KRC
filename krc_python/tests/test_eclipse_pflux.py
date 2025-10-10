"""Tests for Eclipse and Planetary Flux parameters."""

import pytest
import inspect
from typing import get_origin, get_args, Union, Optional
from pykrc.core import krc


class TestEclipseParameters:
    """Test Eclipse modeling parameter support."""

    def test_eclipse_parameters_exist(self):
        """Test that all Eclipse parameters are in the signature."""
        sig = inspect.signature(krc)
        eclipse_params = [
            "Eclipse", "Eclipse_Style", "Eclipser", "Sun_Dis",
            "Eclipser_Rad", "Eclipsed_Rad", "CM", "Gamma",
            "Date", "Eclipse_line"
        ]
        for param in eclipse_params:
            assert param in sig.parameters, f"Missing Eclipse parameter: {param}"

    def test_eclipse_default_disabled(self):
        """Test that Eclipse is disabled by default."""
        sig = inspect.signature(krc)
        assert sig.parameters["Eclipse"].default == "F"

    def test_eclipse_style_default(self):
        """Test Eclipse_Style default value."""
        sig = inspect.signature(krc)
        assert sig.parameters["Eclipse_Style"].default == 1.0

    def test_eclipse_optional_params_are_optional(self):
        """Test that Eclipse optional parameters have None default."""
        sig = inspect.signature(krc)
        optional_params = ["Eclipser", "Sun_Dis", "Eclipser_Rad",
                          "Eclipsed_Rad", "CM", "Gamma", "Date", "Eclipse_line"]
        for param in optional_params:
            assert sig.parameters[param].default is None


class TestPlanetaryFluxParameters:
    """Test Planetary Flux parameter support."""

    def test_pflux_parameters_exist(self):
        """Test that all PFlux parameters are in the signature."""
        sig = inspect.signature(krc)
        pflux_params = [
            "PFlux", "BT_Avg", "BT_Min", "BT_Max",
            "Lon_Hr", "IR", "Vis", "Emissivity"
        ]
        for param in pflux_params:
            assert param in sig.parameters, f"Missing PFlux parameter: {param}"

    def test_pflux_default_disabled(self):
        """Test that PFlux is disabled by default."""
        sig = inspect.signature(krc)
        assert sig.parameters["PFlux"].default == "F"

    def test_lon_hr_default(self):
        """Test Lon_Hr default value."""
        sig = inspect.signature(krc)
        assert sig.parameters["Lon_Hr"].default == 12.0

    def test_pflux_optional_params_are_optional(self):
        """Test that PFlux optional parameters have None default."""
        sig = inspect.signature(krc)
        optional_params = ["BT_Avg", "BT_Min", "BT_Max", "IR", "Vis", "Emissivity"]
        for param in optional_params:
            assert sig.parameters[param].default is None


class TestOrbitalOverrideParameters:
    """Test orbital parameter override support."""

    def test_orbital_override_params_exist(self):
        """Test that all orbital override parameters are in the signature."""
        sig = inspect.signature(krc)
        orbital_params = [
            "GRAV", "DAU", "SOLCON", "SOLARDEC",
            "ARC2_G0", "LsubS", "Atm_Cp"
        ]
        for param in orbital_params:
            assert param in sig.parameters, f"Missing orbital override: {param}"

    def test_orbital_overrides_are_optional(self):
        """Test that all orbital overrides default to None."""
        sig = inspect.signature(krc)
        orbital_params = [
            "GRAV", "DAU", "SOLCON", "SOLARDEC",
            "ARC2_G0", "LsubS", "Atm_Cp"
        ]
        for param in orbital_params:
            assert sig.parameters[param].default is None


class TestTimeSpecificationParameters:
    """Test alternative time specification parameters."""

    def test_time_params_exist(self):
        """Test that JD and GD parameters exist."""
        sig = inspect.signature(krc)
        assert "JD" in sig.parameters
        assert "GD" in sig.parameters
        assert "LKEY" in sig.parameters

    def test_lkey_default(self):
        """Test that LKEY defaults to 'T' (use Ls)."""
        sig = inspect.signature(krc)
        assert sig.parameters["LKEY"].default == "T"

    def test_jd_gd_are_optional(self):
        """Test that JD and GD default to None."""
        sig = inspect.signature(krc)
        assert sig.parameters["JD"].default is None
        assert sig.parameters["GD"].default is None


class TestControlParameters:
    """Test additional control parameters."""

    def test_control_params_exist(self):
        """Test that all control parameters exist."""
        sig = inspect.signature(krc)
        control_params = ["KPREF", "LZONE", "bodyforce", "LMST", "WRITE", "KEEP", "JBARE"]
        for param in control_params:
            assert param in sig.parameters, f"Missing control parameter: {param}"

    def test_kpref_default(self):
        """Test KPREF default (Viking pressure model)."""
        sig = inspect.signature(krc)
        assert sig.parameters["KPREF"].default == 1

    def test_lzone_default(self):
        """Test LZONE default (no zone file)."""
        sig = inspect.signature(krc)
        assert sig.parameters["LZONE"].default == "F"

    def test_bodyforce_default(self):
        """Test bodyforce default (use cached PORB)."""
        sig = inspect.signature(krc)
        assert sig.parameters["bodyforce"].default == 0

    def test_jbare_default(self):
        """Test JBARE default (disabled)."""
        sig = inspect.signature(krc)
        assert sig.parameters["JBARE"].default == 0

    def test_output_control_defaults(self):
        """Test output control parameter defaults."""
        sig = inspect.signature(krc)
        assert sig.parameters["LMST"].default == "F"
        assert sig.parameters["WRITE"].default == "F"
        assert sig.parameters["KEEP"].default == "F"


class TestTimeVaryingParameters:
    """Test time-varying parameter support."""

    def test_albedo_accepts_list(self):
        """Test that ALBEDO parameter can accept a list."""
        sig = inspect.signature(krc)
        albedo_annotation = sig.parameters["ALBEDO"].annotation
        assert get_origin(albedo_annotation) is Union
        args = get_args(albedo_annotation)
        # Should be Optional[Union[float, List[float]]]
        # Which expands to Union[float, List[float], None]
        assert float in args or any(get_origin(arg) is Union and float in get_args(arg) for arg in args)

    def test_taud_accepts_list(self):
        """Test that TAUD parameter can accept a list."""
        sig = inspect.signature(krc)
        taud_annotation = sig.parameters["TAUD"].annotation
        assert get_origin(taud_annotation) is Union
        args = get_args(taud_annotation)
        # Should be Optional[Union[float, List[float]]]
        assert float in args or any(get_origin(arg) is Union and float in get_args(arg) for arg in args)


class TestAdvancedParameters:
    """Test advanced computational parameters."""

    def test_stability_param_exists(self):
        """Test that stability parameter exists."""
        sig = inspect.signature(krc)
        assert "stability" in sig.parameters

    def test_anc_param_exists(self):
        """Test that anc parameter exists."""
        sig = inspect.signature(krc)
        assert "anc" in sig.parameters

    def test_advanced_params_optional(self):
        """Test that advanced parameters default to None."""
        sig = inspect.signature(krc)
        assert sig.parameters["stability"].default is None
        assert sig.parameters["anc"].default is None


class TestParameterCoverage:
    """Test overall parameter coverage vs krc.dvrc."""

    def test_total_parameter_count(self):
        """Test that we have close to 85 parameters (krc.dvrc parity)."""
        sig = inspect.signature(krc)
        # Exclude kwargs and standard Python params
        param_count = len([p for p in sig.parameters if p not in ['kwargs']])
        # Should have 85+ parameters for full krc.dvrc parity
        assert param_count >= 85, f"Only {param_count} parameters, need 85+ for full parity"

    def test_all_phase4_params_present(self):
        """Test that all Phase 4 specialized parameters are present."""
        sig = inspect.signature(krc)
        phase4_params = [
            # Eclipse (10)
            "Eclipse", "Eclipse_Style", "Eclipser", "Sun_Dis",
            "Eclipser_Rad", "Eclipsed_Rad", "CM", "Gamma", "Date", "Eclipse_line",
            # PFlux (8)
            "PFlux", "BT_Avg", "BT_Min", "BT_Max", "Lon_Hr", "IR", "Vis", "Emissivity",
            # Time-varying (already tested)
            # Multi-latitude (already tested in test_multilat.py)
            # Control parameters (9)
            "KPREF", "LZONE", "LKEY", "JD", "GD", "bodyforce", "LMST", "WRITE", "KEEP",
            "JBARE",
            # Orbital overrides (7)
            "GRAV", "DAU", "SOLCON", "SOLARDEC", "ARC2_G0", "LsubS", "Atm_Cp",
            # Advanced (2)
            "stability", "anc"
        ]
        for param in phase4_params:
            assert param in sig.parameters, f"Missing Phase 4 parameter: {param}"


class TestBackwardCompatibility:
    """Test that new parameters don't break existing functionality."""

    def test_all_phase4_params_optional(self):
        """Test that all Phase 4 parameters are optional (have defaults)."""
        sig = inspect.signature(krc)
        phase4_params = [
            "Eclipse", "Eclipse_Style", "Eclipser", "Sun_Dis",
            "Eclipser_Rad", "Eclipsed_Rad", "CM", "Gamma", "Date", "Eclipse_line",
            "PFlux", "BT_Avg", "BT_Min", "BT_Max", "Lon_Hr", "IR", "Vis", "Emissivity",
            "KPREF", "LZONE", "LKEY", "JD", "GD", "bodyforce", "LMST", "WRITE", "KEEP",
            "JBARE", "GRAV", "DAU", "SOLCON", "SOLARDEC", "ARC2_G0", "LsubS", "Atm_Cp",
            "stability", "anc"
        ]
        for param in phase4_params:
            param_obj = sig.parameters[param]
            assert param_obj.default is not inspect.Parameter.empty, \
                f"Phase 4 parameter {param} must have a default value for backward compatibility"

    def test_phase4_params_sensible_defaults(self):
        """Test that Phase 4 parameters have sensible defaults."""
        sig = inspect.signature(krc)
        # Boolean-like flags should default to disabled
        assert sig.parameters["Eclipse"].default == "F"
        assert sig.parameters["PFlux"].default == "F"
        assert sig.parameters["LZONE"].default == "F"
        assert sig.parameters["LMST"].default == "F"
        assert sig.parameters["WRITE"].default == "F"
        assert sig.parameters["KEEP"].default == "F"
        # Numerical flags should default to standard values
        assert sig.parameters["KPREF"].default == 1
        assert sig.parameters["bodyforce"].default == 0
        assert sig.parameters["JBARE"].default == 0
        # Optional parameters should default to None
        assert sig.parameters["JD"].default is None
        assert sig.parameters["GD"].default is None
