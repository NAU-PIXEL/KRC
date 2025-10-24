"""
Unit and integration tests for porb_handler.py functions.

This module tests PORB (Planetary ORBit) parameter handling:
- porb() function for loading body orbital parameters
- generic_porb() function for custom orbital configurations
- Validation against davinci test_KRC.dv lines 3-23

Per CLAUDE.md testing philosophy:
- Test both unit functionality AND integration with KRC
- Reference davinci source lines
- Validate parameter values and types
"""

import pytest
from typing import Dict, Any
from pathlib import Path

from pykrc.porb_handler import (
    porb,
    generic_porb,
    OrbitalElements,
    identify_body_type,
    load_body_parameters,
)
from pykrc.data_loaders import KRCDataLoader


# ==============================================================================
# Test Class 1: Body Type Identification
# ==============================================================================

class TestBodyTypeIdentification:
    """Test body type identification logic."""

    def test_planet_identification(self):
        """Test planet identification."""
        assert identify_body_type("Mars") == "Planet"
        assert identify_body_type("Earth") == "Planet"
        assert identify_body_type("Jupiter") == "Planet"

    def test_satellite_identification(self):
        """Test satellite identification."""
        assert identify_body_type("Moon") == "Satellite"
        assert identify_body_type("Phobos") == "Satellite"
        assert identify_body_type("Europa") == "Satellite"
        assert identify_body_type("Ganymede") == "Satellite"

    def test_asteroid_identification(self):
        """Test asteroid identification."""
        assert identify_body_type("Ceres") == "Asteroid"
        assert identify_body_type("Vesta") == "Asteroid"
        assert identify_body_type("Bennu") == "Asteroid"

    def test_unknown_body(self):
        """Test unknown body returns 'Unknown'."""
        assert identify_body_type("UnknownBody") == "Unknown"


# ==============================================================================
# Test Class 2: porb() Function Tests
# ==============================================================================

class TestPorbFunction:
    """
    Test porb() function for loading body orbital parameters.

    Per test_KRC.dv lines 3-8, tests validate:
    - OUT = porb("Mars", force=1)
    - OUT = porb("Bennu", force=1)
    - OUT = porb("Phobos", force=1)
    - OUT = porb("Europa", force=1)
    - OUT = porb("2688_Halley", force=1)
    - OUT = porb("Ceres", force=1)
    """

    @pytest.fixture
    def data_loader(self):
        """Create KRCDataLoader instance for testing."""
        try:
            return KRCDataLoader()
        except Exception:
            pytest.skip("KRC data files not available")

    def test_porb_mars(self, data_loader):
        """Test porb() for Mars."""
        # Per test_KRC.dv line 3: OUT = porb("Mars", force=1)
        result = porb("Mars", force=True, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "Mars"
        assert result.body_type == "Planet"
        assert hasattr(result, "rotation_period")
        assert result.rotation_period > 0

    def test_porb_phobos(self, data_loader):
        """Test porb() for Phobos."""
        # Per test_KRC.dv line 5: OUT = porb("Phobos", force=1)
        result = porb("Phobos", force=True, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "Phobos"
        assert result.body_type == "Satellite"
        assert result.parent_body == "Mars" or result.parent_body is None

    def test_porb_europa(self, data_loader):
        """Test porb() for Europa."""
        # Per test_KRC.dv line 6: OUT = porb("Europa", force=1)
        result = porb("Europa", force=True, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "Europa"
        assert result.body_type == "Satellite"

    def test_porb_bennu(self, data_loader):
        """Test porb() for Bennu."""
        # Per test_KRC.dv line 4: OUT = porb("Bennu", force=1)
        result = porb("Bennu", force=True, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "Bennu"
        assert result.body_type == "Asteroid"

    def test_porb_ceres(self, data_loader):
        """Test porb() for Ceres."""
        # Per test_KRC.dv line 8: OUT = porb("Ceres", force=1)
        result = porb("Ceres", force=True, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "Ceres"
        assert result.body_type == "Asteroid"

    def test_porb_halley(self, data_loader):
        """Test porb() for 1P-Halley comet."""
        # Per test_KRC.dv line 7: OUT = porb("2688_Halley", force=1)
        # Note: In the HDF files, Halley is named "1P-Halley"
        result = porb("1P-Halley", force=True, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "1P-Halley"
        assert result.body_type == "Comet"

    def test_porb_with_dict_input(self):
        """Test porb() with dictionary input (generic body)."""
        body_dict = {
            "name": "TestBody",
            "rotation_period": 24.0,
            "radius": 6371.0,
        }

        result = porb(body_dict)

        assert isinstance(result, OrbitalElements)
        assert result.name == "TestBody"
        assert result.body_type == "Generic"
        assert result.rotation_period == 24.0
        assert result.radius == 6371.0

    def test_porb_epoch_parameter(self, data_loader):
        """Test porb() with custom epoch parameter."""
        result = porb("Mars", epoch=0.20, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "Mars"


# ==============================================================================
# Test Class 3: generic_porb() Function Tests
# ==============================================================================

class TestGenericPorbFunction:
    """
    Test generic_porb() function for custom orbital configurations.

    Per test_KRC.dv lines 9-11, tests validate:
    - poledec = 0
    - tmp = generic_porb(e=0, a=1., i=1.30439695, node=100.47390909,
                         peri=293.923, m=79.668, rot_per=200.,
                         polera=273.85, poledec=poledec, merid=7.7,
                         period=4332.589, name="IdealJupiterTrojan")
    - OUT = porb(tmp)
    """

    def test_generic_porb_basic(self):
        """Test generic_porb() with basic parameters."""
        result = generic_porb(
            name="TestGeneric",
            e=0.1,
            a=2.0,
            rot_per=10.0,
        )

        assert isinstance(result, OrbitalElements)
        assert result.name == "TestGeneric"
        assert result.body_type == "Generic"
        assert result.eccentricity == 0.1
        assert result.semi_major_axis == 2.0
        assert result.rotation_period == 10.0

    def test_generic_porb_jupiter_trojan(self):
        """Test generic_porb() for IdealJupiterTrojan."""
        # Per test_KRC.dv lines 9-11
        poledec = 0.0
        result = generic_porb(
            e=0.0,
            a=1.0,
            i=1.30439695,
            node=100.47390909,
            peri=293.923,
            m=79.668,
            rot_per=200.0,
            polera=273.85,
            poledec=poledec,
            merid=7.7,
            period=4332.589,
            name="IdealJupiterTrojan"
        )

        assert isinstance(result, OrbitalElements)
        assert result.name == "IdealJupiterTrojan"
        assert result.body_type == "Generic"
        assert result.eccentricity == 0.0
        assert result.semi_major_axis == 1.0
        assert result.inclination == 1.30439695
        assert result.node == 100.47390909
        assert result.perihelion == 293.923
        assert result.mean_anomaly == 79.668
        assert result.rotation_period == 200.0
        assert result.pole_ra == 273.85
        assert result.pole_dec == poledec
        assert result.meridian == 7.7
        assert result.orbital_period == 4332.589

    def test_generic_porb_automatic_period(self):
        """Test generic_porb() automatic orbital period calculation."""
        result = generic_porb(
            name="AutoPeriod",
            a=2.0,  # 2 AU
        )

        assert isinstance(result, OrbitalElements)
        # Kepler's third law: period ≈ 365.25 * a^1.5
        expected_period = 365.25 * (2.0 ** 1.5)
        assert abs(result.orbital_period - expected_period) < 0.1

    def test_generic_porb_all_parameters(self):
        """Test generic_porb() with all parameters specified."""
        result = generic_porb(
            name="CompleteBody",
            epoch=2451545.0,
            e=0.2,
            a=3.0,
            w=45.0,
            i=10.0,
            node=80.0,
            peri=120.0,
            m=90.0,
            rot_per=25.0,
            polera=180.0,
            poledec=45.0,
            merid=30.0,
            period=1000.0,
        )

        assert result.name == "CompleteBody"
        assert result.epoch == 2451545.0
        assert result.eccentricity == 0.2
        assert result.semi_major_axis == 3.0
        assert result.inclination == 10.0
        assert result.node == 80.0
        assert result.perihelion == 120.0
        assert result.mean_anomaly == 90.0
        assert result.rotation_period == 25.0
        assert result.pole_ra == 180.0
        assert result.pole_dec == 45.0
        assert result.meridian == 30.0
        assert result.orbital_period == 1000.0

    def test_generic_porb_default_epoch(self):
        """Test generic_porb() uses default epoch of 2000."""
        result = generic_porb(name="DefaultEpoch")

        assert result.epoch == 2000.0

    def test_generic_porb_default_pole_declination(self):
        """Test generic_porb() uses default pole_dec of 90 degrees."""
        result = generic_porb(name="DefaultPole")

        assert result.pole_dec == 90.0

    def test_generic_porb_zero_eccentricity(self):
        """Test generic_porb() with circular orbit (e=0)."""
        result = generic_porb(name="CircularOrbit", e=0.0, a=1.5)

        assert result.eccentricity == 0.0
        assert result.semi_major_axis == 1.5


# ==============================================================================
# Test Class 4: porb() with generic_porb() Output
# ==============================================================================

class TestPorbWithGenericPorbOutput:
    """
    Test using generic_porb() output as input to porb().

    Per test_KRC.dv line 11:
    - tmp = generic_porb(...)
    - OUT = porb(tmp)
    """

    def test_porb_accepts_generic_porb_output(self):
        """Test that porb() can accept generic_porb() output."""
        # Create generic body
        generic_body = generic_porb(
            name="GenericTest",
            e=0.05,
            a=1.2,
            rot_per=15.0,
        )

        # Convert to dict for porb() input
        body_dict = {
            "name": generic_body.name,
            "eccentricity": generic_body.eccentricity,
            "semi_major_axis": generic_body.semi_major_axis,
            "rotation_period": generic_body.rotation_period,
        }

        # Pass to porb()
        result = porb(body_dict)

        assert isinstance(result, OrbitalElements)
        assert result.name == "GenericTest"
        assert result.body_type == "Generic"
        assert result.eccentricity == 0.05
        assert result.semi_major_axis == 1.2
        assert result.rotation_period == 15.0

    def test_porb_with_jupiter_trojan_from_generic(self):
        """Test porb() with IdealJupiterTrojan from generic_porb()."""
        # Per test_KRC.dv lines 9-11
        generic_trojan = generic_porb(
            e=0.0,
            a=1.0,
            i=1.30439695,
            node=100.47390909,
            peri=293.923,
            m=79.668,
            rot_per=200.0,
            polera=273.85,
            poledec=0.0,
            merid=7.7,
            period=4332.589,
            name="IdealJupiterTrojan"
        )

        # Convert to dict
        body_dict = {
            "name": generic_trojan.name,
            "eccentricity": generic_trojan.eccentricity,
            "semi_major_axis": generic_trojan.semi_major_axis,
            "inclination": generic_trojan.inclination,
            "node": generic_trojan.node,
            "perihelion": generic_trojan.perihelion,
            "mean_anomaly": generic_trojan.mean_anomaly,
            "rotation_period": generic_trojan.rotation_period,
            "pole_ra": generic_trojan.pole_ra,
            "pole_dec": generic_trojan.pole_dec,
            "meridian": generic_trojan.meridian,
            "orbital_period": generic_trojan.orbital_period,
        }

        result = porb(body_dict)

        assert result.name == "IdealJupiterTrojan"
        assert result.body_type == "Generic"
        assert result.eccentricity == 0.0
        assert result.rotation_period == 200.0


# ==============================================================================
# Test Class 5: OrbitalElements Dataclass
# ==============================================================================

class TestOrbitalElements:
    """Test OrbitalElements dataclass functionality."""

    def test_orbital_elements_creation(self):
        """Test creating OrbitalElements instance."""
        elem = OrbitalElements(
            name="TestBody",
            body_type="Planet",
            rotation_period=24.0,
            radius=6371.0,
        )

        assert elem.name == "TestBody"
        assert elem.body_type == "Planet"
        assert elem.rotation_period == 24.0
        assert elem.radius == 6371.0

    def test_orbital_elements_defaults(self):
        """Test OrbitalElements default values."""
        elem = OrbitalElements(
            name="MinimalBody",
            body_type="Generic",
        )

        assert elem.epoch == 0.0
        assert elem.eccentricity == 0.0
        assert elem.semi_major_axis == 0.0
        assert elem.rotation_period == 1.0
        assert elem.pole_dec == 0.0

    def test_orbital_elements_optional_fields(self):
        """Test OrbitalElements optional fields."""
        elem = OrbitalElements(
            name="SatelliteTest",
            body_type="Satellite",
            parent_body="Jupiter",
            orbital_radius=421700.0,  # Io
        )

        assert elem.parent_body == "Jupiter"
        assert elem.orbital_radius == 421700.0


# ==============================================================================
# Test Class 6: Parameter Value Validation
# ==============================================================================

class TestParameterValidation:
    """Test that porb functions return valid parameter values."""

    def test_rotation_period_positive(self):
        """Test rotation period is always positive."""
        result = generic_porb(name="Test", rot_per=50.0)
        assert result.rotation_period > 0

    def test_eccentricity_range(self):
        """Test eccentricity is in valid range [0, 1)."""
        result = generic_porb(name="Test", e=0.5)
        assert 0 <= result.eccentricity < 1.0

    def test_semi_major_axis_positive(self):
        """Test semi-major axis is positive."""
        result = generic_porb(name="Test", a=2.5)
        assert result.semi_major_axis > 0

    def test_inclination_range(self):
        """Test inclination is in valid range."""
        result = generic_porb(name="Test", i=45.0)
        assert 0 <= result.inclination <= 180.0

    def test_angles_wrap(self):
        """Test angle parameters accept full range."""
        result = generic_porb(
            name="Test",
            node=270.0,
            peri=180.0,
            m=90.0,
        )
        assert result.node == 270.0
        assert result.perihelion == 180.0
        assert result.mean_anomaly == 90.0


# ==============================================================================
# Test Class 7: Dynamic PORB Generation
# ==============================================================================

class TestDynamicPorbGeneration:
    """
    Test dynamic PORB generation functions.

    Tests the new functionality for generating PORB data dynamically
    when HDF files don't exist or when force=True.
    """

    @pytest.fixture
    def data_loader(self):
        """Create KRCDataLoader instance for testing."""
        support_dir = Path(__file__).parent.parent / "pykrc" / "data" / "krc_support"
        return KRCDataLoader(support_dir)

    def test_parse_standish_table_earth(self, data_loader):
        """Test parsing Earth from standish.tab."""
        from pykrc.porb_handler import _parse_standish_table

        standish_path = data_loader.support_dir / "standish.tab"
        lines = _parse_standish_table(standish_path, "Earth")

        assert lines is not None
        assert len(lines) == 3
        assert lines[0].startswith("C_END")
        assert "Earth" in lines[1]
        assert "1.00000261" in lines[1]  # Semi-major axis

    def test_parse_standish_table_mars(self, data_loader):
        """Test parsing Mars from standish.tab."""
        from pykrc.porb_handler import _parse_standish_table

        standish_path = data_loader.support_dir / "standish.tab"
        lines = _parse_standish_table(standish_path, "Mars")

        assert lines is not None
        assert len(lines) == 3
        assert "Mars" in lines[1]
        assert "1.52371034" in lines[1]  # Semi-major axis for Mars

    def test_parse_spinaxis_table_moon(self, data_loader):
        """Test parsing Moon from spinaxis.tab."""
        from pykrc.porb_handler import _parse_spinaxis_table

        spinaxis_path = data_loader.support_dir / "spinaxis.tab"
        lines = _parse_spinaxis_table(spinaxis_path, "Moon")

        assert lines is not None
        assert len(lines) == 2
        assert lines[0] == "C_END"
        assert "Moon" in lines[1]
        assert "269.9949" in lines[1]  # RA at epoch

    def test_parse_spinaxis_table_phobos(self, data_loader):
        """Test parsing Phobos from spinaxis.tab."""
        from pykrc.porb_handler import _parse_spinaxis_table

        spinaxis_path = data_loader.support_dir / "spinaxis.tab"
        lines = _parse_spinaxis_table(spinaxis_path, "Phobos")

        assert lines is not None
        assert len(lines) == 2
        assert "Phobos" in lines[1]

    def test_parse_planetary_params_moon(self, data_loader):
        """Test parsing Moon from planetary_params3.csv."""
        from pykrc.porb_handler import _parse_planetary_params

        params_path = data_loader.support_dir / "planetary_params3.csv"
        params = _parse_planetary_params(params_path, "Moon")

        assert params is not None
        assert params['GRAV'] == pytest.approx(1.622, abs=0.01)
        assert params['radius'] == pytest.approx(1737.4, abs=1.0)
        assert params['rotation_period'] > 0

    def test_parse_planetary_params_mars(self, data_loader):
        """Test parsing Mars from planetary_params3.csv."""
        from pykrc.porb_handler import _parse_planetary_params

        params_path = data_loader.support_dir / "planetary_params3.csv"
        params = _parse_planetary_params(params_path, "Mars")

        assert params is not None
        assert params['GRAV'] == pytest.approx(3.711, abs=0.01)
        assert params['PTOTAL'] == pytest.approx(546, abs=1)
        assert params['ARC2_G0'] == 0.5
        assert params['DUSTA'] == 0.9
        assert params['TAURAT'] == pytest.approx(0.22, abs=0.01)

    def test_get_parent_body_moon(self, data_loader):
        """Test getting parent body for Moon."""
        from pykrc.porb_handler import _get_parent_body

        spinaxis_path = data_loader.support_dir / "spinaxis.tab"
        parent = _get_parent_body(spinaxis_path, "Moon")

        assert parent == "Earth"

    def test_get_parent_body_phobos(self, data_loader):
        """Test getting parent body for Phobos."""
        from pykrc.porb_handler import _get_parent_body

        spinaxis_path = data_loader.support_dir / "spinaxis.tab"
        parent = _get_parent_body(spinaxis_path, "Phobos")

        assert parent == "Mars"

    def test_calculate_derived_params(self):
        """Test derived parameter calculations."""
        from pykrc.porb_handler import _calculate_derived_params

        # Mars values
        rot_per_hours = 24.623
        orbital_period = 686.9928

        derived = _calculate_derived_params(rot_per_hours, orbital_period)

        assert derived['N24'] == 96  # Minimum value for Mars
        assert derived['PERIOD'] == pytest.approx(1.0260, abs=0.001)
        assert derived['DELJUL'] == pytest.approx(1.9083, abs=0.001)

    def test_calculate_derived_params_moon(self):
        """Test derived parameter calculations for Moon."""
        from pykrc.porb_handler import _calculate_derived_params

        # Moon values (27.32 days = 655.68 hours)
        rot_per_hours = 655.68
        orbital_period = 365.25636

        derived = _calculate_derived_params(rot_per_hours, orbital_period)

        assert derived['N24'] >= 96  # Should be much larger for Moon
        assert derived['PERIOD'] == pytest.approx(27.32, abs=0.1)
        assert derived['DELJUL'] == pytest.approx(1.0146, abs=0.01)

    def test_porb_moon_dynamic_generation(self, data_loader):
        """Test dynamic PORB generation for Moon."""
        result = porb("Moon", epoch=0.10, force=True, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "Moon"
        assert result.body_type == "Satellite"
        assert hasattr(result, 'porb_header')
        assert hasattr(result, 'porb_text_lines')
        assert hasattr(result, 'porb_params')
        assert hasattr(result, 'krc_params')
        assert len(result.porb_text_lines) == 6
        assert len(result.porb_params) == 30
        assert 'GRAV' in result.krc_params
        assert 'N24' in result.krc_params
        assert result.krc_params['N24'] >= 96

    def test_porb_mars_dynamic_generation(self, data_loader):
        """Test dynamic PORB generation for Mars."""
        result = porb("Mars", epoch=0.10, force=True, data_loader=data_loader)

        assert isinstance(result, OrbitalElements)
        assert result.name == "Mars"
        assert result.body_type == "Planet"
        assert hasattr(result, 'porb_header')
        assert hasattr(result, 'porb_text_lines')
        assert len(result.porb_text_lines) == 6
        assert 'GRAV' in result.krc_params
        assert result.krc_params['GRAV'] == pytest.approx(3.711, abs=0.01)


# ==============================================================================
# Pytest markers
# ==============================================================================

pytestmark = pytest.mark.unit
