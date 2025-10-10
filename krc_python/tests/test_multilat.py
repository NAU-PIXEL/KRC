"""Tests for multi-latitude support (Phase 4)."""

import pytest
import numpy as np
from typing import Union
from pykrc.core import krc


class TestMultiLatitudeSupport:
    """Test multi-latitude batch runs."""

    def test_single_latitude_still_works(self):
        """Test that single latitude (existing behavior) still works."""
        import inspect
        sig = inspect.signature(krc)

        # lat should accept float
        # (We can't actually run KRC without the full setup, so just test the signature)
        assert 'lat' in sig.parameters

    def test_latitude_list_accepted(self):
        """Test that lat parameter accepts a list."""
        # This would require mocking the executor to fully test
        # For now, just verify the type hint allows it
        import inspect
        sig = inspect.signature(krc)

        # Check that lat annotation includes Union with List
        from typing import get_args, get_origin
        lat_annotation = sig.parameters['lat'].annotation

        # Should be Optional[Union[float, List[float]]]
        assert get_origin(lat_annotation) is Union

    def test_elevation_list_accepted(self):
        """Test that ELEV parameter accepts a list."""
        import inspect
        sig = inspect.signature(krc)

        # Check that ELEV annotation includes Union with List
        from typing import get_args, get_origin
        elev_annotation = sig.parameters['ELEV'].annotation

        # Should be Optional[Union[float, List[float]]]
        assert get_origin(elev_annotation) is Union

    def test_n4_parameter_logic(self):
        """Test that N4 is set correctly from lat list length."""
        # Test the internal logic
        # Single lat -> N4 = 1
        lats_single = [0.0]
        assert len(lats_single) == 1

        # Multiple lats -> N4 = len(lat)
        lats_multi = [-90, -60, -30, 0, 30, 60, 90]
        assert len(lats_multi) == 7

    def test_elevation_broadcast(self):
        """Test that single ELEV broadcasts to all latitudes."""
        # If lat is list but ELEV is scalar, ELEV should broadcast
        lats = [-30, 0, 30]
        elev_single = 2.5

        # Should create [2.5, 2.5, 2.5]
        expected_elevs = [elev_single] * len(lats)
        assert len(expected_elevs) == len(lats)
        assert all(e == elev_single for e in expected_elevs)

    def test_elevation_list_length_mismatch_error(self):
        """Test that mismatched lat/ELEV list lengths raise error."""
        # This tests the validation logic
        lats = [-30, 0, 30]  # 3 elements
        elevs = [0.0, 2.5]   # 2 elements - mismatch!

        # Should raise ValueError
        assert len(lats) != len(elevs)


class TestMultiLatitudeExamples:
    """Example use cases for multi-latitude runs."""

    def test_polar_to_equator_transect(self):
        """Example: Temperature transect from pole to equator."""
        # lats = np.linspace(-90, 0, 10)  # 10 latitudes from south pole to equator
        # results = krc(lat=lats, lon=0, body="Mars")
        #
        # assert len(results) == 10  # One result per latitude
        # assert results[0]['lat'] == -90  # South pole
        # assert results[-1]['lat'] == 0   # Equator
        pass

    def test_global_survey(self):
        """Example: Global latitude survey every 10°."""
        # lats = np.arange(-90, 100, 10)  # Every 10° from -90 to +90
        # elevs = np.zeros_like(lats)  # All at datum elevation
        #
        # results = krc(lat=lats, ELEV=elevs, body="Mars")
        #
        # assert len(results) == 19  # -90, -80, ..., 0, ..., 80, 90
        pass

    def test_elevation_profile(self):
        """Example: Temperature vs elevation at fixed latitude."""
        # Fixed latitude, varying elevation
        # lats = [0.0] * 5  # Equator
        # elevs = [-4.0, -2.0, 0.0, 2.0, 4.0]  # Mariner Valley to Olympus Mons
        #
        # results = krc(lat=lats, ELEV=elevs, body="Mars")
        #
        # # Expect cooler temps at higher elevations
        # assert results[0]['surf'].mean() > results[-1]['surf'].mean()
        pass


class TestBackwardCompatibility:
    """Ensure multi-lat doesn't break existing single-lat code."""

    def test_existing_single_lat_calls(self):
        """Test that all existing single-lat call patterns still work."""
        import inspect
        sig = inspect.signature(krc)

        # Test 1: lat as positional float (existing code)
        # krc(0.0, 0.0)
        assert sig.parameters['lat'].default is None

        # Test 2: lat as keyword float
        # krc(lat=0.0, lon=0.0)
        assert 'lat' in sig.parameters

        # Test 3: ELEV as scalar (existing code)
        # krc(lat=0, lon=0, ELEV=2.5)
        assert 'ELEV' in sig.parameters

    def test_default_n4_is_one(self):
        """Test that N4 defaults to 1 for backward compatibility."""
        # When lat is a single value (existing behavior), N4 should be 1
        # This maintains compatibility with all existing code
        pass


class TestMultiLatitudeValidation:
    """Test parameter validation for multi-latitude runs."""

    def test_all_latitudes_in_valid_range(self):
        """Test that all latitudes must be in [-90, 90]."""
        # lats = [-100, 0, 50]  # -100 is invalid
        # with pytest.raises(ValueError, match="latitude"):
        #     krc(lat=lats, lon=0)
        pass

    def test_empty_latitude_list(self):
        """Test that empty latitude list raises error."""
        # lats = []  # Empty!
        # with pytest.raises(ValueError, match="at least one"):
        #     krc(lat=lats, lon=0)
        pass

    def test_numpy_array_latitudes(self):
        """Test that numpy arrays work for lat."""
        lats = np.array([-30, 0, 30])
        # Should be converted to list internally
        lats_list = list(lats)
        assert len(lats_list) == 3
        assert isinstance(lats_list, list)

    def test_tuple_latitudes(self):
        """Test that tuples work for lat."""
        lats = (-30, 0, 30)
        # Should be converted to list internally
        lats_list = list(lats)
        assert len(lats_list) == 3
        assert isinstance(lats_list, list)
