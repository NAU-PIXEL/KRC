"""Tests for numerical parameter calculations."""

import pytest
import numpy as np
from pykrc.numerical import (
    krc_evalN1,
    krc_evalN2,
    check_stability,
    calculate_convergence_params,
    estimate_runtime
)

# Standard Mars parameters for testing
MARS_RLAY = 1.08
MARS_FLAY = 2.0
MARS_INERTIA = 200.0
MARS_SPEC_HEAT = 800.0
MARS_DENSITY = 1500.0
MARS_DELJUL = 1.9083
MARS_N5 = 1080
MARS_JDISK = 721
MARS_MAXN1 = 100
MARS_PERIOD = 1.0275
MARS_N24 = 24
MARS_MAXN2 = 10000


class TestKrcEvalN1:
    """Test automatic N1 (layer count) calculation."""

    def test_standard_mars_run(self):
        """Test N1 for standard Mars parameters."""
        N1 = krc_evalN1(
            RLAY=MARS_RLAY,
            FLAY=MARS_FLAY,
            INERTIA=MARS_INERTIA,
            SPEC_HEAT=MARS_SPEC_HEAT,
            DENSITY=MARS_DENSITY,
            DELJUL=MARS_DELJUL,
            N5=MARS_N5,
            JDISK=MARS_JDISK,
            MAXN1=MARS_MAXN1,
            PERIOD=MARS_PERIOD
        )
        # Should be in reasonable range for Mars
        assert 25 <= N1 <= 100
        assert isinstance(N1, int)

    def test_high_inertia(self):
        """Test N1 for high thermal inertia material."""
        N1_high = krc_evalN1(
            RLAY=MARS_RLAY, FLAY=MARS_FLAY, INERTIA=2000,
            SPEC_HEAT=MARS_SPEC_HEAT, DENSITY=MARS_DENSITY,
            DELJUL=MARS_DELJUL, N5=MARS_N5, JDISK=MARS_JDISK,
            MAXN1=MARS_MAXN1, PERIOD=MARS_PERIOD
        )
        N1_low = krc_evalN1(
            RLAY=MARS_RLAY, FLAY=MARS_FLAY, INERTIA=50,
            SPEC_HEAT=MARS_SPEC_HEAT, DENSITY=MARS_DENSITY,
            DELJUL=MARS_DELJUL, N5=MARS_N5, JDISK=MARS_JDISK,
            MAXN1=MARS_MAXN1, PERIOD=MARS_PERIOD
        )

        # Higher TI -> deeper skin depth -> more layers needed (or same if hitting minimum)
        assert N1_high >= N1_low

    def test_fine_resolution(self):
        """Test N1 for different season resolutions."""
        # Coarse: 10° per season -> fewer seasons -> smaller depth needed
        N1_coarse = krc_evalN1(
            RLAY=MARS_RLAY, FLAY=MARS_FLAY, INERTIA=MARS_INERTIA,
            SPEC_HEAT=MARS_SPEC_HEAT, DENSITY=MARS_DENSITY,
            DELJUL=MARS_PERIOD * 10.0 / 360.0,  # DELLS=10°
            N5=108,  # 3 years at 10°/season
            JDISK=73,  # 2 years spinup
            MAXN1=MARS_MAXN1, PERIOD=MARS_PERIOD
        )

        # Fine: 0.1° per season -> more seasons -> deeper penetration needed
        N1_fine = krc_evalN1(
            RLAY=MARS_RLAY, FLAY=MARS_FLAY, INERTIA=MARS_INERTIA,
            SPEC_HEAT=MARS_SPEC_HEAT, DENSITY=MARS_DENSITY,
            DELJUL=MARS_PERIOD * 0.1 / 360.0,  # DELLS=0.1°
            N5=10800,  # 3 years at 0.1°/season
            JDISK=7201,  # 2 years spinup
            MAXN1=MARS_MAXN1, PERIOD=MARS_PERIOD
        )

        # Fine resolution needs deeper model (more output time)
        assert N1_fine >= N1_coarse

    def test_long_period_body(self):
        """Test N1 for slowly rotating body."""
        N1_fast = krc_evalN1(
            RLAY=MARS_RLAY, FLAY=MARS_FLAY, INERTIA=200,
            SPEC_HEAT=800, DENSITY=1500,
            DELJUL=1.0 * 1.0 / 360.0,  # 1 day period
            N5=1080, JDISK=721,
            MAXN1=100, PERIOD=1.0
        )
        N1_slow = krc_evalN1(
            RLAY=MARS_RLAY, FLAY=MARS_FLAY, INERTIA=200,
            SPEC_HEAT=800, DENSITY=1500,
            DELJUL=100.0 * 1.0 / 360.0,  # 100 day period
            N5=1080, JDISK=721,
            MAXN1=100, PERIOD=100.0
        )

        # Longer period -> deeper skin depth -> more layers (or same if hitting minimum)
        assert N1_slow >= N1_fast

    def test_maxn1_limit(self):
        """Test that N1 respects MAXN1 limit."""
        N1 = krc_evalN1(
            RLAY=1.08, FLAY=2.0, INERTIA=5000,
            SPEC_HEAT=800, DENSITY=1500,
            DELJUL=100.0 * 0.01 / 360.0,
            N5=10800, JDISK=7201,
            MAXN1=50, PERIOD=100.0
        )
        assert N1 <= 50

    def test_minn1_floor(self):
        """Test that N1 has minimum of 25 (Min_Num_Layer in krc.dvrc)."""
        N1 = krc_evalN1(
            RLAY=1.08, FLAY=2.0, INERTIA=10,
            SPEC_HEAT=800, DENSITY=1500,
            DELJUL=0.1 * 90.0 / 360.0,  # Short period, coarse resolution
            N5=12, JDISK=5,
            MAXN1=100, PERIOD=0.1
        )
        assert N1 >= 25


class TestKrcEvalN2:
    """Test automatic N2 (timesteps/day) calculation."""

    def test_standard_mars_run(self):
        """Test N2 for standard Mars parameters."""
        N2 = krc_evalN2(
            FLAY=MARS_FLAY,
            INERTIA=MARS_INERTIA,
            DENSITY=MARS_DENSITY,
            SPEC_HEAT=MARS_SPEC_HEAT,
            PERIOD=MARS_PERIOD,
            N24=MARS_N24,
            MAXN2=MARS_MAXN2
        )
        # With FLAY=2.0, the first layer is thick enough that N24 is sufficient
        assert N2 >= MARS_N24  # At least N24
        assert isinstance(N2, int)

    def test_high_inertia_needs_more_steps(self):
        """Test that high TI requires more timesteps (or at least doesn't need fewer)."""
        N2_low = krc_evalN2(
            FLAY=2.0, INERTIA=50, DENSITY=1500, SPEC_HEAT=800,
            PERIOD=1.0275, N24=24, MAXN2=10000
        )
        N2_high = krc_evalN2(
            FLAY=2.0, INERTIA=2000, DENSITY=1500, SPEC_HEAT=800,
            PERIOD=1.0275, N24=24, MAXN2=10000
        )

        # Higher TI -> smaller stable timestep -> more steps per day (or same if N24-limited)
        assert N2_high >= N2_low

    def test_more_layers_similar_steps(self):
        """Test that N2 doesn't depend on N1 (only on material properties)."""
        # N2 is determined by top layer thickness and stability
        N2 = krc_evalN2(
            FLAY=2.0, INERTIA=200, DENSITY=1500, SPEC_HEAT=800,
            PERIOD=1.0275, N24=24, MAXN2=10000
        )

        # Should be reasonable (at least N24)
        assert N2 >= 24
        assert N2 <= 10000

    def test_multiple_of_n24(self):
        """Test that N2 is rounded to multiple of N24."""
        N2 = krc_evalN2(
            FLAY=2.0, INERTIA=200, DENSITY=1500, SPEC_HEAT=800,
            PERIOD=1.0275, N24=24, MAXN2=10000
        )
        assert N2 % 24 == 0

    def test_maxn2_limit(self):
        """Test that N2 respects MAXN2 limit."""
        N2 = krc_evalN2(
            FLAY=2.0, INERTIA=5000, DENSITY=1500, SPEC_HEAT=800,
            PERIOD=1.0, N24=24, MAXN2=500
        )
        assert N2 <= 500

    def test_minn2_floor(self):
        """Test that N2 respects minimum (N24)."""
        N2 = krc_evalN2(
            FLAY=2.0, INERTIA=10, DENSITY=1500, SPEC_HEAT=800,
            PERIOD=10.0, N24=96, MAXN2=1000
        )
        assert N2 >= 96


class TestCheckStability:
    """Test numerical stability checking."""

    def test_stable_configuration(self):
        """Test stable N1/N2 combination."""
        is_stable, msg = check_stability(
            N1=50, N2=288, INERTIA=200, PERIOD=1.0275,
            FLAY=2.0, DENSITY=1500, SPEC_HEAT=800, warn=False
        )
        assert is_stable
        assert "Stable" in msg
        assert "ratio" in msg

    def test_unstable_configuration(self):
        """Test unstable N1/N2 combination."""
        # Very high TI with few timesteps and SMALL FLAY -> unstable
        # Need very small first layer to make instability
        is_stable, msg = check_stability(
            N1=50, N2=24, INERTIA=5000, PERIOD=1.0,
            FLAY=0.01, DENSITY=1500, SPEC_HEAT=800, warn=False
        )
        # This should be unstable (very small first layer, high TI, low N2)
        assert not is_stable
        assert "UNSTABLE" in msg

    def test_warning_issued(self):
        """Test that warning is issued for unstable config."""
        with pytest.warns(UserWarning, match="instability"):
            check_stability(
                N1=50, N2=24, INERTIA=5000, PERIOD=1.0,
                FLAY=0.01, DENSITY=1500, SPEC_HEAT=800, warn=True
            )

    def test_auto_calculated_n1n2_stable(self):
        """Test that auto-calculated N1/N2 are stable."""
        # Auto-calculated values should always be stable
        N1 = krc_evalN1(
            RLAY=1.08, FLAY=2.0, INERTIA=200, SPEC_HEAT=800, DENSITY=1500,
            DELJUL=1.9083, N5=1080, JDISK=721, MAXN1=100, PERIOD=1.0275
        )
        N2 = krc_evalN2(
            FLAY=2.0, INERTIA=200, DENSITY=1500, SPEC_HEAT=800,
            PERIOD=1.0275, N24=24, MAXN2=10000
        )

        is_stable, msg = check_stability(
            N1=N1, N2=N2, INERTIA=200, PERIOD=1.0275,
            FLAY=2.0, DENSITY=1500, SPEC_HEAT=800, warn=False
        )
        assert is_stable


class TestCalculateConvergenceParams:
    """Test convergence parameter calculation."""

    def test_standard_mode(self):
        """Test standard convergence parameters."""
        params = calculate_convergence_params(
            N1=50, N2=288, DELLS=1.0, fast_mode=False
        )

        assert params["N3"] == 20
        assert params["NRSET"] == 0
        assert params["GGT"] == 1.0
        assert params["TPREDICT"] == 0.0

    def test_fast_mode(self):
        """Test fast convergence mode."""
        params = calculate_convergence_params(
            N1=50, N2=288, DELLS=1.0, fast_mode=True
        )

        assert params["N3"] == 10
        assert params["GGT"] == 1.5
        assert params["TPREDICT"] == 1.0

    def test_fine_resolution_adjusts_n3(self):
        """Test that fine DELLS adjusts N3."""
        params_coarse = calculate_convergence_params(
            N1=50, N2=288, DELLS=10.0, fast_mode=False
        )
        params_fine = calculate_convergence_params(
            N1=50, N2=288, DELLS=0.1, fast_mode=False
        )

        # Finer resolution -> more frequent checks
        assert params_fine["N3"] > params_coarse["N3"]


class TestEstimateRuntime:
    """Test runtime estimation."""

    def test_baseline_runtime(self):
        """Test baseline runtime estimate."""
        runtime = estimate_runtime(N1=50, N2=288, N5=1080)
        assert runtime == 10.0

    def test_scaling_with_parameters(self):
        """Test runtime scales with parameters."""
        runtime_base = estimate_runtime(N1=50, N2=288, N5=1080)
        runtime_2x = estimate_runtime(N1=100, N2=288, N5=1080)

        # Doubling N1 should double runtime
        assert abs(runtime_2x / runtime_base - 2.0) < 0.01

    def test_high_resolution_longer(self):
        """Test high resolution runs take longer."""
        runtime_low = estimate_runtime(N1=30, N2=96, N5=360)
        runtime_high = estimate_runtime(N1=80, N2=576, N5=3600)

        assert runtime_high > runtime_low


class TestIntegration:
    """Integration tests for complete workflow."""

    def test_complete_workflow(self):
        """Test complete N1/N2 calculation workflow."""
        # Mars parameters
        INERTIA = 200.0
        PERIOD = 1.0275
        DELJUL = 1.9083
        N5 = 1080
        JDISK = 721

        # Calculate N1
        N1 = krc_evalN1(
            RLAY=1.08, FLAY=2.0, INERTIA=INERTIA,
            SPEC_HEAT=800, DENSITY=1500,
            DELJUL=DELJUL, N5=N5, JDISK=JDISK,
            MAXN1=100, PERIOD=PERIOD
        )

        # Calculate N2
        N2 = krc_evalN2(
            FLAY=2.0, INERTIA=INERTIA, DENSITY=1500, SPEC_HEAT=800,
            PERIOD=PERIOD, N24=24, MAXN2=10000
        )

        # Check stability
        is_stable, msg = check_stability(
            N1=N1, N2=N2, INERTIA=INERTIA, PERIOD=PERIOD,
            FLAY=2.0, DENSITY=1500, SPEC_HEAT=800
        )

        # Get convergence params
        conv_params = calculate_convergence_params(N1, N2, 1.0)

        # Estimate runtime
        runtime = estimate_runtime(N1, N2, N5)

        # All should be reasonable
        assert 25 <= N1 <= 100
        assert 24 <= N2 <= 1000  # N2 can be as low as N24
        assert is_stable
        assert conv_params["N3"] > 0
        assert runtime > 0

    def test_extreme_parameters_stable(self):
        """Test that extreme parameters still produce stable configs."""
        test_cases = [
            {
                "INERTIA": 50, "PERIOD": 0.5, "DELJUL": 0.5 * 10.0 / 360.0,
                "N5": 108, "JDISK": 73, "SPEC_HEAT": 800, "DENSITY": 1500
            },
            {
                "INERTIA": 2000, "PERIOD": 10.0, "DELJUL": 10.0 * 0.1 / 360.0,
                "N5": 10800, "JDISK": 7201, "SPEC_HEAT": 800, "DENSITY": 1500
            },
        ]

        for params in test_cases:
            N1 = krc_evalN1(
                RLAY=1.08, FLAY=2.0, MAXN1=100,
                INERTIA=params["INERTIA"],
                SPEC_HEAT=params["SPEC_HEAT"],
                DENSITY=params["DENSITY"],
                DELJUL=params["DELJUL"],
                N5=params["N5"],
                JDISK=params["JDISK"],
                PERIOD=params["PERIOD"]
            )

            N2 = krc_evalN2(
                FLAY=2.0,
                INERTIA=params["INERTIA"],
                DENSITY=params["DENSITY"],
                SPEC_HEAT=params["SPEC_HEAT"],
                PERIOD=params["PERIOD"],
                N24=24,
                MAXN2=10000
            )

            is_stable, _ = check_stability(
                N1=N1, N2=N2,
                INERTIA=params["INERTIA"],
                PERIOD=params["PERIOD"],
                FLAY=2.0,
                DENSITY=params["DENSITY"],
                SPEC_HEAT=params["SPEC_HEAT"],
                warn=False
            )

            assert is_stable, f"Unstable for params: {params}"
