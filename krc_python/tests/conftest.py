"""
Pytest configuration for KRC test suite.

This file registers custom command-line options and hooks for:
- --tolerance: Numerical comparison tolerance
- --keep-files: Keep temporary files after tests
- --summary-report: Generate markdown summary report
"""

import pytest
import os
from typing import List, Dict, Any

# Global test results storage (properly initialized in pytest_configure)
TEST_RESULTS: List[Dict[str, Any]] = None


def pytest_runtest_logreport(report):
    """
    Hook called for each test report (setup, call, teardown).

    We collect test results here to ensure we capture them properly.
    """
    # Only process the 'call' phase (actual test execution, not setup/teardown)
    if report.when == "call":
        # Check if this is an integration test with results
        if hasattr(report, 'test_result_data'):
            TEST_RESULTS.append(report.test_result_data)


# Pytest fixtures for integration tests
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
def summary_report(request):
    """Get summary report path from command line."""
    return request.config.getoption("--summary-report", default=None)


@pytest.fixture(scope="session")
def validator(krc_home):
    """Create KRCValidator instance."""
    from pykrc.interface_validator import KRCValidator
    # davinci_krc_path is optional - davinci loads krc.dvrc automatically from ~/.dvrc
    return KRCValidator(krc_home=krc_home, davinci_krc_path=None)


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
    parser.addoption(
        "--summary-report",
        type=str,
        default=None,
        help="Path to write summary report (markdown format)"
    )


def pytest_configure(config):
    """Configure pytest with custom markers."""
    global TEST_RESULTS
    TEST_RESULTS = []  # Initialize the global list

    config.addinivalue_line(
        "markers",
        "integration: mark test as integration test (requires KRC_HOME and DAVINCI_KRC)"
    )
    config.addinivalue_line(
        "markers",
        "unit: mark test as unit test"
    )


def pytest_sessionfinish(session, exitstatus):
    """Generate summary report at end of test session."""
    summary_path = session.config.getoption("--summary-report")
    if summary_path:
        # Import here to avoid circular dependencies
        try:
            import sys

            # Get the ALREADY LOADED module from sys.modules (don't reimport!)
            if 'tests.test_integration_parity' in sys.modules:
                tip_module = sys.modules['tests.test_integration_parity']
                test_results = tip_module._TEST_RESULTS
                tip_module.generate_summary_report(summary_path, test_results=test_results)
            else:
                print("Warning: test_integration_parity not in sys.modules")
        except ImportError as e:
            print(f"Warning: Could not import generate_summary_report function: {e}")
        except Exception as e:
            print(f"Warning: Error generating summary report: {e}")
