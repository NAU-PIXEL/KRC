"""Tests for KRC configuration module."""

import pytest
from pathlib import Path
import os

from pykrc.config import (
    set_krc_home,
    get_krc_home,
    get_support_dir,
    get_paths,
    KRCPaths
)


def test_set_and_get_krc_home(tmp_path):
    """Test setting and getting KRC_HOME."""
    # Create a temporary KRC directory
    krc_dir = tmp_path / "krc"
    krc_dir.mkdir()

    # Set KRC_HOME
    set_krc_home(krc_dir)

    # Get KRC_HOME
    assert get_krc_home() == krc_dir


def test_set_krc_home_invalid_path():
    """Test that setting invalid path raises error."""
    with pytest.raises(FileNotFoundError):
        set_krc_home("/nonexistent/path")


def test_get_krc_home_from_env(tmp_path, monkeypatch):
    """Test getting KRC_HOME from environment variable."""
    krc_dir = tmp_path / "krc_env"
    krc_dir.mkdir()

    # Clear any existing setting
    import krc.config
    krc.config._KRC_HOME = None

    # Set environment variable
    monkeypatch.setenv("KRC_HOME", str(krc_dir))

    # Should get from environment
    assert get_krc_home() == krc_dir


def test_get_krc_home_not_set():
    """Test that error raised when KRC_HOME not set."""
    import krc.config
    krc.config._KRC_HOME = None

    # Clear environment
    if "KRC_HOME" in os.environ:
        del os.environ["KRC_HOME"]

    with pytest.raises(RuntimeError, match="KRC_HOME not set"):
        get_krc_home()


def test_get_support_dir(tmp_path):
    """Test getting support directory."""
    krc_dir = tmp_path / "krc"
    krc_dir.mkdir()
    support_dir = krc_dir / "krc_support"
    support_dir.mkdir()

    set_krc_home(krc_dir)
    assert get_support_dir() == support_dir


def test_krc_paths(tmp_path):
    """Test KRCPaths class."""
    krc_dir = tmp_path / "krc"
    krc_dir.mkdir()
    (krc_dir / "krc_support").mkdir()
    (krc_dir / "run").mkdir()
    (krc_dir / "src").mkdir()

    set_krc_home(krc_dir)
    paths = get_paths()

    assert isinstance(paths, KRCPaths)
    assert paths.support_dir == krc_dir / "krc_support"
    assert paths.run_dir == krc_dir / "run"
    assert paths.src_dir == krc_dir / "src"
    assert paths.master_inp == krc_dir / "run" / "master.inp"
