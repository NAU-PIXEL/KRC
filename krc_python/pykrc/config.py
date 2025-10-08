"""Configuration module for KRC Python interface."""

import os
from pathlib import Path
from typing import Optional

_KRC_HOME: Optional[Path] = None


def set_krc_home(path: str | Path) -> None:
    """
    Set the KRC_HOME directory path.

    Parameters
    ----------
    path : str or Path
        Path to the KRC installation directory

    Raises
    ------
    FileNotFoundError
        If the path doesn't exist
    """
    global _KRC_HOME
    krc_path = Path(path).resolve()

    if not krc_path.exists():
        raise FileNotFoundError(f"KRC_HOME path does not exist: {krc_path}")

    if not krc_path.is_dir():
        raise NotADirectoryError(f"KRC_HOME is not a directory: {krc_path}")

    _KRC_HOME = krc_path


def get_krc_home() -> Path:
    """
    Get the KRC_HOME directory path.

    Returns
    -------
    Path
        Path to the KRC installation directory

    Raises
    ------
    RuntimeError
        If KRC_HOME is not set
    """
    global _KRC_HOME

    if _KRC_HOME is None:
        # Try to get from environment variable
        env_krc_home = os.environ.get("KRC_HOME")
        if env_krc_home:
            set_krc_home(env_krc_home)
        else:
            raise RuntimeError(
                "KRC_HOME not set. Use set_krc_home() or set the KRC_HOME "
                "environment variable."
            )

    return _KRC_HOME


def get_support_dir() -> Path:
    """Get the krc_support directory path."""
    # Support files are in krc_python/pykrc/data/krc_support
    krc_home = get_krc_home()
    support_dir = krc_home / "krc_python" / "pykrc" / "data" / "krc_support"
    if not support_dir.exists():
        # Fallback to old location
        support_dir = krc_home / "krc_support"
    return support_dir


def get_run_dir() -> Path:
    """Get the run directory path."""
    return get_krc_home() / "run"


def get_src_dir() -> Path:
    """Get the src directory path."""
    return get_krc_home() / "src"


def get_master_inp_path() -> Path:
    """Get the path to master.inp file."""
    return get_run_dir() / "master.inp"


def get_krc_executable() -> Path:
    """
    Get the path to the KRC executable.

    Returns
    -------
    Path
        Path to the KRC executable

    Raises
    ------
    FileNotFoundError
        If KRC executable not found
    """
    krc_home = get_krc_home()

    # Try root directory first (where it's compiled)
    krc_exe = krc_home / "krc"
    if krc_exe.exists() and krc_exe.is_file():
        return krc_exe

    # Try src directory
    krc_exe = krc_home / "src" / "krc"
    if krc_exe.exists() and krc_exe.is_file():
        return krc_exe

    raise FileNotFoundError(
        f"KRC executable not found in {krc_home} or {krc_home / 'src'}. "
        "Please compile KRC using 'make' in the root directory."
    )


class KRCPaths:
    """Container for KRC file paths."""

    def __init__(self):
        self.support_dir = get_support_dir()
        self.run_dir = get_run_dir()
        self.src_dir = get_src_dir()

    @property
    def inertia_map(self) -> Path:
        return self.support_dir / "ti_map2ppd_v4.vicar"

    @property
    def albedo_map(self) -> Path:
        return self.support_dir / "albedo_2ppd.vicar"

    @property
    def elevation_map(self) -> Path:
        return self.support_dir / "mola_2ppd.vicar"

    @property
    def standish_table(self) -> Path:
        return self.support_dir / "standish.tab"

    @property
    def spinaxis_table(self) -> Path:
        return self.support_dir / "spinaxis.tab"

    @property
    def planetary_params(self) -> Path:
        return self.support_dir / "planetary_params3.csv"

    @property
    def small_bodies(self) -> Path:
        return self.support_dir / "small_bodies.hdf"

    @property
    def comets(self) -> Path:
        return self.support_dir / "comets.hdf"

    @property
    def porb_master(self) -> Path:
        return self.support_dir / "porb_master.hdf"

    @property
    def var_header(self) -> Path:
        return self.support_dir / "var_header.ascii"

    @property
    def master_inp(self) -> Path:
        return self.run_dir / "master.inp"

    @property
    def fake_krc_input(self) -> Path:
        return self.support_dir / "fake_krc344"

    @property
    def porbcm_mat(self) -> Path:
        """Path to PORBCM.mat binary orbital data file."""
        return self.support_dir / "PORBCM.mat"

    @property
    def krc_executable(self) -> Path:
        """Path to KRC executable."""
        return get_krc_executable()


def get_paths() -> KRCPaths:
    """Get a KRCPaths instance with all standard file paths."""
    return KRCPaths()
