"""
KRC Python Interface

A Python interface for the KRC (Kieffer Rapid Calculation) thermal model.

Copyright 2018-2025. All Rights reserved.
Part of this work was performed at the Jet Propulsion Laboratory,
California Institute of Technology under a contract with NASA.
Government support acknowledged.
"""

__version__ = "0.1.0"

from pykrc.core import krc
from pykrc.config import set_krc_home, get_krc_home

__all__ = ["krc", "set_krc_home", "get_krc_home"]
