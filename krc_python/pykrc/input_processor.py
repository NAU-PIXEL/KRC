"""Parser for KRC master.inp input files."""

from pathlib import Path
from typing import Dict, Any, List
import re


class MasterInputParser:
    """Parser for KRC master.inp format files."""

    def __init__(self, filepath: str | Path):
        """
        Initialize parser with input file path.

        Parameters
        ----------
        filepath : str or Path
            Path to the master.inp file
        """
        self.filepath = Path(filepath)
        self.params: Dict[str, Any] = {}
        self.latitudes: List[float] = []
        self.elevations: List[float] = []

    def parse(self) -> Dict[str, Any]:
        """
        Parse the master.inp file.

        Returns
        -------
        dict
            Dictionary containing all parsed parameters
        """
        with open(self.filepath, "r") as f:
            lines = f.readlines()

        # Parse first line (KOLD, KEEP)
        first_line = lines[0].strip().split("/")
        values = first_line[0].strip().split()
        self.params["KOLD"] = int(values[0])
        self.params["KEEP"] = int(values[1])

        # Skip version comment line
        # Line 2 is already processed

        # Parse parameter blocks
        self._parse_param_block(lines[2:4], self._get_param_names_block1())
        self._parse_param_block(lines[4:6], self._get_param_names_block2())
        self._parse_param_block(lines[6:8], self._get_param_names_block3())
        self._parse_param_block(lines[8:10], self._get_param_names_block4())
        self._parse_param_block(lines[10:12], self._get_param_names_block5())
        self._parse_param_block(lines[12:14], self._get_param_names_block6())
        self._parse_param_block(lines[14:16], self._get_param_names_block7())
        self._parse_param_block(lines[16:18], self._get_param_names_block8())

        # Parse integer parameters
        self._parse_param_block(lines[18:20], self._get_param_names_block9(), is_int=True)
        self._parse_param_block(lines[20:22], self._get_param_names_block10(), is_int=True)
        self._parse_param_block(lines[22:24], self._get_param_names_block11(), is_int=True)

        # Parse logical flags
        self._parse_logical_flags(lines[24:28])

        # Parse latitudes and elevations
        self._parse_latitudes(lines[28:32])
        self._parse_elevations(lines[32:34])

        # Parse PORB orbital elements (6 lines after latitudes/elevations)
        # Look for the PORB header line (contains "=RUNTIME" or body name pattern)
        porb_start_idx = None
        for i, line in enumerate(lines):
            if '=RUNTIME' in line or 'IPLAN' in line:
                porb_start_idx = i
                self.params['PORB_HEADER'] = line.strip()
                break

        # Parse the 6 lines of PORB numerical data (30 values total, 5 per line)
        if porb_start_idx is not None and porb_start_idx + 6 < len(lines):
            porb_params = []
            for i in range(1, 7):  # 6 lines after header
                line = lines[porb_start_idx + i].strip()
                # Split by whitespace and convert to float
                values = line.split()
                for val_str in values:
                    try:
                        porb_params.append(float(val_str))
                    except ValueError:
                        break  # Stop if we hit non-numeric data

            if len(porb_params) == 30:
                self.params['PORB_PARAMS'] = porb_params

        return self.params

    def _parse_param_block(
        self, lines: List[str], param_names: List[str], is_int: bool = False
    ) -> None:
        """Parse a block of parameters."""
        # First line contains parameter names (header)
        # Second line contains values
        values_line = lines[1].strip()

        # Remove any comments (after /)
        if "/" in values_line:
            values_line = values_line.split("/")[0]

        # Split values
        values = values_line.split()

        for i, name in enumerate(param_names):
            if i < len(values):
                if is_int:
                    self.params[name] = int(values[i])
                else:
                    self.params[name] = float(values[i])

    def _parse_logical_flags(self, lines: List[str]) -> None:
        """Parse logical flag lines."""
        # Line 1: LP1-LP10 names
        # Line 2: LP1-LP10 values
        # Line 3: LPORB-L_ONE names
        # Line 4: LPORB-L_ONE values

        lp_flags = lines[1].strip().split()
        lporb_flags = lines[3].strip().split()

        lp_names = ["LP1", "LP2", "LP3", "LP4", "LP5", "LP6", "LPGLOB", "LVFA", "LVFT", "LKofT"]
        lporb_names = ["LPORB", "LKEY", "LSC", "LZONE", "LOCAL", "Prt76", "LPTAVE", "Prt78", "Prt79", "L_ONE"]

        for i, name in enumerate(lp_names):
            if i < len(lp_flags):
                self.params[name] = lp_flags[i] == "T"

        for i, name in enumerate(lporb_names):
            if i < len(lporb_flags):
                self.params[name] = lporb_flags[i] == "T"

    def _parse_latitudes(self, lines: List[str]) -> None:
        """Parse latitude values."""
        lat_values = []
        for line in lines[1:]:  # Skip header line
            values_str = line.strip()
            if "/" in values_str:
                values_str = values_str.split("/")[0]
            values = values_str.split()
            for v in values:
                try:
                    lat_values.append(float(v))
                except ValueError:
                    pass

        self.params["Latitudes"] = lat_values
        self.latitudes = lat_values

    def _parse_elevations(self, lines: List[str]) -> None:
        """Parse elevation values."""
        elev_values = []
        # First line may have mixed labels and values
        for line in lines:
            values_str = line.strip()
            if "/" in values_str:
                values_str = values_str.split("/")[0]
            values = values_str.split()
            for v in values:
                if v.startswith("_"):  # Skip placeholder labels
                    continue
                if v == "Elevations:" or v == "in" or v == "10F7.2":
                    continue
                try:
                    elev_values.append(float(v))
                except ValueError:
                    pass

        self.params["Elevations"] = elev_values
        self.elevations = elev_values

    @staticmethod
    def _get_param_names_block1() -> List[str]:
        return ["ALBEDO", "EMISS", "INERTIA", "COND2", "DENS2", "PERIOD", "SPEC_HEAT", "DENSITY"]

    @staticmethod
    def _get_param_names_block2() -> List[str]:
        return ["CABR", "AMW", "SatPrA", "PTOTAL", "FANON", "TATM", "TDEEP", "SpHeat2"]

    @staticmethod
    def _get_param_names_block3() -> List[str]:
        return ["TAUD", "DUSTA", "TAURAT", "TWILI", "ARC2_G0", "ARC3_Safe", "SLOPE", "SLOAZI"]

    @staticmethod
    def _get_param_names_block4() -> List[str]:
        return ["TFROST", "CFROST", "AFROST", "FEMIS", "AF1", "AF2", "FROEXT", "SatPrB"]

    @staticmethod
    def _get_param_names_block5() -> List[str]:
        return ["RLAY", "FLAY", "CONVF", "DEPTH", "DRSET", "PhotoFunc", "GGT", "DTMAX"]

    @staticmethod
    def _get_param_names_block6() -> List[str]:
        return ["DJUL", "DELJUL", "SOLARDEC", "DAU", "LsubS", "SOLCON", "GRAV", "Atm_Cp"]

    @staticmethod
    def _get_param_names_block7() -> List[str]:
        return ["ConUp0", "ConUp1", "ConUp2", "ConUp3", "ConLo0", "ConLo1", "ConLo2", "ConLo3"]

    @staticmethod
    def _get_param_names_block8() -> List[str]:
        return ["SphUp0", "SphUp1", "SphUp2", "SphUp3", "SphLo0", "SphLo1", "SphLo2", "SphLo3"]

    @staticmethod
    def _get_param_names_block9() -> List[str]:
        return ["N1", "N2", "N3", "N4", "N5", "N24", "IIB", "IC2"]

    @staticmethod
    def _get_param_names_block10() -> List[str]:
        return ["NRSET", "NMHA", "NRUN", "JDISK", "IDOWN", "FlxP14", "TUN_Flx15", "KPREF"]

    @staticmethod
    def _get_param_names_block11() -> List[str]:
        return ["K4OUT", "JBARE", "Notif", "IDISK2"]


def parse_master_inp(filepath: str | Path) -> Dict[str, Any]:
    """
    Convenience function to parse a master.inp file.

    Parameters
    ----------
    filepath : str or Path
        Path to the master.inp file

    Returns
    -------
    dict
        Dictionary containing all parsed parameters
    """
    parser = MasterInputParser(filepath)
    return parser.parse()
