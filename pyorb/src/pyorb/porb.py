# get rotation matrix for KRC input file.
# intended to replace PORB fortran stuff, specifically porbig.f
# returns BFRM, in the PORB/KRC nomenclature.

import numpy as np
import spiceypy as spice
import datetime
from . import constants as const
from .kernel_mgmt import get_naifid, get_body_type, get_mk
from . import defaults 
from . import install
from typing import Self

class OrbParams:
    def __init__(self, 
                 long_of_asc_node: float, 
                 eccentricity: float, 
                 inclination: float, 
                 arg_of_peri: float, 
                 mean_anomaly: float, 
                 semimajor_axis: float, 
                 epoch_JD: float, 
                 orbit_period: float, 
                 perihelion_date: float, 
                 centuries_from_j2000: float):
        self.long_of_asc_node = long_of_asc_node
        self.eccentricity = eccentricity
        self.inclination = inclination
        self.arg_of_peri = arg_of_peri
        self.mean_anomaly = mean_anomaly
        self.semimajor_axis = semimajor_axis
        self.epoch_JD = epoch_JD
        self.orbit_period = orbit_period
        self.perihelion_date = perihelion_date
        self.centuries_from_j2000 = centuries_from_j2000

    def __eq__(self, other: "OrbParams") -> bool:
        is_equal = all([
            self.long_of_asc_node       == other.long_of_asc_node,
            self.eccentricity           == other.eccentricity,
            self.inclination            == other.inclination,
            self.arg_of_peri            == other.arg_of_peri,
            np.isclose(self.mean_anomaly, other.mean_anomaly),
            np.isclose(self.semimajor_axis, other.semimajor_axis),
            np.isclose(self.epoch_JD, other.epoch_JD),
            np.isclose(self.orbit_period, other.orbit_period),
            np.isclose(self.perihelion_date, other.perihelion_date),
            np.isclose(self.centuries_from_j2000, other.centuries_from_j2000)
        ])

        return is_equal
    
    # def __str__(self) -> str:
    #     string = ''
    #     string += f'long_of_asc_node: {self.long_of_asc_node}\n'
    #     string += f'eccentricity: {self.eccentricity}\n'
    #     string += f'inclination: {self.inclination}\n'
    #     string += f'arg_of_peri: {self.arg_of_peri}\n'
    #     string += f'mean_anomaly: {self.mean_anomaly }\n'
    #     string += f'semimajor_axis: {self.semimajor_axis}\n'
    #     string += f'epoch_JD: {self.epoch_JD}\n'
    #     string += f'orbit_period: {self.orbit_period}\n'
    #     string += f'perihelion_date: {self.perihelion_date}\n'
    #     string += f'centuries_from_j2000: {self.centuries_from_j2000}\n'
        
    #     return string

    @classmethod
    def from_elems(cls, 
            orb_elems:tuple[float, float, float, float, float, float, float]) -> Self:
        """
        Constructs an OrbParams object from an orb_elems tuple, as would be output 
        by get_orbital_elements(). Secondary orbital parameters are calculated based on 
        the contents of the orb_elems tuple.

        Args:
            orb_elems (tuple[float, float, float, float, float, float, float]): Tuple of
                orbital elements, usually from get_orbital_elements(). Contains:
                    long_of_asc_node:   longitude of the ascending node [radians]
                    eccentricity:       eccentricity [unitless]
                    inclination:        inclination [radians]
                    arg_of_peri:        argument of perihelion [radians] 
                    mean_anomaly:       mean anomaly at epoch [radians]
                    semimajor_axis:     semimajor axis [km]
                    epoch_JD:           Julian date of epoch [Julian Date]

        Returns:
            Self: An OrbParams object.
        """
        (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems
        (orbit_period, perihelion_date, centuries_from_j2000) = get_secondary_orb_params(orb_elems)
        return cls(long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD, orbit_period, perihelion_date, centuries_from_j2000)
    
    @classmethod
    def from_elems_and_second_params(cls, 
            orb_elems:tuple[float, float, float, float, float, float, float], 
            orb_second_params:tuple[float, float, float]) -> Self:
        """
        Constructs an OrbParams object from an orb_elems tuple, as would be output 
        by get_orbital_elements(), and an orb_second_params tuple, as would be output
        by get_secondary_orb_params().

        Args:
            orb_elems (tuple[float, float, float, float, float, float, float]): 
                Tuple of orbital elements, usually from get_orbital_elements(). Contains:
                    long_of_asc_node:   longitude of the ascending node [radians]
                    eccentricity:       eccentricity [unitless]
                    inclination:        inclination [radians]
                    arg_of_peri:        argument of perihelion [radians] 
                    mean_anomaly:       mean anomaly at epoch [radians]
                    semimajor_axis:     semimajor axis [km]
                    epoch_JD:           Julian date of epoch [Julian Date]
            orb_second_params (tuple[float, float, float]): Tuple of derived orbital 
                parameters, usually from get_secondary_orb_params(). Contains:
                    orbit_period:           Period of the orbit in Earth days
                    perihelion_date:        J2000 date (days past J2000 epoch) of previous perihelion passage
                    centuries_from_j2000:   Time of reference epoch from j2000 epoch, in centuries 

        Returns:
            Self: An OrbParams object.
        """
        (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems
        (orbit_period, perihelion_date, centuries_from_j2000) = orb_second_params
        return cls(long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD, orbit_period, perihelion_date, centuries_from_j2000)

    @classmethod
    def from_porb_params(cls, porb_params:"PorbParams") -> Self:
        """
        Constructs an OrbParams object from a PorbParams object.

        Args:
            porb_params (PorbParams): A PorbParams object.

        Returns:
            Self: An OrbParams object.
        """
        long_of_asc_node    = porb_params.RODE
        eccentricity        = porb_params.XECC
        inclination         = porb_params.CLIN
        arg_of_peri         = porb_params.ARGP 
        semimajor_axis      = porb_params.SJA
        orbit_period        = porb_params.OPERIOD
        perihelion_date     = porb_params.TJP
        centuries_from_j2000 = porb_params.TC
        
        epoch_JD = centuries_from_j2000 * const.earth_year*100 + const.j2000_JD
        mean_anomaly = ((perihelion_date - epoch_JD + const.j2000_JD) / orbit_period) * (-2*np.pi)

        return cls(long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD, orbit_period, perihelion_date, centuries_from_j2000)
    
    @classmethod
    def from_modified_params(cls, porb_params:"PorbParams",
                long_of_asc_node:float|None = None,
                eccentricity:float|None = None,
                inclination:float|None = None,
                arg_of_peri:float|None = None,
                semimajor_axis:float|None = None,
                orbit_period:float|None = None,
                perihelion_date:float|None = None,
                centuries_from_j2000:float|None = None,
                epoch_JD:float|None = None,
                mean_anomaly:float|None = None) -> Self:
        """
        Constructs an OrbParams object using a PorbParams object, plus optional inputs
        with which to replace each parameter. The function ensures conflicting parameters
        are not set at the same time, and correctly derives dependent parameters based on
        the input.

        Args:
            porb_params (PorbParams): PorbParams object to modify.
            long_of_asc_node (float | None, optional): longitude of the ascending node [radians]. 
                Defaults to None.
            eccentricity (float | None, optional): eccentricity [unitless]. 
                Defaults to None.
            inclination (float | None, optional): inclination [radians]. 
                Defaults to None.
            arg_of_peri (float | None, optional): argument of perihelion [radians]. 
                Defaults to None.
            semimajor_axis (float | None, optional): semimajor axis [km]. 
                Defaults to None.
            orbit_period (float | None, optional): Period of the orbit in Earth days. 
                Defaults to None.
            perihelion_date (float | None, optional): J2000 date (days past J2000 epoch) 
                of previous perihelion passage. Defaults to None.
            centuries_from_j2000 (float | None, optional): Time of reference epoch from 
                J2000 epoch, in centuries. Defaults to None.
            epoch_JD (float | None, optional): Julian date of epoch [Julian Date]. 
                Defaults to None.
            mean_anomaly (float | None, optional): mean anomaly at epoch [radians]. 
                Defaults to None.      

        Raises:
            ValueError: Raised when conflicting arguments are both specified (semimajor_axis and orbit_period)
            ValueError: Raised when conflicting arguments are both specified (epoch_JD and centuries_from_j2000)
            ValueError: Raised when conflicting arguments are both specified (perihelion_date and mean_anomaly)

        Returns:
            Self: An OrbParams object containing the modified values.
        """
        
        rode    = porb_params.RODE
        xecc    = porb_params.XECC
        clin    = porb_params.CLIN
        argp    = porb_params.ARGP 
        sja     = porb_params.SJA
        operiod = porb_params.OPERIOD
        tjp     = porb_params.TJP
        tc      = porb_params.TC

        if (semimajor_axis is not None) and (orbit_period is not None):
            raise ValueError("Only one of semimajor_axis and orbit_period may be specified.")
        if semimajor_axis is not None:
            orbit_period = semimajor_axis**(1.5) * const.earth_year    
        elif orbit_period is not None:
            semimajor_axis = (orbit_period / const.earth_year)**(2./3.)
        else:
            semimajor_axis = sja
            orbit_period = operiod

        if (epoch_JD is not None) and (centuries_from_j2000 is not None):
            raise ValueError("Only one of epoch_JD and centuries_from_j2000 may be specified.")
        if epoch_JD is not None:
            centuries_from_j2000 = (epoch_JD - const.j2000_JD) / (const.earth_year*100)
        elif centuries_from_j2000 is not None:
            epoch_JD = centuries_from_j2000 * const.earth_year*100 + const.j2000_JD
        else:
            centuries_from_j2000 = tc
            epoch_JD = centuries_from_j2000 * const.earth_year*100 + const.j2000_JD

        if (perihelion_date is not None) and (mean_anomaly is not None):
            raise ValueError("Only one of perihelion_date and mean_anomaly may be specified.")
        if perihelion_date is not None:
            mean_anomaly = ((perihelion_date - epoch_JD + const.j2000_JD) / orbit_period) * (-2*np.pi)
        elif mean_anomaly is not None:
            perihelion_date = epoch_JD - (mean_anomaly/(2*np.pi))*orbit_period - const.j2000_JD
        else:
            perihelion_date = tjp
            mean_anomaly = ((perihelion_date - epoch_JD + const.j2000_JD) / orbit_period) * (-2*np.pi)

        if long_of_asc_node is None:
            long_of_asc_node = rode
        if eccentricity is None:
            eccentricity = xecc
        if inclination is None:
            inclination = clin
        if arg_of_peri is None:
            arg_of_peri = argp

        return cls(long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD, orbit_period, perihelion_date, centuries_from_j2000)

class SpinParams:
    def __init__(self,
                 rotation_period: float, 
                 phase_at_j2000: float, 
                 pole_ra: float, 
                 pole_dec: float, 
                 default_spin_flag: int,
                 obliquity: float, 
                 rotation_matrix_FtoB: np.ndarray, 
                 true_anomaly_at_vernal_equinox: float):
        self.rotation_period = rotation_period
        self.phase_at_j2000 = phase_at_j2000
        self.pole_ra = pole_ra
        self.pole_dec = pole_dec
        self.default_spin_flag = default_spin_flag
        self.obliquity = obliquity
        self.rotation_matrix_FtoB = rotation_matrix_FtoB
        self.true_anomaly_at_vernal_equinox = true_anomaly_at_vernal_equinox

    def __eq__(self, other:"SpinParams") -> bool:
        is_equal = all([
            self.rotation_period == other.rotation_period,
            self.phase_at_j2000 == other.phase_at_j2000,
            np.isclose(self.pole_ra, other.pole_ra),
            np.isclose(self.pole_dec, other.pole_dec),
            self.default_spin_flag == other.default_spin_flag,
            np.isclose(self.obliquity, other.obliquity),
            np.all(np.isclose(self.rotation_matrix_FtoB, other.rotation_matrix_FtoB)),
            np.isclose(self.true_anomaly_at_vernal_equinox, other.true_anomaly_at_vernal_equinox)
        ])

        return is_equal
    
    def __str__(self) -> str:
        """
        Produces a string representation of the SpinParams object. Used for testing only.

        Returns:
            str: String representation of the SpinParams object.
        """
        string = ''
        string += f'rotation_period: {self.rotation_period}\n'
        string += f'phase_at_j2000: {self.phase_at_j2000}\n'
        string += f'pole_ra: {self.pole_ra}\n'
        string += f'pole_dec: {self.pole_dec}\n'
        string += f'default_spin_flag: {self.default_spin_flag}\n'
        string += f'obliquity: {self.obliquity}\n'
        string += f'rotation_matrix_FtoB: \n{self.rotation_matrix_FtoB}\n'
        string += f'true_anomaly_at_vernal_equinox: {self.true_anomaly_at_vernal_equinox}\n'
        return string

    @classmethod
    def from_spin_axis(cls, 
            spin_axis:tuple[float, float, float, float, int], 
            orb:OrbParams) -> Self:
        """
        Constructs a SpinParams object from spin_axis and an OrbParams object 
        as would be output by get_spin_axis() and some OrbParams constructor.

        Args:
            spin_axis (tuple[float, float, float, float, int]): Tuple containing spin
                axis information, as would be produced by get_spin_axis(). Contains:
                    rotation_period:    rotation period [hours]
                    phase_at_j2000:     rotational phase (angle of prime meridian) at J2000 epoch [degrees]
                    pole_ra:            right ascension of spin axis in J2000 frame [radians]
                    pole_dec:           declination of spin axis in J2000 frame [radians]
                    default_spin_flag:  0: rotation period and pole orientation are both real.
                                        1: rotation period and pole orientation are both default.
                                        2: rotation period is real, pole orientation is default. 
                                            (not implemented, but I imagine this could be done by 
                                            searching the small body lightcurve database) 
            orb (OrbParams): An OrbParams object containing orbital elements.

        Returns:
            Self: A SpinParams object, containing spin axis orientation and rate information.
        """
        (rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag) = spin_axis 
        (obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox) = get_secondary_spin_params(orb, pole_ra, pole_dec) 
        return cls(rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag, obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox)
    
    @classmethod
    def from_porb_params(cls, porb_params:"PorbParams") -> Self:
        """
        Generates a SpinParams object by extracting the relevant parameters from a PorbParams object.

        Args:
            porb_params (PorbParams): A PorbParams object containing orbital elements and 
                spin axis information.

        Returns:
            Self: A SpinParams object, containing spin axis orientation and rate information.
        """
        rotation_period         = (360.*24.)/porb_params.WDOT
        phase_at_j2000          = porb_params.WO
        pole_ra                 = porb_params.ZBAB
        pole_dec                = porb_params.ZBAA
        default_spin_flag       = porb_params.default_spin
        obliquity               = porb_params.BLIP
        rotation_matrix_FtoB    = porb_params.BFRM
        true_anomaly_at_vernal_equinox = porb_params.TAV
        return cls(rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag, obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox)
    
    def set_obliq_and_true_anomaly(self, 
            obliquity:float, 
            true_anomaly_at_vernal_equinox:float, 
            orb:OrbParams) -> Self:
        """
        Updates the obliquity and true anomaly at vernal equinox, accounting for
        the impacts on pole orientation and rotation matrix.

        Args:
            obliquity (float): Angle between the object's spin axis and the north pole of 
                its orbit. [radians]
            true_anomaly_at_vernal_equinox (float): True anomaly at vernal equinox (prograde 
                angle between perihelion vector and vernal equinox vector) [radians]
            orb (OrbParams): An OrbParams object, containing orbital elements.

        Returns:
            Self: A SpinParams object, containing spin axis orientation and rate information.
        """
        self.obliquity = obliquity
        self.true_anomaly_at_vernal_equinox = true_anomaly_at_vernal_equinox
        self.pole_ra, self.pole_dec, self.rotation_matrix_FtoB = alt_get_secondary_spin_params(orb, obliquity, true_anomaly_at_vernal_equinox)
        return self
    
    @classmethod
    def from_modified_params(cls, porb_params:"PorbParams", orb:OrbParams,
            rotation_period:float|None = None,
            phase_at_j2000:float|None = None,
            pole_ra:float|None = None,
            pole_dec:float|None = None,
            default_spin_flag:int|None = None,
            obliquity:float|None = None,
            rotation_matrix_FtoB:np.ndarray|None = None,
            true_anomaly_at_vernal_equinox:float|None = None) -> Self:
        """
        Creates a SpinParams object, containing spin axis orientation and rate information,
        based on existing PorbParams and OrbParams objects, plus a set of optional parameters
        which may be directly modified. The system will correctly reject sets of modified 
        parameters when one of a pair of linked parameters are missing, or when two 
        conflicting parameters are both set at the same time. The system will use the 
        provided parameters to calculate any derived parameters to complete the contents
        of the SpinParams object.

        Args:
            porb_params (PorbParams): A PorbParams object, containing orbital elements and
                spin axis information.
            orb (OrbParams): An OrbParams object, containing orbital elements.
            
            rotation_period (float | None, optional): rotation period [hours]. 
                Defaults to None.
            phase_at_j2000 (float | None, optional): rotational phase (angle of prime 
                meridian) at J2000 epoch [degrees]. Defaults to None.
            pole_ra (float | None, optional): right ascension of spin axis in J2000 
                frame [radians]. Defaults to None.
            pole_dec (float | None, optional): declination of spin axis in J2000 
                frame [radians]. Defaults to None.
            default_spin_flag (int | None, optional): Flag indicating if the spin parameters 
                really describe the object's spin state, or are default placeholder values.
                Possible values listed below. Defaults to None.
                    0: rotation period and pole orientation are both real.
                    1: rotation period and pole orientation are both default.
                    2: rotation period is real, pole orientation is default. 
                        (not implemented, but I imagine this could be done by 
                        searching the small body lightcurve database). 
            obliquity (float | None, optional): angle between spin axis and orbit 
                pole [radians]. Defaults to None.
            rotation_matrix_FtoB (np.ndarray | None, optional): rotation matrix from 
                orbital frame (F) to seasonal frame (B) [3x3 matrix]. Defaults to None.
            true_anomaly_at_vernal_equinox (float | None, optional): 
                True anomaly at vernal equinox [radians]. Defaults to None.

        Raises:
            ValueError: Raised when one of two paired parameters (pole_ra and pole_dec) 
                is set without the other.  
            ValueError: Raised when one of two paired parameters (obliquity and 
                true_anomaly_at_vernal_equinox) is set without the other.
            ValueError: Raised when conflicting sets of parameters ([Spin pole RA and Dec] 
                and [Obliquity and True Anomaly]) are set at the same time.
            NotImplementedError: Raised when rotation_matrix_FtoB is input as a parameter 
                to be modified directly. This is not currently implemented.

        Returns:
            Self: A Spin Params object, containing spin axis orientation and rate.
        """

        default_spin = porb_params.default_spin

        if all([v is None for v in [rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag, obliquity, true_anomaly_at_vernal_equinox, rotation_matrix_FtoB]]):
            default_spin_flag = default_spin
        else:
            default_spin_flag = 0

        if rotation_period is None:
            rotation_period = (360.*24.)/porb_params.WDOT
        if phase_at_j2000 is None:
            phase_at_j2000  = porb_params.WO
        
        if not (pole_ra is None) == (pole_dec is None):
            raise ValueError("pole_ra and pole_dec must be set together or not at all.")
        if not (obliquity is None) == (true_anomaly_at_vernal_equinox is None):
            raise ValueError("obliquity and true_anomaly_at_vernal_equinox must be set together or not at all.")
        if (pole_ra is not None) and (obliquity is not None):
            raise ValueError("Spin pole RA and Dec cannot be set at the same time as Obliquity and True Anomaly.")
        if rotation_matrix_FtoB is not None:
            raise NotImplementedError("Directly specifying rotation_matrix_FtoB is not currently implemented.")
        
        if pole_ra is not None:
            (obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox) = get_secondary_spin_params(orb, pole_ra, pole_dec)
        elif obliquity is not None:
            (pole_ra, pole_dec, rotation_matrix_FtoB) = alt_get_secondary_spin_params(orb, obliquity, true_anomaly_at_vernal_equinox)
        else:
            pole_ra     = porb_params.ZBAB
            pole_dec    = porb_params.ZBAA
            obliquity   = porb_params.BLIP
            true_anomaly_at_vernal_equinox = porb_params.TAV
            rotation_matrix_FtoB = porb_params.BFRM
        
        return cls(rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag, obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox)

class PorbParams:
    def __init__(self,
                 default_spin: int,
                 porb_version: str,
                 generation_date: str,
                 NAME: str,
                 body_type: str,
                 PLANUM: int,
                 TC: float,
                 RODE: float,
                 CLIN: float,
                 ARGP: float,
                 XECC: float,
                 SJA: float,
                 EOBL: float,
                 SFLAG: int,
                 ZBAA: float,
                 ZBAB: float,
                 WDOT: float,
                 WO: float,
                 OPERIOD: float,
                 TJP: float,
                 SIDAY: float,
                 spar17: int,
                 TAV: float,
                 BLIP: float,
                 PBUG: int,
                 spar21: int,
                 BFRM: np.ndarray,
                 ):  
        self.default_spin     = default_spin
        self.porb_version     = porb_version
        self.generation_date  = generation_date
        self.NAME             = NAME
        self.body_type        = body_type

        self.PLANUM           = PLANUM
        self.TC               = TC
        self.RODE             = RODE
        self.CLIN             = CLIN
        self.ARGP             = ARGP

        self.XECC             = XECC
        self.SJA              = SJA 
        self.EOBL             = EOBL
        self.SFLAG            = SFLAG 
        self.ZBAA             = ZBAA

        self.ZBAB             = ZBAB
        self.WDOT             = WDOT
        self.WO               = WO
        self.OPERIOD          = OPERIOD
        self.TJP              = TJP

        self.SIDAY            = SIDAY
        self.spar17           = spar17
        self.TAV              = TAV
        self.BLIP             = BLIP
        self.PBUG             = PBUG

        self.spar21           = spar21
        self.BFRM             = BFRM

    def __eq__(self, other:"PorbParams") -> bool:
        is_equal = all([
            self.default_spin     == other.default_spin,
            self.porb_version     == other.porb_version,
            self.NAME             == other.NAME,
            self.body_type        == other.body_type,

            self.PLANUM           == other.PLANUM,
            self.TC               == other.TC,
            self.RODE             == other.RODE,
            self.CLIN             == other.CLIN,
            self.ARGP             == other.ARGP,

            self.XECC             == other.XECC,
            self.SJA              == other.SJA,
            self.EOBL             == other.EOBL,
            self.SFLAG            == other.SFLAG,
            self.ZBAA             == other.ZBAA,

            self.ZBAB             == other.ZBAB,
            self.WDOT             == other.WDOT,
            self.WO               == other.WO,
            self.OPERIOD          == other.OPERIOD,
            self.TJP              == other.TJP,

            self.SIDAY            == other.SIDAY,
            self.spar17           == other.spar17,
            self.TAV              == other.TAV,
            self.BLIP             == other.BLIP,
            self.PBUG             == other.PBUG,

            self.spar21           == other.spar21,
            np.all(self.BFRM      == other.BFRM)
        ])

        return is_equal
        

    @classmethod
    def from_orb_and_spin_params(cls,
                                 body_name: str, 
                                 body_type: str, 
                                 body_naifid: int, 
                                 orb: OrbParams, 
                                 spin: "SpinParams") -> Self:
        """
        Constructs a PorbParams object for some specified body, from OrbParams and 
        SpinParams objects for that body.

        Args:
            body_name (str): String identifier for the object of interest.
            body_type (str): Body type for the object of interest. Can be "Planet", 
                "Satellite", "Comet", or "Minor".
            body_naifid (int): NAIF object ID code for the object of interest.
            orb (OrbParams): OrbParams object for the specified body, containing orbital 
                elements and derived orbital parameters.
            spin (SpinParams): SpinParams object for the specified body, containing spin
                axis orientation and rate information. 

        Returns:
            Self: A PorbParams object, containing orbital elements and spin axis information.
        """    
        default_spin     = spin.default_spin_flag
        porb_version     = const.porb_version
        generation_date  = datetime.datetime.now().strftime('%Y %b %d %H:%M:%S')
        NAME             = body_name
        body_type        = body_type

        PLANUM           = body_naifid    
        if   PLANUM  >= 20000000:
             PLANUM  -= 20000000
        elif PLANUM  >=  2000000:
             PLANUM  -=  2000000

        TC               = orb.centuries_from_j2000
        RODE             = orb.long_of_asc_node
        CLIN             = orb.inclination
        ARGP             = orb.arg_of_peri

        XECC             = orb.eccentricity
        SJA              = orb.semimajor_axis
        EOBL             = const.earth_obliquity
        SFLAG            = const.sflag
        ZBAA             = spin.pole_dec

        ZBAB             = spin.pole_ra
        WDOT             = (360.*24)/spin.rotation_period
        WO               = spin.phase_at_j2000
        OPERIOD          = orb.orbit_period
        TJP              = orb.perihelion_date

        SIDAY            = spin.rotation_period
        spar17           = const.spar17
        TAV              = spin.true_anomaly_at_vernal_equinox
        BLIP             = spin.obliquity
        PBUG             = const.pbug

        spar21           = const.spar21
        BFRM             = spin.rotation_matrix_FtoB

        return cls(default_spin, porb_version, generation_date, NAME, body_type, PLANUM, TC, RODE, CLIN, ARGP, XECC, SJA, EOBL, SFLAG, ZBAA, ZBAB, WDOT, WO, OPERIOD, TJP, SIDAY, spar17, TAV, BLIP, PBUG, spar21, BFRM)

    def __str__(self) -> str:
        """
        Produces a string representation of the PorbParams object, formatted as a 
        Fortran-style multiline PORB output string. 

        Returns:
            str: A Fortran-style multiline PORB output string containing the data from
                PorbParams object.
        """
        flat_bfrm = self.BFRM.T.flatten()

        out_str = ''
        out_str += f"PORB:{self.porb_version} {self.generation_date} IPLAN,TC= {self.PLANUM:5.4g} {self.TC:7.5g} {self.NAME}:{self.NAME}\n"
        out_str += f" {self.PLANUM:10.7g}     {self.TC:10.7g}     {self.RODE:10.7g}      {self.CLIN:.7E} {self.ARGP:10.7f}\n"
        out_str += f"  {self.XECC:.7E} {self.SJA:10.7g}     {self.EOBL:10.7g}     {self.SFLAG:10.7g}     {self.ZBAA:10.7g}\n"
        out_str += f" {self.ZBAB:10.7g}     {self.WDOT:10.7g}     {self.WO:10.7g}     {self.OPERIOD:10.7g}     {self.TJP:10.7g}\n"
        out_str += f" {self.SIDAY:10.7g}     {self.spar17:10.7g}     {self.TAV:10.7g}     {self.BLIP:10.7g}     {self.PBUG:10.7g}\n"
        out_str += f" {self.spar21:10.7g}     {flat_bfrm[0]:10.7f}     {flat_bfrm[1]:10.7f}     {flat_bfrm[2]:10.7f}     {flat_bfrm[3]:10.7f}\n"
        out_str += f" {flat_bfrm[4]:10.7f}     {flat_bfrm[5]:10.7f}     {flat_bfrm[6]:10.7f}     {flat_bfrm[7]:10.7f}     {flat_bfrm[8]:10.7f}\n"

        return out_str
    
    def verbose_output(self) -> str:
        """
        Produces a string representation of the PorbParams object, formatted as a 
        verbose Fortran-style multiline PORB output string, including variable labels. 

        Returns:
            str: A verbose Fortran-style multiline PORB output string containing the data from
                PorbParams object, including variable labels.
        """
        flat_bfrm = self.BFRM.T.flatten()

        out_str = ''
        out_str += f"<--VERSION---> <--generation date->           IPLAN      TC orbit:pole\n"
        out_str += f"PORB:{self.porb_version} {self.generation_date} IPLAN,TC= {self.PLANUM:5.4g} {self.TC:7.5g} {self.NAME}:{self.NAME}\n"
        out_str += f"     PLANUM             Tc           RODE           CLIN           ARGP\n"
        out_str += f" {self.PLANUM:10.7g}     {self.TC:10.7g}     {self.RODE:10.7g}      {self.CLIN:.7E} {self.ARGP:10.7f}\n"
        out_str += f"       XECC            SJA           EOBL          SFLAG           ZBAA\n"
        out_str += f"  {self.XECC:.7E} {self.SJA:10.7g}     {self.EOBL:10.7g}     {self.SFLAG:10.7g}     {self.ZBAA:10.7g}\n"
        out_str += f"       ZBAB           WDOT             WO        OPERIOD            TJP\n"
        out_str += f" {self.ZBAB:10.7g}     {self.WDOT:10.7g}     {self.WO:10.7g}     {self.OPERIOD:10.7g}     {self.TJP:10.7g}\n"
        out_str += f"      SIDAY          spare            TAV           BLIP           PBUG\n"
        out_str += f" {self.SIDAY:10.7g}     {self.spar17:10.7g}     {self.TAV:10.7g}     {self.BLIP:10.7g}     {self.PBUG:10.7g}\n"
        out_str += f"      spare         BFRM 1              2              3              4\n"
        out_str += f" {self.spar21:10.7g}     {flat_bfrm[0]:10.7f}     {flat_bfrm[1]:10.7f}     {flat_bfrm[2]:10.7f}     {flat_bfrm[3]:10.7f}\n"
        out_str += f"          5              6              7              8         BFRM 9\n"
        out_str += f" {flat_bfrm[4]:10.7f}     {flat_bfrm[5]:10.7f}     {flat_bfrm[6]:10.7f}     {flat_bfrm[7]:10.7f}     {flat_bfrm[8]:10.7f}\n"

        return out_str
    
    @classmethod
    def from_str(cls, porb_str: str) -> Self:
        """
        Constructs a PorbParams object from a Fortran-style multiline PORB output string.

        Args:
            porb_str (str): A Fortran-style multiline PORB output string containing the 
                data to load into a PorbParams object.

        Returns:
            Self: A PorbParams object, containing orbital elements and spin axis information.
        """
        flat_bfrm = np.zeros(9)

        lines = porb_str.split('\n')
        porb_version = lines[0].split(' ')[0][5:]
        generation_date = ' '.join(lines[0].split('IPLAN')[0].split()[1:])
        NAME = lines[0].split(':')[-1]
        
        [PLANUM, TC, RODE, CLIN, ARGP] = [float(i) for i in lines[1].split()]
        [XECC, SJA, EOBL, SFLAG, ZBAA] = [float(i) for i in lines[2].split()]
        [ZBAB, WDOT, WO, OPERIOD, TJP] = [float(i) for i in lines[3].split()]
        [SIDAY, spar17, TAV, BLIP, PBUG] = [float(i) for i in lines[4].split()]
        [spar21, flat_bfrm[0], flat_bfrm[1], flat_bfrm[2], flat_bfrm[3]] = [float(i) for i in lines[5].split()]
        [flat_bfrm[4], flat_bfrm[5], flat_bfrm[6], flat_bfrm[7], flat_bfrm[8]] = [float(i) for i in lines[6].split()]

        BFRM = flat_bfrm.reshape(3,3).T

        return cls(-1, porb_version, generation_date, NAME, 'unknown', int(PLANUM), TC, RODE, CLIN, ARGP, XECC, SJA, EOBL, int(SFLAG), ZBAA, ZBAB, WDOT, WO, OPERIOD, TJP, SIDAY, int(spar17), TAV, BLIP, int(PBUG), int(spar21), BFRM)


def get_orbital_naifid(metakernel:str, body_naifid:int, epoch_date:datetime.datetime) -> int:
    """
    Use spice to determine if the specified body orbits the sun. If it does, return the 
    body's naifid, and if not, return the naifid of whatever parent body does orbit the 
    sun.

    For KRC purposes, we only care about the orbit of the parent body (or, more 
    precisely, the system barycenter).

    This should work for binary asteroids and other multiple-body systems, as the NAIFid 
    of each system member will return the system barycenter as its orbital center, and 
    the system barycenter will return itself, as its center is the Solar System 
    Barycenter (id=0).

    Does not work when specifying planet barycenters (i.e., '400' or '4' for Mars).

    Args:
        metakernel (str): Metakernel for the specified body.
        body_naifid (int): NAIF object ID code for the object of interest.
        epoch_date (datetime.datetime): epoch at which to calculate orbital params 
            (must be covered by available kernels)

    Returns:
        int: The NAIF id to be used in calculating the specified body's orbit around the sun.
    """
    spice.furnsh(metakernel)
    et = spice.datetime2et(epoch_date)

    handle, descr, ident = spice.spksfs(body_naifid, et, 40) 
    dc, ic = spice.dafus(descr, 2, 6)
    center_id = ic[1]

    if center_id in (0, 10):
        # body orbits sun
        orbital_id = body_naifid
    else:
        # body does not orbit the sun
        orbital_id = center_id

    return orbital_id

def get_orbital_elements(
        metakernel:str, 
        orbital_naifid:int, 
        parent:str, 
        epoch_date:datetime.datetime) -> tuple[float, float, float, float, float, float, float]:
    """
    Use spice to get the keplerian(?) orbital elements for a specified body.

    Args:
        metakernel (str): Metakernel for the specified body.
        orbital_naifid (int): NAIF object ID code for object of interest, or (in the case 
            of satellites) the sun-orbiting barycenter of its host planetary system.
        parent (str): String identifying the parent object around which the object of 
            interest orbits. (Currently only ever called with "SUN")
        epoch_date (datetime.datetime): epoch at which to calculate orbital params 
            (must be covered by available kernels)

    Returns:
        tuple[float, float, float, float, float, float, float]: 
            Tuple of orbital elements, usually from get_orbital_elements(). Contains:
                    long_of_asc_node:   longitude of the ascending node [radians]
                    eccentricity:       eccentricity [unitless]
                    inclination:        inclination [radians]
                    arg_of_peri:        argument of perihelion [radians] 
                    mean_anomaly:       mean anomaly at epoch [radians]
                    semimajor_axis:     semimajor axis [km]
                    epoch_JD:           Julian date of epoch [Julian Date]
    """
    spice.furnsh(metakernel)

    et = spice.datetime2et(epoch_date)
    epoch_JD = float(spice.et2utc(et,'J', 6).split(' ')[-1])

    # get the state vector of body (or its system barycenter) relative to some specified parent body.
    state_vector = spice.spkezr(str(orbital_naifid), et, 'ECLIPJ2000', 'NONE', parent)[0]

    # get orbital elements of body relative to sun
    elts = spice.oscelt(state_vector, et, const.mu)

    long_of_asc_node    = elts[3]
    eccentricity        = elts[1]
    inclination         = elts[2]
    arg_of_peri         = elts[4]
    mean_anomaly        = elts[5]
    semimajor_axis      = elts[0] / (1-eccentricity) / const.km_per_au

    return (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD)

def get_spin_axis(metakernel:str, body_naifid:int) -> tuple[float, float, float, float, int]:
    """
    Calculates the spin axis parameters using spice kernels supplied by the input metakernel.

    Args:
        metakernel (str): Metakernel for the specified body.
        body_naifid (int): NAIF object ID code for the object of interest.

    Returns:
        tuple[float, float, float, float, int]: Tuple of spin axis parameters:
            rotation_period:    rotation period [hours]
            phase_at_j2000:     rotational phase (angle of prime meridian) at J2000 epoch [degrees]
            pole_ra:            right ascension of spin axis in J2000 frame [radians]
            pole_dec:           declination of spin axis in J2000 frame [radians]
            default_spin_flag:  0: rotation period and pole orientation are both real.
                                1: rotation period and pole orientation are both default.
                                2: rotation period is real, pole orientation is default. 
                                    (not implemented, but I imagine this could be done by 
                                    searching the small body lightcurve database) 
    """
    spice.furnsh(metakernel)

    ### This translates the new-style 8-9 digit asteroid naifIDs to the old-style 7-digit ones.
    #   Currently, the latest PCK (pck00011.tpc) uses only 7-digit asteroid IDs.
    #   For more info, see: https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/naif_ids.html#Asteroids
    # if len(str(body_naifid))>=8:
    #     pck_naifid=int("2"+str(body_naifid)[-6:])
    # else: pck_naifid = body_naifid

    try:
        spice.bodvcd(body_naifid, 'PM', 3)
        pck_naifid = body_naifid
    except:
        if len(str(body_naifid))>=8:
            pck_naifid=int("2"+str(body_naifid)[-6:])
        

    #pm: prime meridian
    body_pm = spice.bodvcd(pck_naifid, 'PM', 3)[1]

    # WO    : rotational phase (angle of prime meridian) at J2000 epoch [degrees]
    phase_at_j2000  = body_pm[0]
    # WDOT  : rotation rate in [degrees/24 hours]
    rotation_rate   = body_pm[1]

    # ZBAB  : right ascension of spin axis in J2000 frame [radians]
    pole_ra  = spice.bodvcd(pck_naifid, 'POLE_RA',  3)[1][0] * np.pi/180.
    # ZBAA  : declination of spin axis in J2000 frame [radians]
    pole_dec = spice.bodvcd(pck_naifid, 'POLE_DEC', 3)[1][0] * np.pi/180.

    # SIDAY : Rotation period in hours.
    rotation_period = (360.*24)/rotation_rate

    default_spin_flag = 0

    return (rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag)

def get_secondary_orb_params(
        orb_elems:tuple[float, float, float, float, float, float, float]) -> tuple[float, float, float]:
    """
    Calculate additional values needed for porb output.
    These values are derived from the input orbital elements tuple.

    Args:
        orb_elems (tuple[float, float, float, float, float, float, float]): Tuple of 
            orbital elements, usually from get_orbital_elements(). Contains:
                long_of_asc_node:   longitude of the ascending node [radians]
                eccentricity:       eccentricity [unitless]
                inclination:        inclination [radians]
                arg_of_peri:        argument of perihelion [radians] 
                mean_anomaly:       mean anomaly at epoch [radians]
                semimajor_axis:     semimajor axis [km]
                epoch_JD:           Julian date of epoch [Julian Date]

    Returns:
        tuple[float, float, float]: Tuple containing derived orbital parameters:
            orbit_period:           Period of the orbit in Earth days
            perihelion_date:        J2000 date (days past J2000 epoch) of previous perihelion passage
            centuries_from_j2000:   Time of reference epoch from j2000 epoch, in centuries 
    """
    (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems

    # PERIOD, OPERIOD   : Period of the orbit (Earth days)
    orbit_period = semimajor_axis**(1.5) * const.earth_year
    # TJP   : J2000 Date of previous perihelion
    perihelion_date = epoch_JD - (mean_anomaly/(2*np.pi))*orbit_period - const.j2000_JD
    # TC    : time in centuries from reference date (2000.0)
    centuries_from_j2000 = (epoch_JD - const.j2000_JD) / (const.earth_year*100)

    return (orbit_period, perihelion_date, centuries_from_j2000)

def get_secondary_spin_params(
        orb:OrbParams, 
        pole_ra:float, 
        pole_dec:float) -> tuple[float, np.ndarray, float]:
    """
    Derive secondary parameters, relating the spin axis to the orbital reference frame.
    These can all be derived from existing orbital elements and spin axis parameters.

    Args:
        orb (OrbParams): OrbParams object containing the orbital elements of the object of interest.
        pole_ra (float): Right Ascension of the spin axis in J2000 frame [radians]
        pole_dec (float): Declination of the spin axis in J2000 frame [radians]

    Returns:
        tuple[float, np.ndarray, float]: Tuple of derived spin parameters, containing:
            obliquity:                      angle between spin axis and orbit pole [radians]
            rotation_matrix_FtoB:           rotation matrix from orbital frame (F) to seasonal frame (B) [3x3 matrix]
            true_anomaly_at_vernal_equinox: True anomaly at vernal equinox [radians]
    """
    long_of_asc_node = orb.long_of_asc_node
    inclination = orb.inclination
    arg_of_peri = orb.arg_of_peri

    # AFRM  : rotation matrix from orbital (F) to J2000 (A)
    ## First do rotation from orbital (F) to ecliptic (E)
    #### the 3 Euler rotations required are:
    #### A = (-node)Z * (-inclination)X * (-argument of periapsis)Z
    rotation_matrix_FtoE = spice.eul2m(-1*long_of_asc_node, -1*inclination, -1*arg_of_peri, 3, 1, 3)
    ## add rotation from ecliptic to A frame, equatorial (J2000)
    rotation_matrix_FtoA = spice.rotmat(rotation_matrix_FtoE, -1*const.earth_obliquity, 1)
    
    # ZFAXU : orbit pole, Z unit vector, in J2000 (a 3-vector)
    orbit_Z_axis_j2000 = rotation_matrix_FtoA[:,2].copy()
    # ZBAXU : spin axis unit vector in J2000
    spin_axis_j2000 = spice.latrec(1, pole_ra, pole_dec)
    # BLIP  : angle between spin axis and orbit pole, == obliquity [radians]
    obliquity = spice.vsep(orbit_Z_axis_j2000, spin_axis_j2000)

    # ZBFXU : spin axis, rotated from j2000 into orbital (F) reference frame
    spin_axis_orbital = np.matmul(rotation_matrix_FtoA.T, spin_axis_j2000)
    # XBFXU : Vernal equinox, along spinAxis cross OrbitPole (orbit pole in F is [0,0,1]) 
    spin_cross_Z = np.cross(spin_axis_orbital, [0,0,1])
    vernal_equinox_orbital = spin_cross_Z / np.linalg.norm(spin_cross_Z)
    # YBFXU : Y-axis of Season system
    yaxis_season_orbital = np.cross(spin_axis_orbital, vernal_equinox_orbital)
    # BFRM  : rotation matrix from orbital frame (F) to seasonal frame (B)
    rotation_matrix_FtoB = np.vstack((vernal_equinox_orbital,yaxis_season_orbital,spin_axis_orbital))
    # TAV   : True anomaly at vernal equinox [radians] (prograde angle between perihelion and VE vectors)
    true_anomaly_at_vernal_equinox = np.arctan2(vernal_equinox_orbital[1], vernal_equinox_orbital[0])

    return (obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox)

def alt_get_secondary_spin_params(
        orb:OrbParams, 
        obliquity:float, 
        true_anomaly_at_vernal_equinox:float) -> tuple[float, float, np.ndarray]:
    """
    Uses obliquity and true anomaly at vernal equinox to get the pole ra and dec, 
    then calculates the rotation matrix. This is potentially more in line with how people 
    think about objects with unknown spins, so it's probably more useful for manually 
    inputting such a case.

    Args:
        orb (OrbParams): OrbParams object containing the orbital elements of the object 
            of interest.
        obliquity (float): Angle between the object's spin axis and the north pole of 
            its orbit. [radians]
        true_anomaly_at_vernal_equinox (float): True anomaly at vernal equinox (prograde 
            angle between perihelion vector and vernal equinox vector) [radians]

    Returns:
        tuple[float, float, np.ndarray]: Tuple of derived spin parameters, containing:
            pole_ra:                Right Ascension of the spin axis in J2000 frame [radians]
            pole_dec:               Declination of the spin axis in J2000 frame [radians]
            rotation_matrix_FtoB:   Rotation matrix from orbital frame (F) to seasonal frame (B) [3x3 matrix]
    """
    long_of_asc_node = orb.long_of_asc_node
    inclination = orb.inclination
    arg_of_peri = orb.arg_of_peri

    # AFRM  : rotation matrix from orbital (F) to J2000 (A)
    ## First do rotation from orbital (F) to ecliptic (E)
    #### the 3 Euler rotations required are:
    #### A = (-node)Z * (-inclination)X * (-argument of periapsis)Z
    rotation_matrix_FtoE = spice.eul2m(-1*long_of_asc_node, -1*inclination, -1*arg_of_peri, 3, 1, 3)
    ## add rotation from ecliptic to A frame, equatorial (J2000)
    rotation_matrix_FtoA = spice.rotmat(rotation_matrix_FtoE, -1*const.earth_obliquity, 1)
    
    # ZFAXU : orbit pole, Z unit vector, in J2000 (a 3-vector)
    orbit_Z_axis_j2000 = rotation_matrix_FtoA[:,2].copy()

    # to get spin axis vector, rotate orbit_Z_axis_j2000 around -1*(vernal equinox vector) by obliquity.
    # so I need vernal equinox vector in j2000. 
    vernal_equinox_orbital = spice.rotvec([1,0,0], -1*true_anomaly_at_vernal_equinox, 3)
    vernal_equinox_j2000 = np.matmul(rotation_matrix_FtoA, vernal_equinox_orbital)

    spin_axis_j2000 = spice.vrotv(orbit_Z_axis_j2000, -1*vernal_equinox_j2000, obliquity)

    # ZBAB  : right ascension of spin axis in J2000 frame [radians]
    # ZBAA  : declination of spin axis in J2000 frame [radians]
    _, pole_ra, pole_dec = spice.recrad(spin_axis_j2000)

    # ZBFXU : spin axis, rotated from j2000 into orbital (F) reference frame
    spin_axis_orbital = np.matmul(rotation_matrix_FtoA.T, spin_axis_j2000)
    # YBFXU : Y-axis of Season system
    yaxis_season_orbital = np.cross(spin_axis_orbital, vernal_equinox_orbital)
    # BFRM  : rotation matrix from orbital frame (F) to seasonal frame (B)
    rotation_matrix_FtoB = np.vstack((vernal_equinox_orbital,yaxis_season_orbital,spin_axis_orbital))

    return (pole_ra, pole_dec, rotation_matrix_FtoB)


def get_porb_params(
        body_name: str, 
        body_naifid: int, 
        metakernel: str, 
        epoch_date: datetime.datetime = defaults.epoch_date) -> PorbParams:
    """
    Generate the standard PORB output for a specified body, at some epoch, using 
    SPICE kernels. Return a PorbParams object containing the standard PORB parameters.

    Args:
        body_name (str): String identifier for the object of interest.
        body_naifid (int): NAIF object ID code for the object of interest.
        metakernel (str): Metakernel for the specified body.
        epoch_date (datetime.datetime, optional): epoch at which to calculate orbital params 
            (must be covered by available kernels). Defaults to defaults.epoch_date.

    Returns:
        PorbParams: A PorbParams object containing orbit and spin parameters.
    """
    # Determine orbital elements for either the specified body, or, if the 
    # specified body is a satellite, its sun-orbiting parent.
    body_type = get_body_type(body_naifid)

    orbital_naifid = get_orbital_naifid(metakernel, body_naifid, epoch_date)
    
    orb_elems = get_orbital_elements(metakernel, orbital_naifid, 'SUN', epoch_date)
    

    # Determine the parameters defining the specified body's spin axis.
    try:
        spin_axis = get_spin_axis(metakernel, body_naifid)
    except spice.utils.exceptions.SpiceKERNELVARNOTFOUND:
        print(f'WARNING!')
        print(f'No spin axis info found for body: {body_name} in PCK from metakernel: {metakernel}')
        print(f'Make sure PCK has data for this body, or specify spin axis directly. (not yet implemented!)')
        print(f'Using default spin axis (24hr period, aligned w/ ecliptic)')
        print()
        spin_axis = defaults.spin_axis

    # Generate the parameters used for standard PORB output. 
    # out  = get_porb_params(body_name, body_naifid, body_type, orb_elems, spin_axis)

    orb  = OrbParams.from_elems(orb_elems)
    spin = SpinParams.from_spin_axis(spin_axis, orb)
    out  = PorbParams.from_orb_and_spin_params(body_name, body_type, body_naifid, orb, spin)

    return out

def high_level_get_porb_params(
        body_name:str, 
        update_kernels:bool = False, 
        default_mk:str=f'{install.kernels_dir}/mk/krc_default.tm', 
        naifid_map_file:str=f'{install.kernels_dir}/naifid_map.csv',
        kernels_dir:str=install.kernels_dir) -> PorbParams:
    """
    Generate a PorbParams object, containing orbital and spin axis parameters, for a 
    body of interest, specified by a string identifier. 

    Args:
        body_name (str): String uniquely identifying the body of interest. Case-insensitive. 
            This string cannot be castable to an integer. 
            See kernel_mgmt.py:get_naifid() for detailed formatting constraints.
        update_kernels (bool, optional): Flag to force a kernel update for an object, even
            if a metakernel for it already exists in the cache. Defaults to False.
        default_mk (str, optional): metakernel containing core kernels loaded by default. 
            Defaults to f'{install.kernels_dir}/mk/krc_default.tm'.
        naifid_map_file (str, optional): Path to the file containing the name-naifid mapping. 
            Defaults to f'{install.kernels_dir}/naifid_map.csv'.
        kernels_dir (str, optional): Path to directory containing kernels. 
            Defaults to install.kernels_dir.

    Returns:
        PorbParams: A PorbParams object containing orbit and spin parameters.
    """
    metakernel = get_mk(body_name, update_kernels, kernels_dir=kernels_dir, default_mk=default_mk, naifid_map_file=naifid_map_file)
    naifid = get_naifid(body_name, default_mk=default_mk, naifid_map_file=naifid_map_file)
    porb_params = get_porb_params(body_name, naifid, metakernel)
    
    return porb_params

def modify_porb_params(porb_params:PorbParams,
        long_of_asc_node:float|None = None,
        eccentricity:float|None = None,
        inclination:float|None = None,
        arg_of_peri:float|None = None,
        semimajor_axis:float|None = None,
        orbit_period:float|None = None,
        perihelion_date:float|None = None,
        centuries_from_j2000:float|None = None,
        epoch_JD:float|None = None,
        mean_anomaly:float|None = None,
        rotation_period:float|None = None,         
        phase_at_j2000:float|None = None,
        pole_ra:float|None = None,
        pole_dec:float|None = None,   
        default_spin_flag:int|None = None,  
        obliquity:float|None = None,
        rotation_matrix_FtoB:np.ndarray|None = None,
        true_anomaly_at_vernal_equinox:float|None = None
        ) -> PorbParams:
    """
    Constructs PorbParams object using a PorbParams object as input, plus optional inputs
    with which to replace each parameter. The underlying functions ensure conflicting 
    parameters are not set at the same time, and correctly derives dependent parameters 
    based on the input.

    Args:
        porb_params (PorbParams): PorbParams object to modify.
        long_of_asc_node (float | None, optional): longitude of the ascending node [radians]. 
            Defaults to None.
        eccentricity (float | None, optional): eccentricity [unitless]. 
            Defaults to None.
        inclination (float | None, optional): inclination [radians]. 
            Defaults to None.
        arg_of_peri (float | None, optional): argument of perihelion [radians]. 
            Defaults to None.
        semimajor_axis (float | None, optional): semimajor axis [km]. 
            Defaults to None.
        orbit_period (float | None, optional): Period of the orbit in Earth days. 
            Defaults to None.
        perihelion_date (float | None, optional): J2000 date (days past J2000 epoch) 
            of previous perihelion passage. Defaults to None.
        centuries_from_j2000 (float | None, optional): Time of reference epoch from 
            J2000 epoch, in centuries. Defaults to None.
        epoch_JD (float | None, optional): Julian date of epoch [Julian Date]. 
            Defaults to None.
        mean_anomaly (float | None, optional): mean anomaly at epoch [radians]. 
            Defaults to None.      
        rotation_period (float | None, optional): rotation period [hours]. 
            Defaults to None.
        phase_at_j2000 (float | None, optional): rotational phase (angle of prime 
            meridian) at J2000 epoch [degrees]. Defaults to None.
        pole_ra (float | None, optional): right ascension of spin axis in J2000 
            frame [radians]. Defaults to None.
        pole_dec (float | None, optional): declination of spin axis in J2000 
            frame [radians]. Defaults to None.
        default_spin_flag (int | None, optional): Flag indicating if the spin parameters 
            really describe the object's spin state, or are default placeholder values.
            Possible values listed below. Defaults to None.
                0: rotation period and pole orientation are both real.
                1: rotation period and pole orientation are both default.
                2: rotation period is real, pole orientation is default. 
                    (not implemented, but I imagine this could be done by 
                    searching the small body lightcurve database). 
        obliquity (float): Angle between the object's spin axis and the north pole of 
            its orbit [radians]. Defaults to None.
        rotation_matrix_FtoB (np.ndarray | None, optional): rotation matrix from 
            orbital frame (F) to seasonal frame (B) [3x3 matrix]. Defaults to None.
        true_anomaly_at_vernal_equinox (float): True anomaly at vernal equinox (prograde 
            angle between perihelion vector and vernal equinox vector) [radians].
            Defaults to None.

    Returns:
        PorbParams: A PorbParams object containing orbit and spin parameters for 
        the body of interest.
    """
    orb = OrbParams.from_modified_params(porb_params,
            long_of_asc_node = long_of_asc_node, 
            eccentricity     = eccentricity, 
            inclination      = inclination, 
            arg_of_peri      = arg_of_peri, 
            semimajor_axis   = semimajor_axis, 
            orbit_period     = orbit_period, 
            perihelion_date  = perihelion_date, 
            centuries_from_j2000 = centuries_from_j2000, 
            epoch_JD         = epoch_JD, 
            mean_anomaly     = mean_anomaly)

    spin = SpinParams.from_modified_params(porb_params, orb,
            rotation_period  = rotation_period,
            phase_at_j2000   = phase_at_j2000,
            pole_ra          = pole_ra,
            pole_dec         = pole_dec,
            default_spin_flag = default_spin_flag,
            obliquity        = obliquity,
            rotation_matrix_FtoB = rotation_matrix_FtoB,
            true_anomaly_at_vernal_equinox = true_anomaly_at_vernal_equinox)

    return PorbParams.from_orb_and_spin_params(porb_params.NAME, porb_params.body_type, porb_params.PLANUM, orb, spin)
