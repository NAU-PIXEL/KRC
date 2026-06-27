#! /bin/bash/python

# get rotation matrix for KRC input file.
# intended to replace PORB fortran stuff, specifically porbig.f
# returns BFRM, in the PORB/KRC nomenclature.

import numpy as np
import spiceypy as spice
import datetime
from . import constants as const
from .kernel_mgmt import kernels_dir, default_mk, make_sb_mk, make_satellite_mk, get_naifid, cached_mk_exists, get_cached_mk
# from .body_params import write_hdf, get_body_params
from . import defaults 
from . import install
import h5py
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

    @classmethod
    def from_elems(cls, orb_elems) -> Self:
        '''
        Constructs an OrbParams object from an orb_elems tuple, 
        as would be output by get_orbital_elements().
        '''
        (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems
        (orbit_period, perihelion_date, centuries_from_j2000) = get_secondary_orb_params(orb_elems)
        return cls(long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD, orbit_period, perihelion_date, centuries_from_j2000)
    
    @classmethod
    def from_elems_and_second_params(cls, orb_elems, orb_second_params) -> Self:
        (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems
        (orbit_period, perihelion_date, centuries_from_j2000) = orb_second_params
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

    @classmethod
    def from_spin_axis(cls, spin_axis, orb_elems) -> Self:
        '''
        Constructs a SpinParams object from spin_axis and orb_elems tuples, 
        as would be output by get_orbital_elements() and get_spin_axis().
        '''
        (rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag) = spin_axis 
        (obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox) = get_secondary_spin_params(orb_elems, spin_axis) 
        return cls(rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag, obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox)
    
    def set_obliq_and_true_anomaly(self, obliquity:float, true_anomaly_at_vernal_equinox:float, orb_elems) -> Self:
        '''
        updates the obliquity and true anomaly at vernal equinox, accounting for
        the impacts on pole orientation and rotation matrix.
        '''
        self.obliquity = obliquity
        self.true_anomaly_at_vernal_equinox = true_anomaly_at_vernal_equinox
        self.pole_ra, self.pole_dec, self.rotation_matrix_FtoB = alt_get_secondary_spin_params(orb_elems, obliquity, true_anomaly_at_vernal_equinox)
        return self

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

    @classmethod
    def from_orb_and_spin_params(cls,
                                 body_name: str, 
                                 body_type: str, 
                                 body_naifid: int, 
                                 orb: OrbParams, 
                                 spin: SpinParams) -> Self:
        '''
        Constructs a PorbParams object from OrbParams and SpinParams objects.
        '''
    
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
        '''
        returns Fortran-style PORB output as a multiline string.  
        '''
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
        '''
        returns Fortran-style PORB output as a multiline string, including variable labels. 
        '''

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
        '''
        Constructs a PorbParams object from a Fortran-style PORB text table.
        '''
        flat_bfrm = np.zeros(9)

        lines = porb_str.split('\n')
        porb_version = lines[0].split(' ')[0][5:]
        generation_date = lines[0].split(' ')[1]
        NAME = lines[0].split(':')[-1]
        
        PLANUM, TC, RODE, CLIN, ARGP = lines[1].split(' ')
        XECC, SJA, EOBL, SFLAG, ZBAA = lines[2].split(' ')
        ZBAB, WDOT, WO, OPERIOD, TJP = lines[3].split(' ')
        SIDAY, spar17, TAV, BLIP, PBUG = lines[4].split(' ')
        spar21, flat_bfrm[0], flat_bfrm[1], flat_bfrm[2], flat_bfrm[3] = lines[5].split(' ')
        flat_bfrm[4], flat_bfrm[5], flat_bfrm[6], flat_bfrm[7], flat_bfrm[8] = lines[6].split(' ')

        BFRM = flat_bfrm.reshape(3,3).T

        return cls(-1, porb_version, generation_date, NAME, 'unknown', PLANUM, TC, RODE, CLIN, ARGP, XECC, SJA, EOBL, SFLAG, ZBAA, ZBAB, WDOT, WO, OPERIOD, TJP, SIDAY, spar17, TAV, BLIP, PBUG, spar21, BFRM)




def get_orbital_naifid(metakernel, body_naifid, epoch_date):
    '''
    Use spice to determine if the specified body orbits the sun. If it does, return the 
    body's naifid, and if not, return the naifid of whatever parent body does orbit the sun.
    For KRC purposes, we only care about the orbit of the parent body (or, more precisely, the system barycenter).
    Returns:
    orbital_id (int) : The NAIF id to be used in calculating the specified body's orbit around the sun.
    '''
    spice.furnsh(metakernel)
    et = spice.datetime2et(epoch_date)

    handle, descr, ident = spice.spksfs(body_naifid, et, 40) 
    dc, ic = spice.dafus(descr, 2, 6)
    center_id = ic[1]

    ### TODO: make this work for binary asteroids.
    if center_id in (0, 10):
        # body orbits sun
        orbital_id = body_naifid
    else:
        # body does not orbit the sun
        orbital_id = center_id

    # comments = spice.dafec(handle,100, 100)[1]
    # # is SPK source JPL Horizons?
    # from_horizons = 'Horizons On-Line Ephemeris System' in '\t'.join(comments)

    return orbital_id

def get_orbital_elements(metakernel, orbital_naifid, parent, epoch_date):
    '''
    Use spice to get the keplerian(?) orbital elements for a specified body.
    returns a tuple of floats:
    long_of_asc_node:   longitude of the ascending node [radians]
    eccentricity:       eccentricity [unitless]
    inclination:        inclination [radians]
    arg_of_peri:        argument of perihelion [radians] 
    mean_anomaly:       mean anomaly at epoch [radians]
    semimajor_axis:     semimajor axis [km]
    epoch_JD:           Julian date of epoch [Julian Date]
    '''
    spice.furnsh(metakernel)

    et = spice.datetime2et(epoch_date)
    epoch_JD = float(spice.et2utc(et,'J', 6).split(' ')[-1])

    # get the state vector of body (or its system barycenter) relative to sun.
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

def get_spin_axis(metakernel, body_naifid):
    '''
    Calculates the spin axis using spice kernels.
    returns tuple of floats:
    rotation_period:        rotation period [hours]
    phase_at_j2000:         rotational phase (angle of prime meridian) at J2000 epoch [degrees]
    pole_ra:                right ascension of spin axis in J2000 frame [radians]
    pole_dec:               declination of spin axis in J2000 frame [radians]
    default_spin_flag:  int     0: rotation period and pole orientation are both real.
                                1: rotation period and pole orientation are both default.
                                2: rotation period is real, pole orientation is default. (not implemented, but I imagine this could be done by searching the small body lightcurve database) 
    '''    
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

def get_secondary_orb_params(orb_elems):
    '''
    Calculate additional values needed for porb output.
    These values are all derived from the orbital elements.
    returns:
    orbit_period:           Period of the orbit in Earth days
    perihelion_date:        J2000 date (days past J2000 epoch) of previous perihelion passage
    centuries_from_j2000:   Time of reference epoch from j2000 epoch, in centuries 
    '''

    (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems

    # PERIOD, OPERIOD   : Period of the orbit (Earth days)
    orbit_period = semimajor_axis**(1.5) * const.earth_year
    # TJP   : J2000 Date of previous perihelion
    perihelion_date = epoch_JD - (mean_anomaly/(2*np.pi))*orbit_period - const.j2000_JD
    # TC    : time in centuries from reference date (2000.0)
    centuries_from_j2000 = (epoch_JD - const.j2000_JD) / (const.earth_year*100)

    return (orbit_period, perihelion_date, centuries_from_j2000)

def get_secondary_spin_params(orb_elems, spin_axis):
    '''
    Derive secondary parameters, relating the spin axis to the orbital reference frame.
    These can all be derived from existing orbital elements and spin axis parameters.
    Returns: 
    obliquity:                      angle between spin axis and orbit pole [radians]
    rotation_matrix_FtoB:           rotation matrix from orbital frame (F) to seasonal frame (B) [3x3 matrix]
    true_anomaly_at_vernal_equinox: True anomaly at vernal equinox [radians]
    '''    
    (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems
    (rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag) = spin_axis

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

def alt_get_secondary_spin_params(orb_elems, obliquity, true_anomaly_at_vernal_equinox):
    '''
    Uses obliquity and true anomaly at vernal equinox to get the pole ra and dec, 
    then calculates the rotation matrix. 
    this is potentially more in line with how people think about objects with unknown spins,
    so it's probably more useful for manually inputting such a case.

    inputs:
    orb_elems:                      tuple containing keplerian(?) orbital elements (see get_orbital_elements())
    obliquity:                      (BLIP) angle between spin axis and orbit pole [radians]
    true_anomaly_at_vernal_equinox: (TAV) True anomaly at vernal equinox [radians]

    '''
    (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems

    # AFRM  : rotation matrix from orbital (F) to J2000 (A)
    ## First do rotation from orbital (F) to ecliptic (E)
    #### the 3 Euler rotations required are:
    #### A = (-node)Z * (-inclination)X * (-argument of periapsis)Z
    rotation_matrix_FtoE = spice.eul2m(-1*long_of_asc_node, -1*inclination, -1*arg_of_peri, 3, 1, 3)
    ## add rotation from ecliptic to A frame, equatorial (J2000)
    rotation_matrix_FtoA = spice.rotmat(rotation_matrix_FtoE, -1*const.earth_obliquity, 1)
    
    # ZFAXU : orbit pole, Z unit vector, in J2000 (a 3-vector)
    orbit_Z_axis_j2000 = rotation_matrix_FtoA[:,2].copy()

    # to get spin axis vector, rotate orbit_Z_axis_j2000 around vernal equinox vector by obliquity.
    # so I need vernal equinox vector in j2000. 
    vernal_equinox_orbital = spice.rotvec([1,0,0], true_anomaly_at_vernal_equinox, 3)
    vernal_equinox_j2000 = np.matmul(rotation_matrix_FtoA, vernal_equinox_orbital)

    spin_axis_j2000 = spice.vrotv(orbit_Z_axis_j2000, vernal_equinox_j2000, obliquity)
    # ZBAB  : right ascension of spin axis in J2000 frame [radians]
    # ZBAA  : declination of spin axis in J2000 frame [radians]
    _, pole_ra, pole_dec = spice.reclat(spin_axis_j2000)

    # ZBFXU : spin axis, rotated from j2000 into orbital (F) reference frame
    spin_axis_orbital = np.matmul(rotation_matrix_FtoA.T, spin_axis_j2000)
    # YBFXU : Y-axis of Season system
    yaxis_season_orbital = np.cross(spin_axis_orbital, vernal_equinox_orbital)
    # BFRM  : rotation matrix from orbital frame (F) to seasonal frame (B)
    rotation_matrix_FtoB = np.vstack((vernal_equinox_orbital,yaxis_season_orbital,spin_axis_orbital))

    return (pole_ra, pole_dec, rotation_matrix_FtoB)

def get_body_type(body_naifid):
    '''
    return the type of a body, given its naifid
    '''
    body_type = 'General'

    if (body_naifid < 1000) and (body_naifid%100 == 99):
        body_type = 'Planet'
    elif (body_naifid > 10) and (body_naifid < 100000):
        body_type = 'Satellite'
    elif (body_naifid >= 1000000) and (body_naifid < 2000000):
        body_type = 'Comet'
    elif (body_naifid >= 2000000) and (body_naifid < 1000000000):
        body_type = 'Minor'
    
    return body_type

def old_get_porb_params(body_name, body_naifid, body_type, orb_elems, spin_axis):
    '''
    Determines the orbital parameters of a body based on spice kernels.
    Outputs a dictionary containing all the variables to include in the standard PORB
    input table for KRC.
    '''
    # Unpack input orbital elements, derive secondary orbital parameters
    (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems
    (orbit_period, perihelion_date, centuries_from_j2000) = get_secondary_orb_params(orb_elems)

    # Unpack input spin axis parameters, derive secondary spin parameters
    (rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag) = spin_axis 
    (obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox) = get_secondary_spin_params(orb_elems, spin_axis)

    # hacking in a test case for Justitia, 2026.04.28.
    # Basically, just run everything as normal first, modify the values with inputs,
    # then update the secondary values that flow from the first ones.
    # I'll need to consider how to handle user inputs more appropriately later.
    # if body_name=='Justitia':
    if False:
        semimajor_axis = 2.613
        eccentricity = 0.
        # need to repack orb_elems with updated values:
        orb_elems = (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD)
        (orbit_period, perihelion_date, centuries_from_j2000) = get_secondary_orb_params(orb_elems)

        obliquity = 0.
        true_anomaly_at_vernal_equinox = 0.
        pole_ra, pole_dec, rotation_matrix_FtoB = alt_get_secondary_spin_params(orb_elems, obliquity, true_anomaly_at_vernal_equinox)
    

    ##### record variables in output dictionary #####
    out={}
    out['default_spin']     = default_spin_flag
    out['porb_version']     = const.porb_version
    out['generation_date']  = datetime.datetime.now().strftime('%Y %b %d %H:%M:%S')
    out['NAME']             = body_name
    out['body_type']        = body_type

    out['PLANUM']           = body_naifid    
    if   out['PLANUM']  >= 20000000:
         out['PLANUM']  -= 20000000
    elif out['PLANUM']  >=  2000000:
         out['PLANUM']  -=  2000000

    out['TC']               = centuries_from_j2000
    out['RODE']             = long_of_asc_node
    out['CLIN']             = inclination
    out['ARGP']             = arg_of_peri

    out['XECC']             = eccentricity
    out['SJA']              = semimajor_axis
    out['EOBL']             = const.earth_obliquity
    out['SFLAG']            = const.sflag
    out['ZBAA']             = pole_dec

    out['ZBAB']             = pole_ra
    out['WDOT']             = (360.*24)/rotation_period
    out['WO']               = phase_at_j2000
    out['OPERIOD']          = orbit_period
    out['TJP']              = perihelion_date

    out['SIDAY']            = rotation_period
    out['spar17']           = const.spar17
    out['TAV']              = true_anomaly_at_vernal_equinox
    out['BLIP']             = obliquity
    out['PBUG']             = const.pbug

    out['spar21']           = const.spar21
    out['BFRM 1']           = rotation_matrix_FtoB[0][0]
    out['BFRM 2']           = rotation_matrix_FtoB[1][0]
    out['BFRM 3']           = rotation_matrix_FtoB[2][0]
    out['BFRM 4']           = rotation_matrix_FtoB[0][1]

    out['BFRM 5']           = rotation_matrix_FtoB[1][1]
    out['BFRM 6']           = rotation_matrix_FtoB[2][1]
    out['BFRM 7']           = rotation_matrix_FtoB[0][2]
    out['BFRM 8']           = rotation_matrix_FtoB[1][2]
    out['BFRM 9']           = rotation_matrix_FtoB[2][2]
    
    return out

def old_format_output(out: dict, verbose=False):
    '''
    Formats variables stored in out into a the Fortran style PORB output.
    Outputs a multiline string. 
    Variable labels are optionally included using the verbose flag. 
    '''
    out_str = ''
    if verbose:
        out_str += f"<--VERSION---> <--generation date->           IPLAN      TC orbit:pole\n"
        out_str += f"PORB:{out['porb_version']} {out['generation_date']} IPLAN,TC= {out['PLANUM']:5d} {out['TC']:7.5g} {out['NAME']}:{out['NAME']}\n"
        out_str += f"     PLANUM             Tc           RODE           CLIN           ARGP\n"
        out_str += f" {out['PLANUM']:10d}     {out['TC']:10.7g}     {out['RODE']:10.7g}      {out['CLIN']:.7E} {out['ARGP']:10.7f}\n"
        out_str += f"       XECC            SJA           EOBL          SFLAG           ZBAA\n"
        out_str += f"  {out['XECC']:.7E} {out['SJA']:10.7g}     {out['EOBL']:10.7g}     {out['SFLAG']:10.7g}     {out['ZBAA']:10.7g}\n"
        out_str += f"       ZBAB           WDOT             WO        OPERIOD            TJP\n"
        out_str += f" {out['ZBAB']:10.7g}     {out['WDOT']:10.7g}     {out['WO']:10.7g}     {out['OPERIOD']:10.7g}     {out['TJP']:10.7g}\n"
        out_str += f"      SIDAY          spare            TAV           BLIP           PBUG\n"
        out_str += f" {out['SIDAY']:10.7g}     {out['spar17']:10.7g}     {out['TAV']:10.7g}     {out['BLIP']:10.7g}     {out['PBUG']:10.7g}\n"
        out_str += f"      spare         BFRM 1              2              3              4\n"
        out_str += f" {out['spar21']:10.7g}     {out['BFRM 1']:10.7f}     {out['BFRM 2']:10.7f}     {out['BFRM 3']:10.7f}     {out['BFRM 4']:10.7f}\n"
        out_str += f"          5              6              7              8         BFRM 9\n"
        out_str += f" {out['BFRM 5']:10.7f}     {out['BFRM 6']:10.7f}     {out['BFRM 7']:10.7f}     {out['BFRM 8']:10.7f}     {out['BFRM 9']:10.7f}\n"

    else:
        out_str += f"PORB:{out['porb_version']} {out['generation_date']} IPLAN,TC= {out['PLANUM']:5.4g} {out['TC']:7.5g} {out['NAME']}:{out['NAME']}\n"
        out_str += f" {out['PLANUM']:10.7g}     {out['TC']:10.7g}     {out['RODE']:10.7g}      {out['CLIN']:.7E} {out['ARGP']:10.7f}\n"
        out_str += f"  {out['XECC']:.7E} {out['SJA']:10.7g}     {out['EOBL']:10.7g}     {out['SFLAG']:10.7g}     {out['ZBAA']:10.7g}\n"
        out_str += f" {out['ZBAB']:10.7g}     {out['WDOT']:10.7g}     {out['WO']:10.7g}     {out['OPERIOD']:10.7g}     {out['TJP']:10.7g}\n"
        out_str += f" {out['SIDAY']:10.7g}     {out['spar17']:10.7g}     {out['TAV']:10.7g}     {out['BLIP']:10.7g}     {out['PBUG']:10.7g}\n"
        out_str += f" {out['spar21']:10.7g}     {out['BFRM 1']:10.7f}     {out['BFRM 2']:10.7f}     {out['BFRM 3']:10.7f}     {out['BFRM 4']:10.7f}\n"
        out_str += f" {out['BFRM 5']:10.7f}     {out['BFRM 6']:10.7f}     {out['BFRM 7']:10.7f}     {out['BFRM 8']:10.7f}     {out['BFRM 9']:10.7f}\n"

    return out_str


def get_porb_params(
        body_name: str, 
        body_naifid: int, 
        metakernel: str, 
        epoch_date: datetime.datetime = defaults.epoch_date, 
        verbose: bool = False):
    '''
    Generate the standard PORB output for a specified body, at some epoch, using 
    SPICE kernels. Return a PorbParams object containing the standard PORB parameters.

    args:
    body_name: 
    body_naifid: 
    metakernel:

    epoch_date:  epoch at which to calculate orbital params (must be covered by available kernels)

    returns:
    out:        PorbParams object
    '''

    # Determine orbital elements for either the specified body, or, if the 
    # specified body is a satellite, its sun-orbiting parent.
    orbital_naifid = get_orbital_naifid(metakernel, body_naifid, epoch_date)
    orb_elems = get_orbital_elements(metakernel, orbital_naifid, 'SUN', epoch_date)
    body_type = get_body_type(body_naifid)

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
    spin = SpinParams.from_spin_axis(spin_axis)
    out  = PorbParams.from_orb_and_spin_params(body_name, body_type, body_naifid, orb, spin)

    return out

def high_level_get_porb_params(body_name:str, update_kernels:bool = False) -> PorbParams:
    naifid = get_naifid(body_name)
    if cached_mk_exists(naifid) and update_kernels == False:
        metakernel = get_cached_mk(naifid)
    else:
        body_type = get_body_type(naifid)
        if body_type == 'Planet':
            # planet barycenters are all covered by default_mk
            metakernel = default_mk
        if body_type == 'Satellite':
            # make a satellite mk associated with parent body
            metakernel = make_satellite_mk(naifid)
        elif body_type == 'Comet' or body_type == 'Minor':
            metakernel = make_sb_mk(naifid) 

    porb_params = get_porb_params(body_name, naifid, metakernel)
    
    return porb_params

if __name__ == '__main__':
    # Include headers in output?
    verbose = True

    # body_names      = [ 'Mars', 'Deimos', 'Ceres', 'Didymos', 'Dimorphos', 'Chimaera']
    # body_naifids    = [ 499, 402, 20000001, 920065803, 120065803, 20000623]

    body_names      = ['Justitia']
    body_naifids    = [20000269]

    # epoch at which to calculate orbital params (must be covered by available kernels)
    # epoch_date = defaults.epoch_date
    # metakernel = f'{kernels_dir}/mk/krc_default.tm'
    
    for i in range(len(body_names)):
        print()
        # metakernel = get_mk(f'{body_names[i]}')
        metakernel = f'{kernels_dir}/mk/JUSTITIA.tm'
        porb_params = get_porb_params(body_names[i], body_naifids[i], metakernel)
        if verbose:
            print(porb_params.verbose_output())
        else:
            print(str(porb_params))
        
        # print(format_output(out, verbose=True))
        # write_hdf(out, '/home/nsmith/KRC/pyorb/test')
        # body_params = get_body_params(out, metakernel)
        # write_hdf(out, body_params, install.porb_defaults_dir)


#### ./krc_justitia.dv /work/nsmith/justitia/krc/tmp/260327_justitia_1 00599

# function to take body name/ number as a string, get the naifid.

# function to take... I guess the naifid? and see if there's a cached metakernel for it.

# high-level function to take a body name, get the metakernel and naifid, (optionally updating kernels)
# and manage any kwargs to modify default values, then return a porb_params object.

# high-level function (in another file) to attach other params for writing
# output to a defaults hdf.

# high-level function to run the above function for a standard list of bodies? or maybe
# every body already in the cache? while forcing a kernel update.

# high-level function to read the defaults file, extract porb_params object and other objects.

# values in those objects can then be modified. 
# (not recommended to modify from cached defaults directly, as linked values will not
# update automatically, eg semi-major axis & operiod.)
# preferred behavior is to construct a fresh instance of the object directly?

# objects can then:
    # be passed as inputs to pykrc
    # be used by a python based fortran krc interface (?)

# the defaults HDFs can be read by the existing dv interface to work with fortran krc