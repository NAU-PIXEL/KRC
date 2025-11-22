#! /bin/bash/python

# get rotation matrix for KRC input file.
# intended to replace PORB fortran stuff, specifically porbig.f
# returns BFRM, in the PORB/KRC nomenclature.

import numpy as np
import spiceypy as spice
import datetime
import constants as const
import defaults 

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

def get_orbital_elements(metakernel, orbital_naifid, epoch_date):
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
    state_vector = spice.spkezr(str(orbital_naifid), et, 'ECLIPJ2000', 'NONE', 'SUN')[0]

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
    '''    
    spice.furnsh(metakernel)

    ### This translates the new-style 8-9 digit asteroid naifIDs to the old-style 7-digit ones.
    #   Currently, the latest PCK (pck00011.tpc) uses only 7-digit asteroid IDs.
    if len(str(body_naifid))>=8:
        pck_naifid=int("2"+str(body_naifid)[-6:])
    else: pck_naifid = body_naifid

    #pm: prime meridian
    body_pm = spice.bodvcd(pck_naifid, 'PM', 3)[1]

    # WO    : rotational phase (angle of prime meridian) at J2000 epoch [degrees]
    phase_at_j2000  = body_pm[0]
    # WDOT  : rotation rate in [degrees/24 hours]
    rotation_rate   = body_pm[1]

    # ZBAB  : right ascension of spin axis in J2000 frame [radians]
    pole_ra  = spice.bodvcd(pck_naifid, 'POLE_RA',  3)[1][0]
    # ZBAA  : declination of spin axis in J2000 frame [radians]
    pole_dec = spice.bodvcd(pck_naifid, 'POLE_DEC', 3)[1][0]

    # SIDAY : Rotation period in hours.
    rotation_period = (360.*24)/rotation_rate

    return (rotation_period, phase_at_j2000, pole_ra, pole_dec)

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
    perihelion_date = epoch_JD - (mean_anomaly/360.)*orbit_period - const.j2000_JD
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
    (rotation_period, phase_at_j2000, pole_ra, pole_dec) = spin_axis

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
    # TAV   : True anomaly at vernal equinox [radians]
    true_anomaly_at_vernal_equinox = np.arctan2(vernal_equinox_orbital[1], vernal_equinox_orbital[0])

    return (obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox)


def get_porb_params(body_name, body_naifid, orb_elems, spin_axis):
    '''
    Determines the orbital parameters of a body based on spice kernels.
    Outputs a dictionary containing all the variables to include in the standard PORB
    input table for KRC.
    '''
    # Unpack input orbital elements, derive secondary orbital parameters
    (long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems
    (orbit_period, perihelion_date, centuries_from_j2000) = get_secondary_orb_params(orb_elems)

    # Unpack input spin axis parameters, derive secondary spin parameters
    (rotation_period, phase_at_j2000, pole_ra, pole_dec) = spin_axis 
    (obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox) = get_secondary_spin_params(orb_elems, spin_axis)
    ### Note: theoretically, I think you could input the spin axis as obliquity and true 
    ### anomaly at vernal equinox, and derive pole_ra and pole_dec and the rotation matrix.
    ### this would potentially be more in line with how people think about objects with unknown spins.
    
    ##### record variables in output dictionary #####
    out={}
    out['porb_version']     = const.porb_version
    out['generation_date']  = datetime.datetime.now().strftime('%Y %b %d %H:%M:%S')
    out['NAME']             = body_name

    out['PLANUM']           = body_naifid    
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

def format_output(out: dict, verbose=False):
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

def main(body_name, body_naifid, metakernel, epoch_date, verbose=True):
    '''
    Generate the standard PORB output for a specified body, at some epoch, using 
    SPICE kernels. Return the formatted output using PORB's standard FORTRAN style formatting.
    '''

    # Determine orbital elements for either the specified body, or, if the 
    # specified body is a satellite, its sun-orbiting parent.
    orbital_naifid = get_orbital_naifid(metakernel, body_naifid, epoch_date)
    orb_elems = get_orbital_elements(metakernel, orbital_naifid, epoch_date)

    # Determine the parameters defining the specified body's spin axis.
    try:
        spin_axis = get_spin_axis(metakernel, body_naifid)
    except spice.utils.exceptions.SpiceKERNELVARNOTFOUND:
        print(f'WARNING!')
        print(f'No spin axis info found for body: {body_name} in PCK from metakernel: {metakernel}')
        print(f'Make sure PCK has data for this body, or specify spin axis directly. (not yet implemented!)')
        print(f'Using default spin axis (24 period, aligned w/ ecliptic)')
        print()
        spin_axis = defaults.spin_axis

    # Generate the parameters used for standard PORB output. 
    out  = get_porb_params(body_name, body_naifid, orb_elems, spin_axis)

    return format_output(out, verbose=verbose)

if __name__ == '__main__':
    # Include headers in output?
    verbose = True

    body_names      = ['Ceres', 'Mars', 'Deimos', 'Didymos', 'Dimorphos', 'Chimaera']
    body_naifids    = [20000001, 499, 402, 920065803, 120065803, 20000623]

    # epoch at which to calculate orbital params (must be covered by available kernels)
    epoch_date = datetime.datetime(2024,11,1,0,0,0)
    metakernel = '/home/nsmith/KRC/pyorb/kernels/mk/krc_default.tm'
    
    for i in range(len(body_names)):
        print()
        print(main(body_names[i], body_naifids[i], metakernel, epoch_date, verbose=verbose))