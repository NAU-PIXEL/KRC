#! /bin/bash/python

# get rotation matrix for KRC input file.
# intended to replace PORB fortran stuff, specifically porbig.f
# returns BFRM, in the PORB/KRC nomenclature.

import numpy as np
import spiceypy as spice
import datetime

def get_orbit_params(body_name, body_naifid):
    '''
    Determines the orbital parameters of a body based on spice kernels.
    Outputs a dictionary containing all the variables to include in the standard PORB
    input table for KRC.
    '''

    out_dict={}

    # PORB version date:
    out_dict['porb_version'] = '2025nov20'
    # generation date:
    out_dict['generation_date'] = datetime.datetime.now().strftime('%Y %b %d %H:%M:%S')
    # epoch at which to calculate orbital params (must be covered by available kernels)
    epoch_date = datetime.datetime(2025,1,1,0,0,0)
    

    ##### Unused flags #####
    # SFLAG : Input system flag 0:default, +1: pole=ec +10:orb=eq
    out_dict['SFLAG']   = 0
    # PBUG  : Debug Flag
    out_dict['PBUG']    = 0
    # spar17: unused
    out_dict['spar17']  = 0
    # spar21: unused
    out_dict['spar21']  = 0

    ##### Constants #####
    #earth sidereal year in days 
    earth_year = 365.256363004
    # EOBL  : Earth's obliquity, in radians
    earth_obliquity = 0.409092601        # ecliptic obliquity 84381.406" in radians IAU2009
    # Full julian date of 2000.0
    j2000_JD = 2451545.0
    # 1 AU in km
    km_per_au = 149597870.6996262
    # solar mass parameter in km^3/s^2:
    mu = 1.32712440042 * 10**11

    ##### Spice stuff #####
    metakernel = '/home/nsmith/KRC/kernels/mk/krc_default.tm'
    spice.furnsh(metakernel)

    et = spice.datetime2et(epoch_date)
    epoch_JD = float(spice.et2utc(et,'J', 6).split(' ')[-1])

    if body_naifid > 10000:
        body_spk_num = body_naifid - 2000000 + 20000000 #JPL horizons spks use 20 million offset instead of 2 million. what the hell??
    else: body_spk_num = body_naifid

    handle, descr, ident = spice.spksfs(body_spk_num, et, 40) 
    dc, ic = spice.dafus(descr, 2, 6)
    center_id = ic[1]

    if center_id in (0, 10):
        # body orbits sun
        orbital_id = body_spk_num
    else:
        # body does not orbit the sun
        orbital_id = center_id

    # comments = spice.dafec(handle,100, 100)[1]
    # # is SPK source JPL Horizons?
    # from_horizons = 'Horizons On-Line Ephemeris System' in '\t'.join(comments)

    # get the state vector of body (or its system barycenter) relative to sun.
    state_vector = spice.spkezr(str(orbital_id), et, 'ECLIPJ2000', 'NONE', 'SUN')[0]

    # get orbital elements of body relative to sun
    elts = spice.oscelt(state_vector, et, mu)

    long_of_asc_node    = elts[3]
    eccentricity        = elts[1]
    inclination         = elts[2]
    arg_of_peri         = elts[4]
    mean_anomaly        = elts[5]
    semimajor_axis      = elts[0] / (1-eccentricity) / km_per_au

    body_pm = spice.bodvcd(body_naifid, 'PM', 3)[1]
    phase_at_j2000  = body_pm[0]
    rotation_rate   = body_pm[1]

    rotation_period = (360.*24)/rotation_rate

    body_frame_name = spice.cidfrm(body_naifid, 33)[1]
    pole_axis_body = [0, 0, 1]
    pole_axis_j2000 = np.matmul(spice.pxform(body_frame_name, 'J2000', et), pole_axis_body)

    _, pole_ra, pole_dec = spice.recrad(pole_axis_j2000)

    #### Finished with spice kernels. ####

    ##### Calculated from inputs #####
    # PERIOD, OPERIOD   : Period of the orbit (days)
    orbit_period = semimajor_axis**(1.5) * earth_year
    # TJP   : J2000 Date of previous perihelion
    perihelion_date = epoch_JD - (mean_anomaly/360.)*orbit_period - j2000_JD
    # TC    : time in centuries from reference date (2000.0)
    centuries_from_j2000 = (epoch_JD - j2000_JD) / (earth_year*100)

    # AFRM  : rotation matrix from orbital (F) to J2000 (A)
    ## First do rotation from orbital (F) to ecliptic (E)
    #### the 3 Euler rotations required are:
    #### A = (-node)Z * (-inclination)X * (-argument of periapsis)Z
    rotation_matrix_FtoE = spice.eul2m(-1*long_of_asc_node, -1*inclination, -1*arg_of_peri, 3, 1, 3)
    ## add rotation from ecliptic to A frame, equatorial (J2000)
    rotation_matrix_FtoA = spice.rotmat(rotation_matrix_FtoE, -1*earth_obliquity, 1)

    # ZFAXU : orbit pole, Z unit vector, in J2000 (a 3-vector)
    orbit_Z_axis_j2000 = rotation_matrix_FtoA[:,2].copy()
    # ZBAXU : spin axis unit vector in J2000
    spin_axis_j2000 = spice.latrec(1, pole_ra, pole_dec)
    # BLIP  : angle between spin axis and orbit pole, == obliquity
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
    # TAV   : True anomaly at vernal equinox
    true_anomaly_at_vernal_equinox = np.arctan2(vernal_equinox_orbital[1], vernal_equinox_orbital[0])


    ##### record variables in output dictionary #####
    out_dict['NAME']        = body_name
    out_dict['PLANUM']      = body_naifid
    
    out_dict['TC']          = centuries_from_j2000
    out_dict['RODE']        = long_of_asc_node
    out_dict['CLIN']        = inclination
    out_dict['ARGP']        = arg_of_peri
    
    out_dict['XECC']        = eccentricity
    out_dict['SJA']         = semimajor_axis
    out_dict['EOBL']        = earth_obliquity
    out_dict['ZBAA']        = pole_dec

    out_dict['ZBAB']        = pole_ra
    out_dict['WDOT']        = rotation_rate
    out_dict['WO']          = phase_at_j2000
    out_dict['OPERIOD']     = orbit_period
    out_dict['TJP']         = perihelion_date

    out_dict['SIDAY']       = rotation_period
    out_dict['TAV']         = true_anomaly_at_vernal_equinox
    out_dict['BLIP']        = obliquity

    out_dict['BFRM 1']      = rotation_matrix_FtoB[0][0]
    out_dict['BFRM 2']      = rotation_matrix_FtoB[1][0]
    out_dict['BFRM 3']      = rotation_matrix_FtoB[2][0]
    out_dict['BFRM 4']      = rotation_matrix_FtoB[0][1]
    
    out_dict['BFRM 5']      = rotation_matrix_FtoB[1][1]
    out_dict['BFRM 6']      = rotation_matrix_FtoB[2][1]
    out_dict['BFRM 7']      = rotation_matrix_FtoB[0][2]
    out_dict['BFRM 8']      = rotation_matrix_FtoB[1][2]
    out_dict['BFRM 9']      = rotation_matrix_FtoB[2][2]
    
    return out_dict

def format_output(out_dict: dict, verbose=False):
    '''
    Formats variables stored in out_dict into a the Fortran style PORB output.
    Outputs a multiline string. 
    Variable labels are optionally included using the verbose flag. 
    '''
    out_str = ''
    if verbose:
        out_str += f'<--VERSION---> <--generation date->           IPLAN      TC orbit:pole\n'
        out_str += f'PORB:{out_dict['porb_version']} {out_dict['generation_date']} IPLAN,TC= {out_dict['PLANUM']:5.4g} {out_dict['TC']:7.5g} {out_dict['NAME']}:{out_dict['NAME']}\n'
        out_str += f'     PLANUM             Tc           RODE           CLIN           ARGP\n'
        out_str += f' {out_dict['PLANUM']:10.7g}     {out_dict['TC']:10.7g}     {out_dict['RODE']:10.7g}      {out_dict['CLIN']:.7E} {out_dict['ARGP']:10.7f}\n'
        out_str += f'       XECC            SJA           EOBL          SFLAG           ZBAA\n'
        out_str += f'  {out_dict['XECC']:.7E} {out_dict['SJA']:10.7g}     {out_dict['EOBL']:10.7g}     {out_dict['SFLAG']:10.7g}     {out_dict['ZBAA']:10.7g}\n'
        out_str += f'       ZBAB           WDOT             WO        OPERIOD            TJP\n'
        out_str += f' {out_dict['ZBAB']:10.7g}     {out_dict['WDOT']:10.7g}     {out_dict['WO']:10.7g}     {out_dict['OPERIOD']:10.7g}     {out_dict['TJP']:10.7g}\n'
        out_str += f'      SIDAY          spare            TAV           BLIP           PBUG\n'
        out_str += f' {out_dict['SIDAY']:10.7g}     {out_dict['spar17']:10.7g}     {out_dict['TAV']:10.7g}     {out_dict['BLIP']:10.7g}     {out_dict['PBUG']:10.7g}\n'
        out_str += f'      spare         BFRM 1              2              3              4\n'
        out_str += f' {out_dict['spar21']:10.7g}     {out_dict['BFRM 1']:10.7g}     {out_dict['BFRM 2']:10.7g}     {out_dict['BFRM 3']:10.7g}     {out_dict['BFRM 4']:10.7g}\n'
        out_str += f'          5              6              7              8         BFRM 9\n'
        out_str += f' {out_dict['BFRM 5']:10.7g}     {out_dict['BFRM 6']:10.7g}     {out_dict['BFRM 7']:10.7g}     {out_dict['BFRM 8']:10.7g}     {out_dict['BFRM 9']:10.7g}\n'

    else:
        out_str += f'PORB:{out_dict['porb_version']} {out_dict['generation_date']} IPLAN,TC= {out_dict['PLANUM']:5.4g} {out_dict['TC']:7.5g} {out_dict['NAME']}:{out_dict['NAME']}\n'
        out_str += f' {out_dict['PLANUM']:10.7g}     {out_dict['TC']:10.7g}     {out_dict['RODE']:10.7g}      {out_dict['CLIN']:.7E} {out_dict['ARGP']:10.7f}\n'
        out_str += f'  {out_dict['XECC']:.7E} {out_dict['SJA']:10.7g}     {out_dict['EOBL']:10.7g}     {out_dict['SFLAG']:10.7g}     {out_dict['ZBAA']:10.7g}\n'
        out_str += f' {out_dict['ZBAB']:10.7g}     {out_dict['WDOT']:10.7g}     {out_dict['WO']:10.7g}     {out_dict['OPERIOD']:10.7g}     {out_dict['TJP']:10.7g}\n'
        out_str += f' {out_dict['SIDAY']:10.7g}     {out_dict['spar17']:10.7g}     {out_dict['TAV']:10.7g}     {out_dict['BLIP']:10.7g}     {out_dict['PBUG']:10.7g}\n'
        out_str += f' {out_dict['spar21']:10.7g}     {out_dict['BFRM 1']:10.7g}     {out_dict['BFRM 2']:10.7g}     {out_dict['BFRM 3']:10.7g}     {out_dict['BFRM 4']:10.7g}\n'
        out_str += f' {out_dict['BFRM 5']:10.7g}     {out_dict['BFRM 6']:10.7g}     {out_dict['BFRM 7']:10.7g}     {out_dict['BFRM 8']:10.7g}     {out_dict['BFRM 9']:10.7g}\n'

    return out_str

def main():
    # Include headers in output?
    verbose = True

    body_names      = ['Ceres', 'Mars', 'Deimos'] #, 'Chimaera')
    body_naifids    = [2000001, 499, 402] #, 20000623)
    
    for i in range(len(body_names)):
        body_name = body_names[i]
        body_naifid = body_naifids[i]

        out_dict=get_orbit_params(body_name, body_naifid)

        print()
        print(format_output(out_dict), verbose=verbose)

    return

if __name__ == '__main__':
    main()