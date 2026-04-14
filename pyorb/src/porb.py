#! /bin/bash/python

# get rotation matrix for KRC input file.
# intended to replace PORB fortran stuff, specifically porbig.f
# returns BFRM, in the PORB/KRC nomenclature.

import numpy as np
import spiceypy as spice
import datetime
import constants as const
from kernel_mgmt import kernels_dir, get_mk
import defaults 
import h5py

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
    pole_ra  = spice.bodvcd(pck_naifid, 'POLE_RA',  3)[1][0]
    # ZBAA  : declination of spin axis in J2000 frame [radians]
    pole_dec = spice.bodvcd(pck_naifid, 'POLE_DEC', 3)[1][0]

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
    (rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag) = spin_axis 
    (obliquity, rotation_matrix_FtoB, true_anomaly_at_vernal_equinox) = get_secondary_spin_params(orb_elems, spin_axis)
    ### Note: theoretically, I think you could input the spin axis as obliquity and true 
    ### anomaly at vernal equinox, and derive pole_ra and pole_dec and the rotation matrix.
    ### this would potentially be more in line with how people think about objects with unknown spins.
    
    ##### record variables in output dictionary #####
    out={}
    out['default_spin']     = default_spin_flag
    out['porb_version']     = const.porb_version
    out['generation_date']  = datetime.datetime.now().strftime('%Y %b %d %H:%M:%S')
    out['NAME']             = body_name

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

def add_str_dset(string, group, label):
    '''
    add a single-string dataset to an hdf with all the particular formatting requirements. 
    '''
    dt = h5py.string_dtype(encoding='ascii',length=len(string)+1)
    dt_id = h5py.h5t.py_create(dt)
    dt_id.set_strpad(h5py.h5t.STR_NULLTERM)
    space = h5py.h5s.create_simple((1,))

    dset_id = h5py.h5d.create(group.id, label.encode('ascii'), dt_id, space)
    dset = h5py.Dataset(dset_id)
    dset[0] = string.encode('ascii')
    if label=='rot':
        dset.attrs.create('lines',7, dtype=np.dtype('>i4'))
    else:
        dset.attrs.create('lines',1, dtype=np.dtype('>i4'))
    
    return

def add_num_dset(value, group, label, d_type):
    '''
    add a single-value float/int dataset to an hdf with all the formatting weirdness.
    '''
    # ensure 32-bit float/int, big-endian. 
    # put in a 1x1x1 array. 
    # assign attr dv_std = 1, org=0, each 32-bit big-endian signed ints.
    # compress w/ deflate.

    # value_arr = np.array([[[value]]]).astype('>f')
    # print(f'value_arr: {value_arr}')
    # print(f'value_arr.shape: {value_arr.shape}')
    dset = group.create_dataset(label, (1,1,1), dtype=d_type, chunks=True, compression='gzip', compression_opts=6)
    dset[0,0,0] = value
    group[f'{label}'].attrs.create('dv_std', 1, dtype=np.dtype('>i4'))
    group[f'{label}'].attrs.create('org', 0, dtype=np.dtype('>i4'))

    return

def write_hdf(out: dict, out_dir: str, verbose=False):
    '''
    creates an output hdf 
    '''
    outfile = f'{out_dir}/{out["NAME"]}.porb.hdf'

    dt = h5py.string_dtype(encoding='ascii',length=len(out['NAME']))

    with h5py.File(outfile, 'w') as f:
        # dt = h5py.string_dtype(encoding='ascii',length=len(out['NAME']))
        # f['body']           = np.array([out['NAME'].encode('ascii')]).astype(dt)
        # f['body'].attrs.create('lines',1, dtype=np.dtype('>i4'))
        add_str_dset(out['NAME'], f, 'body')

        # f['period']         = np.array([[[out['OPERIOD']]]])
        add_num_dset(out['OPERIOD'], f, 'period', '>f')

        rot = format_output(out,verbose=False) #.replace('\n', '')
        add_str_dset(rot, f, 'rot')
        # dt = h5py.string_dtype(encoding='ascii',length=len(rot))
        # f['rot']            = np.array([rot.encode('ascii')]).astype(dt) 
        # f['rot'].attrs.create('lines',7, dtype=np.dtype('>i4'))

        # f['rot_per']        = np.array([[[out['SIDAY']]]])
        add_num_dset(out['SIDAY'], f, 'rot_per', '>f')
        
        # f['rot_per_flag']   = np.array([[[out['default_spin']]]])
        add_num_dset(out['default_spin'], f, 'rot_per_flag', '>i4')


        type_grp = f.create_group('type')
        krc_grp = f.create_group('krc')
        planet_flux_grp = f.create_group('planet_flux')

        print(f'Warning: using body_type=Minor. Atmospheres and moons not yet implemented.')
        body_type = 'Minor' ### hardcoding this for now. TODO: maybe fix? if we stick with this hdf format long term
        if body_type=='Minor':
            
            # dt = h5py.string_dtype(encoding='ascii',length=len(body_type)+1)
            # dt_id = h5py.h5t.py_create(dt)
            # dt_id.set_strpad(h5py.h5t.STR_NULLTERM)
            # space = h5py.h5s.create_simple((1,))

            # dset_id = h5py.h5d.create(type_grp.id, 'body_type'.encode('ascii'), dt_id, space)
            # body_type_dset = h5py.Dataset(dset_id)
            # body_type_dset[0] = body_type.encode('ascii')

            # type_grp['body_type'].attrs.create('lines',1, dtype=np.dtype('>i4'))

            # print(f'strpad: {type_grp['body_type'].id.get_type().get_strpad()}')

            add_str_dset(body_type, type_grp, 'body_type')

            # type_grp['id']          = np.array([out['PLANUM']])
            add_num_dset(out['PLANUM'], type_grp, 'id', '>i4')

            add_str_dset(out['NAME'], type_grp, 'name')
            # dt = h5py.string_dtype(encoding='ascii',length=len(out['NAME']))
            # type_grp['name']        = np.array([out['NAME'].encode('ascii')]).astype(dt)
            # type_grp['name'].attrs.create('lines',1, dtype=np.dtype('>i4'))

            add_str_dset('', type_grp, 'parent_body')
            # dt = h5py.string_dtype(encoding='ascii',length=1)
            # type_grp['parent_body'] = np.array([]).astype(dt)         
            # type_grp['parent_body'].attrs.create('lines',1, dtype=np.dtype('>i4'))

            add_num_dset(0.0, krc_grp, 'ARC2_G0', '>f')
            add_num_dset(out['OPERIOD']/360., krc_grp, 'DELJUL', '>f')          # Default DELJUL, orbit period / 360
            add_num_dset(0.0, krc_grp, 'DUSTA', '>f')
            add_num_dset(0.0, krc_grp, 'GRAV', '>f')
            add_num_dset(96, krc_grp, 'N24', '>i4')                             # Default number of "hour" divisions of a sol.
            add_num_dset(out['SIDAY']/24., krc_grp, 'PERIOD', '>f')             # rotation period in Earth days
            add_num_dset(0.0, krc_grp, 'PTOTAL', '>f')
            add_num_dset(0.0, krc_grp, 'TAUD', '>f')
            add_num_dset(0.0, krc_grp, 'TAURAT', '>f')
            add_num_dset(0.0, krc_grp, 'TFROST', '>f')

            # krc_grp['ARC2_G0']      = np.array([[[0]]])
            # krc_grp['DELJUL']       = np.array([[[out['OPERIOD']/360.]]])       # Default DELJUL, orbit period / 360
            # krc_grp['DUSTA']        = np.array([[[0]]])
            # krc_grp['GRAV']         = np.array([[[0]]])
            # krc_grp['N24']          = np.array([96])                        # Default number of "hour" divisions of a sol.
            # krc_grp['PERIOD']       = np.array([out['SIDAY']/24.])          # rotation period in Earth days
            # krc_grp['PTOTAL']       = np.array([[[0]]])
            # krc_grp['TAUD']         = np.array([[[0]]])
            # krc_grp['TAURAT']       = np.array([[[0]]])
            # krc_grp['TFROST']       = np.array([[[0]]])

            add_num_dset(-999, planet_flux_grp, 'BT_Avg', '>f')
            add_num_dset(-999, planet_flux_grp, 'BT_Max', '>f')
            add_num_dset(-999, planet_flux_grp, 'BT_Min', '>f')
            add_num_dset(-999, planet_flux_grp, 'Dis_AU', '>f')
            add_num_dset(-999, planet_flux_grp, 'Geom_alb', '>f')
            add_num_dset(-999, planet_flux_grp, 'Mut_Period', '>f')
            add_num_dset(-999, planet_flux_grp, 'Orb_Radius', '>f')
            add_num_dset(-999, planet_flux_grp, 'Radius', '>f')

            # planet_flux_grp['BT_Avg']       = np.array([[[-999]]])
            # planet_flux_grp['BT_Max']       = np.array([[[-999]]])
            # planet_flux_grp['BT_Min']       = np.array([[[-999]]])
            # planet_flux_grp['Dis_AU']       = np.array([[[-999]]])
            # planet_flux_grp['Geom_alb']     = np.array([[[-999]]])
            # planet_flux_grp['Mut_Period']   = np.array([[[-999]]])
            # planet_flux_grp['Orb_Radius']   = np.array([[[-999]]])
            # planet_flux_grp['Radius']       = np.array([[[-999]]])

    print(f'Wrote {outfile}')     

    return

def main(body_name, body_naifid, metakernel, epoch_date, verbose=True):
    '''
    Generate the standard PORB output for a specified body, at some epoch, using 
    SPICE kernels. Return the formatted output using PORB's standard FORTRAN style formatting.
    '''

    # Determine orbital elements for either the specified body, or, if the 
    # specified body is a satellite, its sun-orbiting parent.
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
    out  = get_porb_params(body_name, body_naifid, orb_elems, spin_axis)

    return out

if __name__ == '__main__':
    # Include headers in output?
    verbose = True

    # body_names      = [ 'Mars', 'Deimos', 'Ceres', 'Didymos', 'Dimorphos', 'Chimaera']
    # body_naifids    = [ 499, 402, 20000001, 920065803, 120065803, 20000623]

    body_names      = ['Justitia']
    body_naifids    = [20000269]

    # epoch at which to calculate orbital params (must be covered by available kernels)
    epoch_date = defaults.epoch_date
    # metakernel = f'{kernels_dir}/mk/krc_default.tm'
    
    for i in range(len(body_names)):
        print()
        # metakernel = get_mk(f'{body_names[i]}')
        metakernel = f'{kernels_dir}/mk/JUSTITIA.tm'
        out = main(body_names[i], body_naifids[i], metakernel, epoch_date, verbose=verbose)
        print(format_output(out))
        # write_hdf(out, '/home/nsmith/KRC/pyorb/test')
        write_hdf(out, defaults.porb_defaults_dir)


#### ./krc_justitia.dv /work/nsmith/justitia/krc/tmp/260327_justitia_1 00599