#! /bin/bash/python

# handle combining porb output with other parameters to write hdf file for a body. 

import numpy as np
import h5py
import spiceypy as spice

import defaults 
import constants as const
import porb
from kernel_mgmt import kernels_dir

planet_params_file = '/home/nsmith/KRC/pyorb/src/planet_params.csv'

def write_hdf(bodyname:str):
    '''
    write a cacheable hdf for the specified body, containing PORB output, plus other 
    parameters used by various other davinci interface systems.
    '''
    metakernel = f'{kernels_dir}/mk/{bodyname.upper()}.tm'
    spice.furnsh(metakernel)
    naifid = spice.bods2c(bodyname)

    if (naifid < 1000) and (naifid%100 == 99):
        body_type = 'Planet'
    elif (naifid > 10) and (naifid < 100000):
        body_type = 'Satellite'
    elif (naifid >= 1000000) and (naifid < 2000000):
        body_type = 'Comet'
    elif (naifid >= 2000000) and (naifid < 1000000000):
        body_type = 'Minor'

    porb_output = porb.main(bodyname, naifid, metakernel, defaults.epoch_date)

    # TODO: Make this work for binary asteroids 
    parent_body = 0
    if body_type == 'Satellite':
        parent_number = int(str(naifid)[0])
        parents= ['', 'Mercury', 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto']
        parent_body = parents[parent_number]

    planet_flux = {
        'BT_Avg'        :   -999,   
        'BT_Max'        :   -999,    
        'BT_Min'        :   -999,    
        'Dis_AU'        :   -999,    
        'Geom_alb'      :   -999,
        'Mut_Period'    :   -999,
        'Orb_Radius'    :   -999,
        'Radius'        :   -999    
    }

    GRAV = 0

    planet_params = np.genfromtxt(planet_params_file, delimiter=',', names=True)

    if body_type in ['Planet', 'Satellite']:
        try:
            planet_flux['Radius'] = spice.bodvcd(naifid, 'RADII', 3)[0] # Selecting equatorial radius
        except spice.utils.exceptions.SpiceKERNELVARNOTFOUND:
            planet_flux['Radius'] = 100.0 # can probably be arbitrarily small, but non-zero
    
    if body_type == 'Satellite':
        semimajor_axis = porb.get_orbital_elements(metakernel, naifid, parent_body, defaults.epoch_date)
        if bodyname in planet_params:
            satellite_mass = planet_params['mass'][planet_params['Name']==bodyname]
        else: satellite_mass = 0.
        GRAV = const.G * satellite_mass / (1000*planet_flux['Radius'])**2
        planet_flux['Mut_Period'] = 2*np.pi * np.sqrt((1000*semimajor_axis)**3 / (const.G*(planet_params['mass'][planet_params['Name']==parent_body]+satellite_mass)))
        planet_flux['Orb_Radius'] = semimajor_axis
    
    if body_type == 'Planet':
        planet_flux['Dis_AU'] = porb_output['SJA']
        planet_flux['BT_Avg'] = planet_params['BT_Avg'][planet_params['Name']==bodyname]
        planet_flux['BT_Min'] = planet_params['BT_Min'][planet_params['Name']==bodyname]
        planet_flux['BT_Max'] = planet_params['BT_Max'][planet_params['Name']==bodyname]
        planet_flux['Geom_alb'] = planet_params['Geom_alb'][planet_params['Name']==bodyname]
        GRAV = const.G * planet_params['mass'] / (1000*planet_flux['Radius'])**2

    ARC2_G0 = -999
    DUSTA   = -999
    TAURAT  = -999
    PTOTAL  = 0

    if bodyname == 'Mars':
        ARC2_G0 = 0.5
        DUSTA   = 0.9
        TAURAT  = 0.22
    
    if bodyname in ['Venus', 'Earth', 'Mars', 'Pluto', 'Titan']:
        PTOTAL = planet_params['PTOTAL'][planet_params['Name']==bodyname]
    elif bodyname in ['Jupiter', 'Saturn', 'Uranus', 'Neptune']:
        PTOTAL = -999


    N24 = 96
    if porb_output['SIDAY']/N24 > 0.5:
        # factor of 3.8 means timesteps of about 16 minutes
        factor = 3.8 
        N24 = porb_output['SIDAY']*factor - (porb_output['SIDAY']*factor)%24

    '''
    HDF contents, mostly matching the format expected by the Davinci interface:

    rot:                string  formatted string containing table of KRC input parameters from PORB. (see porb.py)
    body:               string  Name of the specified body. Converted to all uppercase for consistency in parsing user inputs.
    period:             float   body's orbital period around the Sun, in Earth Days.
    rot_per:            float   body's sidereal rotation period, in hours.
    rot_per_flag:       int     1: indicates rotation period is using the default, 0: indicates a real rotation period

    type/
        body_type:      string  Planet, Satellite, Comet, or Minor
        id:             int     NAIFID of body. (previous davinci implementation was 0 for planets and satellites, NAIFID for comets, IAU number for asteroids.)
        name:           string  object name, same as top level record "body".
        parent_body:    string  parent body name, blank for anything orbiting the Sun.
    
    planet_flux/                Used by Davinci interface to calculate planetshine and eclipses 
        BT_Avg:         float   Average bolometric temperature in K (?). -999 for all non-planets.
        BT_Max:         float   Max bolometric temperature in K (?). -999 for all non-planets.
        BT_Min:         float   Min bolometric temperature in K (?). -999 for all non-planets.
        Dis_AU:         float   Semimajor axis of orbit around Sun, in AU. -999 for all non-planets.
        Geom_alb:       float   Geometric albedo. -999 for all non-planets.
        Mut_Period:     float   period of body's orbit around its parent, in Earth days. -999 for all non-satellites.
        Orb_Radius:     float   radius of body's orbit around its parent, in km. -999 for all non-satellites.
        Radius:         float   radius of body, in km. -999 for anything other than planets and satellites.

    krc/
        ARC2_G0:        float   unknown, probably an atmospheric property. Only defined for Mars. 
        DUSTA:          float   unknown, probably an atmospheric property. Only defined for Mars.
        TAURAT:         float   unknown, probably an atmospheric property. Only defined for Mars.

        PTOTAL:         float   Surface pressure of atmosphere in Pa. -999 for gas giants. 0 for airless bodies. (examples had 0.1 for asteroids and comets, 0 for airless moons)
        GRAV:           float   surface gravity in m/s^2. Not defined for Minor and Comet types. Ceres is 0, Deimos is 0.003.

        PERIOD:         float   Sidereal rotation period in Earth days. (redundant with top-level record "rot_per")
        DELJUL:         float   Orbit period / 360, in Earth Days. (basically redundant with top-level record "period")
        N24:            int     number of diurnal timesteps for KRC to use. Usually 96. Examples have larger values for some Jovian moons, possibly to keep each time step under ~30 minutes of real time for bodies with longer rotation periods.
            
    '''

    hdf_file = f'{bodyname.upper()}.params.hdf'

    with h5py.File(hdf_file, 'w') as f:
        f.create_dataset('rot',                     data=porb.format_output(porb_output))
        f.create_dataset('body',                    data=bodyname.upper())
        f.create_dataset('period',                  data=porb_output['OPERIOD'])
        f.create_dataset('rot_per',                 data=porb_output['SIDAY'])
        f.create_dataset('rot_per_flag',            data=int(porb_output['default_spin']))

        f.create_group('type')
        f.create_dataset('type/body_type',          data=body_type)
        f.create_dataset('type/id',                 data=naifid)    
        f.create_dataset('type/name',               data=bodyname.upper())
        f.create_dataset('type/parent_body',        data=parent_body)
        
        f.create_group('planet_flux')
        f.create_dataset('planet_flux/BT_Avg',      data=planet_flux['BT_Avg'])
        f.create_dataset('planet_flux/BT_Min',      data=planet_flux['BT_Min'])
        f.create_dataset('planet_flux/BT_Max',      data=planet_flux['BT_Max'])
        f.create_dataset('planet_flux/Dis_AU',      data=planet_flux['Dis_AU'])
        f.create_dataset('planet_flux/Geom_alb',    data=planet_flux['Geom_alb'])
        f.create_dataset('planet_flux/Mut_Period',  data=planet_flux['Mut_Period'])
        f.create_dataset('planet_flux/Orb_Radius',  data=planet_flux['Orb_Radius'])
        f.create_dataset('planet_flux/Radius',      data=planet_flux['Radius'])

        f.create_group('krc')
        f.create_dataset('krc/ARC2_G0',             data=ARC2_G0)
        f.create_dataset('krc/DUSTA',               data=DUSTA)
        f.create_dataset('krc/TAURAT',              data=TAURAT)
        f.create_dataset('krc/PTOTAL',              data=PTOTAL)
        f.create_dataset('krc/GRAV',                data=GRAV)
        f.create_dataset('krc/PERIOD',              data=porb_output['SIDAY']/24)
        f.create_dataset('krc/DELJUL',              data=porb_output['OPERIOD']/360)
        f.create_dataset('krc/N24',                 data=N24)


    return hdf_file