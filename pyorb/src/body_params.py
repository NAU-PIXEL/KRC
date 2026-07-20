#! /bin/bash/python

# handle combining porb output with other parameters to write hdf file for a body. 

import numpy as np
import h5py
import spiceypy as spice
from typing import TypedDict

from . import defaults 
from . import install
from . import constants as const
from . import porb


planet_params_file = install.planet_params_file

def add_str_dset(string:str, group: h5py.Group, label:str):
    """
    Add a single-string dataset to an hdf with all the particular formatting requirements
    expected by ASU's Davinci. 

    (NOTE: I would love nothing more than to abandon Davinci compatibility and use
        standard h5py functions and default behavior for adding datasets.)

    Args:
        string (str): The string to add as the value of the dataset.
        group (h5py.Group): What h5py group to add the dataset to.
        label (str): The name of the dataset to add.
    """
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

def add_num_dset(value:float | int, group: h5py.Group, label:str, d_type:np.dtype|str):
    """
    Add a single-value float/int dataset to an hdf with all the particular formatting 
    requirements expected by ASU's Davinci. 

    Args:
        value (float | int): numeric value to add as the value of the dataset.
        group (h5py.Group): What h5py group to add the dataset to.
        label (str): The name of the dataset to add.
        d_type (np.dtype | str): datatype of the dataset.
    """
    # ensure 32-bit float/int, big-endian. 
    # put in a 1x1x1 array. 
    # assign attr dv_std = 1, org=0, each 32-bit big-endian signed ints.
    # compress w/ deflate.

    dset = group.create_dataset(label, (1,1,1), dtype=d_type, chunks=True, compression='gzip', compression_opts=6)
    dset[0,0,0] = value
    group[f'{label}'].attrs.create('dv_std', 1, dtype=np.dtype('>i4'))
    group[f'{label}'].attrs.create('org', 0, dtype=np.dtype('>i4'))

    return

# Three typed dictionaries, corresponding to the groups expected in the davinci PORB hdf format.
class type_params_dict(TypedDict):
    body_name   :   str
    body_type   :   str
    naifid      :   int
    parent_body :   str

class planet_flux_dict(TypedDict):
    BT_Avg      :   float   
    BT_Max      :   float    
    BT_Min      :   float    
    Dis_AU      :   float    
    Geom_alb    :   float
    Mut_Period  :   float
    Orb_Radius  :   float
    Radius      :   float 

class krc_params_dict(TypedDict):
    ARC2_G0     :   float
    DUSTA       :   float
    TAURAT      :   float
    PTOTAL      :   float
    GRAV        :   float
    PERIOD      :   float
    DELJUL      :   float
    N24         :   int

def get_radius(naifid:int, metakernel:str|None = None) -> float:
    """
    Returns the radius of an object, specified by its naifid. 
    Uses the metakernel supplied, or selects an appropriate one from the kernel cache.
    If no Radius information is available in the chosen kernels, uses the default radius.

    Args:
        naifid (int): NAIF object ID code for the object of interest.
        metakernel (str | None, optional): Full path to the metakernel for the object. 
            Defaults to None.

    Returns:
        float: Radius of the object of interest [km]
    """
    radius = defaults.radius 
    if metakernel is None:
        metakernel = f'{install.kernels_dir}/mk/{naifid:09d}.tm'
    with spice.KernelPool(metakernel):
        try:
            radius = spice.bodvcd(naifid, 'RADII', 3)[1][0] # Selecting equatorial radius
        except spice.utils.exceptions.SpiceKERNELVARNOTFOUND:
            # Using default Radius.
            pass
    return radius

def get_body_params(porb_output:porb.PorbParams, 
                    metakernel:str|None = None) -> tuple[type_params_dict, planet_flux_dict, krc_params_dict]:
    """
    derive parameters (or extract them from the planetary parameters csv file) for 
    writing a porb hdf.

    Args:
        porb_output (porb.PorbParams): PorbParams object containing the orbit and spin 
            parameters for the object of interest.
        metakernel (str | None, optional): Full path to the metakernel for the object. 
            Defaults to None.

    Returns:
        tuple[type_params_dict, planet_flux_dict, krc_params_dict]: a tuple of three dicts, 
            corresponding to the three groups with the davinci PORB hdf format.
    """
    type_params = {
        'body_name'     :   porb_output.NAME,
        'body_type'     :   porb_output.body_type,
        'naifid'        :   porb_output.PLANUM,
        'parent_body'   :   ''
    }

    planet_flux = {
        'BT_Avg'        :   -999.,   
        'BT_Max'        :   -999.,    
        'BT_Min'        :   -999.,    
        'Dis_AU'        :   -999.,    
        'Geom_alb'      :   -999.,
        'Mut_Period'    :   -999.,
        'Orb_Radius'    :   -999.,
        'Radius'        :   -999.    
    }

    krc_params = {
        'ARC2_G0'       : -999.,
        'DUSTA'         : -999.,
        'TAURAT'        : -999.,
        'PTOTAL'        : 0.,
        'GRAV'          : 0.,
        'PERIOD'        : porb_output.SIDAY/24.,
        'DELJUL'        : porb_output.OPERIOD/360.,
        'N24'           : 96
    }

    # TODO: Make this work for binary asteroids 
    if type_params['body_type'] == 'Satellite':
        parent_number = int(str(type_params['naifid'])[0])
        parents = ['', 'Mercury', 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto']
        type_params['parent_body'] = parents[parent_number]

    planet_params = np.genfromtxt(planet_params_file, delimiter=',', names=True, encoding='utf-8',
                                  dtype=['U16',float,float,float,float,float,float])

    if type_params['body_type'] in ['Planet', 'Satellite']:
        planet_flux['Radius'] = get_radius(type_params['naifid'], metakernel)

    if type_params['body_type'] == 'Satellite':
        semimajor_axis = porb_output.SJA
        if type_params['body_name'] in planet_params['Name']:
            satellite_mass = planet_params['mass'][planet_params['Name']==type_params['body_name']]
        else: satellite_mass = 0.
        krc_params['GRAV'] = const.G * satellite_mass / (1000*planet_flux['Radius'])**2
        planet_flux['Mut_Period'] = 2*np.pi * np.sqrt((1000*semimajor_axis)**3 / (const.G*(planet_params['mass'][planet_params['Name']==type_params['parent_body']][0]+satellite_mass)))
        planet_flux['Orb_Radius'] = semimajor_axis
    
    if type_params['body_type'] == 'Planet':
        planet_flux['Dis_AU'] = porb_output.SJA
        planet_flux['BT_Avg'] = planet_params['BT_Avg'][planet_params['Name']==type_params['body_name']][0]
        planet_flux['BT_Min'] = planet_params['BT_Min'][planet_params['Name']==type_params['body_name']][0]
        planet_flux['BT_Max'] = planet_params['BT_Max'][planet_params['Name']==type_params['body_name']][0]
        planet_flux['Geom_alb'] = planet_params['Geom_alb'][planet_params['Name']==type_params['body_name']][0]
        krc_params['GRAV'] = const.G * planet_params['mass'][planet_params['Name']==type_params['body_name']][0] / (1000*planet_flux['Radius'])**2

    if type_params['body_name'] == 'Mars':
        krc_params['ARC2_G0'] = 0.5
        krc_params['DUSTA']   = 0.9
        krc_params['TAURAT']  = 0.22
    
    if type_params['body_name'] in ['Venus', 'Earth', 'Mars', 'Pluto', 'Titan']:
        krc_params['PTOTAL'] = planet_params['PTOTAL'][planet_params['Name']==type_params['body_name']][0]
    elif type_params['body_name'] in ['Jupiter', 'Saturn', 'Uranus', 'Neptune']:
        krc_params['PTOTAL'] = -999.

    if porb_output.SIDAY/krc_params['N24'] > 0.5:
        # if the default N24 produces timesteps that are longer than half an hour (ie, if siday > 48hrs)
        # factor of 3.8 means timesteps of about 16 minutes
        # the additional term ensures N24 is a multiple of 24. 
        # so this will produce timesteps between 16 and 30 minutes, approaching the lower bound as siday grows larger.
        factor = 3.8 
        krc_params['N24'] = int(porb_output.SIDAY*factor - (porb_output.SIDAY*factor)%24)
    
    return (type_params, planet_flux, krc_params)

def write_hdf(porb_output:porb.PorbParams, body_params:tuple, out_dir: str) -> str:
    """
    write a cacheable hdf for the specified body, containing PORB output, plus other 
    parameters used by various other davinci interface systems.

    HDF contents, matching the format expected by the Davinci interface:

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
        DELJUL:         float   Default DELJUL for KRC to use. Orbit period / 360, in Earth Days. (basically redundant with top-level record "period")
        N24:            int     Default number of diurnal timesteps for KRC to use. Usually 96. Examples have larger values for some Jovian moons, possibly to keep each time step under ~30 minutes of real time for bodies with longer rotation periods.

    Args:
        porb_output (porb.PorbParams): PorbParams object containing the orbit and spin 
            parameters for the object of interest.
        body_params (tuple): Tuple of additional parameters to include in the HDF.
        out_dir (str): Directory into which to write the HDF file. 

    Returns:
        str: Full path to the newly-written HDF file.
    """
    type_params, planet_flux, krc_params = body_params

    rot = str(porb_output) 

    # hdf_file = f'{type_params['body_name'].upper()}.params.hdf'
    hdf_file = f'{out_dir}/{porb_output.NAME}.porb.hdf'

    with h5py.File(hdf_file, 'w') as f:

        add_str_dset(porb_output.NAME,           f, 'body')
        add_num_dset(porb_output.OPERIOD,        f, 'period', '>f')
        add_str_dset(rot,                           f, 'rot')
        add_num_dset(porb_output.SIDAY,          f, 'rot_per', '>f')
        add_num_dset(porb_output.default_spin,   f, 'rot_per_flag', '>i4')

        type_grp        = f.create_group('type')
        krc_grp         = f.create_group('krc')
        planet_flux_grp = f.create_group('planet_flux')

        add_str_dset(type_params['body_type'],    type_grp, 'body_type')
        add_num_dset(type_params['naifid'],       type_grp, 'id', '>i4')
        add_str_dset(type_params['body_name'],    type_grp, 'name')
        add_str_dset(type_params['parent_body'],  type_grp, 'parent_body')

        add_num_dset(krc_params['ARC2_G0'],       krc_grp, 'ARC2_G0', '>f')
        add_num_dset(krc_params['DELJUL'],        krc_grp, 'DELJUL', '>f')  
        add_num_dset(krc_params['DUSTA'],         krc_grp, 'DUSTA', '>f')
        add_num_dset(krc_params['GRAV'],          krc_grp, 'GRAV', '>f')
        add_num_dset(krc_params['N24'],           krc_grp, 'N24', '>i4')    
        add_num_dset(krc_params['PERIOD'],        krc_grp, 'PERIOD', '>f')  
        add_num_dset(krc_params['PTOTAL'],        krc_grp, 'PTOTAL', '>f')
        add_num_dset(krc_params['TAURAT'],        krc_grp, 'TAURAT', '>f')
    
        add_num_dset(planet_flux['BT_Avg'],     planet_flux_grp, 'BT_Avg', '>f')
        add_num_dset(planet_flux['BT_Max'],     planet_flux_grp, 'BT_Max', '>f')
        add_num_dset(planet_flux['BT_Min'],     planet_flux_grp, 'BT_Min', '>f')
        add_num_dset(planet_flux['Dis_AU'],     planet_flux_grp, 'Dis_AU', '>f')
        add_num_dset(planet_flux['Geom_alb'],   planet_flux_grp, 'Geom_alb', '>f')
        add_num_dset(planet_flux['Mut_Period'], planet_flux_grp, 'Mut_Period', '>f')
        add_num_dset(planet_flux['Orb_Radius'], planet_flux_grp, 'Orb_Radius', '>f')
        add_num_dset(planet_flux['Radius'],     planet_flux_grp, 'Radius', '>f')

    return hdf_file

def read_hdf(hdf_file:str) -> tuple[type_params_dict, planet_flux_dict, krc_params_dict, porb.PorbParams]:
    """
    Read an HDF file in the davinci PORB hdf format. 
    Unpack the 1x1x1 arrays used for scalars (because of davinci compatibility).
    Decode any bytestrings into standard python strings.

    Args:
        hdf_file (str): Full path to the HDF file to unpack.

    Returns:
        tuple[type_params_dict, planet_flux_dict, krc_params_dict, porb.PorbParams]: 
            A tuple of three dicts and a PorbParams object, corresponding to the contents of the HDF.
    """
    with h5py.File(hdf_file, 'r') as f:
        porb_params = porb.PorbParams.from_str(f['rot'][0].decode())
        porb_params.default_spin = int(f['rot_per_flag'][0,0,0])
        porb_params.body_type = f['type/body_type'][0].decode()

        type_params = {'body_type':   f['type/body_type'][0].decode(),
                       'naifid':      f['type/id'][0,0,0],
                       'body_name':   f['type/name'][0].decode(),
                       'parent_body': f['type/parent_body'][0].decode()}
        
        planet_flux = dict(f['planet_flux'].items())
        krc_params = dict(f['krc'].items())
        for key in planet_flux.keys():
            planet_flux[key] = planet_flux[key][0,0,0]
        for key in krc_params.keys():
            krc_params[key] = krc_params[key][0,0,0]

    return (type_params, planet_flux, krc_params, porb_params)

def high_level_write_hdf(porb_output:porb.PorbParams, out_dir:str=install.porb_defaults_dir) -> str:
    """
    Write an HDF file corresponding to a given PorbParams object, to some given directory.
    This high-level function will automatically get the additional body parameters needed
    to match the Davinci PORB hdf format. 

    Args:
        porb_output (porb.PorbParams): PorbParams object containing the orbit and spin 
            parameters for the object of interest.
        out_dir (str, optional): Directory into which to write the HDF file. 
            Defaults to install.porb_defaults_dir.

    Returns:
        str: Full path to the newly-written HDF file. 
    """
    body_params = get_body_params(porb_output)
    hdf_file = write_hdf(porb_output, body_params, out_dir)
    
    return hdf_file