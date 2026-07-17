#! /bin/bash/python

import glob
import os.path as path
import numpy as np

from . import defaults 
from . import install
from . import constants as const
from . import porb
from . import body_params

def update_default_hdf(body_name:str) -> str:
    porb_params = porb.high_level_get_porb_params(body_name, update_kernels=True)
    hdf_file = body_params.high_level_write_hdf(porb_params)
    return hdf_file

def update_all_default_hdfs():
    default_hdfs = glob.glob(f'{install.porb_defaults_dir}/*.hdf')
    bodies = []
    for hdf in default_hdfs:
        body_name = path.basename(hdf).split('.')[0]
        update_default_hdf(body_name)
        bodies.append(body_name)
    print(f'Updated default hdfs in {install.porb_defaults_dir} for these bodies:')
    print(bodies)

    return

def get_and_modify_porb_params(body_name, 
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
            true_anomaly_at_vernal_equinox:float|None = None) -> porb.PorbParams:
    
    porb_params = porb.high_level_get_porb_params(body_name)
    modified_params = porb.modify_porb_params(porb_params,
        long_of_asc_node,
        eccentricity,
        inclination,
        arg_of_peri,
        semimajor_axis,
        orbit_period,
        perihelion_date,
        centuries_from_j2000,
        epoch_JD,
        mean_anomaly,
        rotation_period,         
        phase_at_j2000,
        pole_ra,
        pole_dec,   
        default_spin_flag,  
        obliquity,
        rotation_matrix_FtoB,
        true_anomaly_at_vernal_equinox)

    return modified_params