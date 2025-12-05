#! /bin/bash/python

# defines defaults used by pyorb when appropriate input is missing.

import numpy as np
import datetime

# default spin axis (24hr period, aligned w/ ecliptic)
'''
tuple of floats:
rotation_period:        rotation period [hours]
phase_at_j2000:         rotational phase (angle of prime meridian) at J2000 epoch [degrees]
pole_ra:                right ascension of spin axis in J2000 frame [radians]
pole_dec:               declination of spin axis in J2000 frame [radians]
'''    
rotation_period     = 24.
phase_at_j2000      = 0.
pole_ra             = 0.
pole_dec            = np.pi
default_spin_flag   = 1
spin_axis = (rotation_period, phase_at_j2000, pole_ra, pole_dec, default_spin_flag)

# default epoch at which to calculate orbit
epoch_date = datetime.datetime(2024,11,1,0,0,0)