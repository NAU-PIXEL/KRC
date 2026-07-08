import src.defaults as defaults
import src.constants as const
import src.porb as porb
import src.kernel_mgmt as km
import src.install as install
import pytest
import numpy as np
import spiceypy as spice
import os
import shutil


def get_mars_porb_params():
    out=porb.PorbParams(
        default_spin        = 1,
        porb_version        = '2000jan01',
        generation_date     = '2000 Jan 01 00:00:00',
        NAME                = 'Mars',
        body_type           = 'Planet',

        PLANUM              = 499,

        TC                  = 0.0,
        RODE                = 0.8644665,
        CLIN                = 0.3226901E-01,
        ARGP                = -1.281586,

        XECC                = 0.9340198E-01,
        SJA                 = 1.523712,
        EOBL                = 0.4090926,
        SFLAG               = 0.000000,
        ZBAA                = 0.9229373,

        ZBAB                = 5.544402,
        WDOT                = 350.8920,
        WO                  = 176.0499,
        OPERIOD             = 686.9928,
        TJP                 = 3397.977,

        SIDAY               = 24.62296,
        spar17              = 0.000000,
        TAV                 = -1.240317,
        BLIP                = 0.4397026,
        PBUG                = 0.000000,

        spar21              = 0.000000,
        BFRM = np.array([[ 0.3244966,  -0.9458869,  0.000000 ],
                         [ 0.8559125,   0.2936299, -0.4256704],
                         [ 0.4026360,   0.1381286,  0.9048783]])
        )
    
    return out

def get_europa_porb_params():
    out=porb.PorbParams(
        default_spin        = 1,
        porb_version        = '2000jan01',
        generation_date     = '2000 Jan 01 00:00:00',
        NAME                = 'Europa',
        body_type           = 'Satellite',

        PLANUM              = 502,

        TC                  = 0.0,
        RODE                = 1.753958,
        CLIN                = 0.2276282E-01,
        ARGP                = -1.496526,

        XECC                = 0.4837299E-01,
        SJA                 = 5.202875,
        EOBL                = 0.4090926,
        SFLAG               = 0.000000,
        ZBAA                = 1.125917,

        ZBAB                = 4.678863,
        WDOT                = 0.000000,
        WO                  = 0.000000,
        OPERIOD             = 4334.739,
        TJP                 = -238.1847,

        SIDAY               = 85.22835,
        spar17              = 0.000000,
        TAV                 = 2.136860,
        BLIP                = 0.5414684E-01,
        PBUG                = 0.000000,

        spar21              = 0.000000,
        BFRM = np.array([[ -0.5363136,      0.8440188,      0.000000     ],
                         [ -0.8427818,     -0.5355276,     -0.5412039E-01],
                         [ -0.4567862E-01, -0.2902550E-01,  0.9985344    ]])
        )

    
    return out

def get_mars_orb_params():
    out = porb.OrbParams(
        long_of_asc_node    = 0.8644665,
        eccentricity        = 0.9340198E-01,
        inclination         = 0.3226901E-01,
        arg_of_peri         = -1.281586,
        mean_anomaly        = (3397.977 / 686.9928) * -2*np.pi,
        semimajor_axis      = 1.523712,
        epoch_JD            = const.j2000_JD,
        orbit_period        = 686.9928, 
        perihelion_date     = 3397.977, 
        centuries_from_j2000= 0.0
    )

    return out

def get_mars_spin_params():
    out = porb.SpinParams(
        rotation_period     = (360.*24.)/350.8920,
        phase_at_j2000      = 176.0499,
        pole_ra             = 5.544402,
        pole_dec            = 0.9229373,
        default_spin_flag   = 1,
        obliquity           = 0.4397026,
        rotation_matrix_FtoB= np.array([[ 0.3244966,  -0.9458869,  0.000000 ],
                         [ 0.8559125,   0.2936299, -0.4256704],
                         [ 0.4026360,   0.1381286,  0.9048783]]),
        true_anomaly_at_vernal_equinox = -1.240317
    )

    return out

mars_porb = get_mars_porb_params()
mars_porb_copy = get_mars_porb_params()
europa_porb = get_europa_porb_params()

mars_orb =  get_mars_orb_params()

mars_spin = get_mars_spin_params()


#### OrbParams class tests ####
def test_OrbParams_equality():
    test1 = get_mars_orb_params()
    test2 = get_mars_orb_params()

    assert test1 == test2

def test_OrbParams_from_porb_params():
    from_porb = porb.OrbParams.from_porb_params(mars_porb)

    assert from_porb == mars_orb

def test_OrbParams_from_orb_elems_tuple():
    orb_elems = (0.8644665, 0.9340198E-01, 0.3226901E-01, -1.281586, (3397.977 / 686.9928) * -2*np.pi, 1.523712, const.j2000_JD)
    from_orb_elems_tuple = porb.OrbParams.from_elems(orb_elems)

    assert from_orb_elems_tuple == mars_orb

def test_OrbParams_from_elems_and_second_params():
    orb_elems = (0.8644665, 0.9340198E-01, 0.3226901E-01, -1.281586, 3397.977 / 686.9928 * -2*np.pi, 1.523712, const.j2000_JD)
    second_params = (686.9928, 3397.977, 0.0)

    from_elems_and_second = porb.OrbParams.from_elems_and_second_params(orb_elems, second_params)

    assert from_elems_and_second == mars_orb

def test_OrbParams_from_modified_params():
    # Only one of semimajor_axis and orbit_period may be specified.
    with pytest.raises(ValueError):
        from_modified = porb.OrbParams.from_modified_params(mars_porb, semimajor_axis=1., orbit_period=1.)
    
    # Only one of epoch_JD and centuries_from_j2000 may be specified.
    with pytest.raises(ValueError):
        from_modified = porb.OrbParams.from_modified_params(mars_porb, epoch_JD=1., centuries_from_j2000=1.)
    
    # Only one of perihelion_date and mean_anomaly may be specified.
    with pytest.raises(ValueError):
        from_modified = porb.OrbParams.from_modified_params(mars_porb, perihelion_date=1., mean_anomaly=1.)
    
    # correct orbit_period, centuries_from_J2000, and perihelion_date:
    test_porb = get_mars_porb_params()
    test_porb.OPERIOD   = 1.
    test_porb.TC        = 2.
    test_porb.TJP       = 3.

    assert porb.OrbParams.from_porb_params(test_porb) != mars_orb

    from_modified = porb.OrbParams.from_modified_params(test_porb, semimajor_axis=1.523712, epoch_JD=const.j2000_JD, mean_anomaly=3397.977 / 686.9928 * -2*np.pi)

    assert from_modified == mars_orb

    # correct semimajor_axis, epoch_JD, and mean_anomaly:
    test_porb = get_mars_porb_params()
    test_porb.SJA       = 1.
    test_porb.TC        = 2.
    test_porb.TJP       = 3.

    assert porb.OrbParams.from_porb_params(test_porb) != mars_orb

    from_modified = porb.OrbParams.from_modified_params(test_porb, orbit_period=686.9928, centuries_from_j2000=0.0, perihelion_date=3397.977)

    assert from_modified == mars_orb

    # can modify all params:
    from_modified = porb.OrbParams.from_modified_params(test_porb, 
                                                        long_of_asc_node    = 1.,
                                                        eccentricity        = 2.,
                                                        inclination         = 3.,
                                                        arg_of_peri         = 4.,
                                                        orbit_period        = 5., 
                                                        perihelion_date     = 6., 
                                                        centuries_from_j2000= 7.
                                                    )
    
    desired_orb = mars_orb
    desired_orb.long_of_asc_node = 1.
    desired_orb.eccentricity     = 2.
    desired_orb.inclination      = 3.
    desired_orb.arg_of_peri      = 4.
    desired_orb.orbit_period     = 5.
    desired_orb.perihelion_date  = 6.
    desired_orb.centuries_from_j2000= 7.
    desired_orb.semimajor_axis = (5./ const.earth_year)**(2./3.)
    desired_orb.epoch_JD = 7.*const.earth_year*100 + const.j2000_JD
    desired_orb.mean_anomaly = ((6. - desired_orb.epoch_JD + const.j2000_JD) / desired_orb.orbit_period) * (-2*np.pi)

    assert from_modified == desired_orb

#### SpinParams class tests ####
def test_SpinParams_init():
    assert isinstance(mars_spin, porb.SpinParams)

def test_SpinParams_equality():
    copy = get_mars_spin_params()

    assert copy == mars_spin

    copy.pole_ra = 0.0

    assert copy != mars_spin

def test_SpinParams_from_spin_axis():
    spin_axis = (mars_spin.rotation_period, mars_spin.phase_at_j2000, mars_spin.pole_ra, mars_spin.pole_dec, mars_spin.default_spin_flag) 

    from_spin_axis = porb.SpinParams.from_spin_axis(spin_axis, mars_orb)
    assert from_spin_axis == mars_spin

def test_SpinParams_from_porb_params():
    from_porb = porb.SpinParams.from_porb_params(mars_porb)

    assert from_porb == mars_spin

def test_SpinParams_set_obliq_and_true_anomaly():
    copy = mars_spin.set_obliq_and_true_anomaly(0.0, 0.0, mars_orb)

    copy2 = get_mars_spin_params()
    copy2.obliquity = 0.0
    copy2.true_anomaly_at_vernal_equinox = 0.0
    copy2.pole_ra = -1.5119741510431786
    copy2.pole_dec = 1.1400991060229944
    copy2.rotation_matrix_FtoB = np.array([
            [ 1.00000000e+00,  0.00000000e+00,  0.00000000e+00],
            [ 0.00000000e+00,  1.00000000e+00, -5.55111512e-17],
            [ 0.00000000e+00,  5.55111512e-17,  1.00000000e+00]
        ])
    
    assert copy.obliquity == 0.0 and copy.true_anomaly_at_vernal_equinox == 0.0

    assert copy == copy2
    
def test_SpinParams_from_modified_params():
    # check that default spin flag updates or doesn't appropriately

    # check errors are raised appropriately

    # check setting pole_ra and dec

    # check setting obliquity and TAV

    # check setting everything else

    assert True


#### PorbParams class tests ####



def test_PorbParams_init():
    assert isinstance(mars_porb, porb.PorbParams)

def test_PorbParams_equality():
    assert mars_porb == mars_porb_copy

    assert mars_porb != europa_porb

def test_PorbParams_string_representation():
    correct_string = 'PORB:2000jan01 2000 Jan 01 00:00:00 IPLAN,TC=   499       0 Mars:Mars\n' + \
                     '        499              0      0.8644665      3.2269010E-02 -1.2815860\n' + \
                     '  9.3401980E-02   1.523712      0.4090926              0      0.9229373\n' + \
                     '   5.544402        350.892       176.0499       686.9928       3397.977\n' + \
                     '   24.62296              0      -1.240317      0.4397026              0\n' + \
                     '          0      0.3244966      0.8559125      0.4026360     -0.9458869\n' + \
                     '  0.2936299      0.1381286      0.0000000     -0.4256704      0.9048783\n'

    assert str(mars_porb) == correct_string

def test_PorbParams_verbose_string_representation():
    correct_string = '<--VERSION---> <--generation date->           IPLAN      TC orbit:pole\n' +\
                'PORB:2000jan01 2000 Jan 01 00:00:00 IPLAN,TC=   499       0 Mars:Mars\n' +\
                '     PLANUM             Tc           RODE           CLIN           ARGP\n' +\
                '        499              0      0.8644665      3.2269010E-02 -1.2815860\n' +\
                '       XECC            SJA           EOBL          SFLAG           ZBAA\n' +\
                '  9.3401980E-02   1.523712      0.4090926              0      0.9229373\n' +\
                '       ZBAB           WDOT             WO        OPERIOD            TJP\n' +\
                '   5.544402        350.892       176.0499       686.9928       3397.977\n' +\
                '      SIDAY          spare            TAV           BLIP           PBUG\n' +\
                '   24.62296              0      -1.240317      0.4397026              0\n' +\
                '      spare         BFRM 1              2              3              4\n' +\
                '          0      0.3244966      0.8559125      0.4026360     -0.9458869\n' +\
                '          5              6              7              8         BFRM 9\n' +\
                '  0.2936299      0.1381286      0.0000000     -0.4256704      0.9048783\n'
    
    assert mars_porb.verbose_output() == correct_string

def test_PorbParams_from_str():
    test_string =    'PORB:2000jan01 2000 Jan 01 00:00:00 IPLAN,TC=   499       0 Mars:Mars\n' + \
                     '        499              0      0.8644665      3.2269010E-02 -1.2815860\n' + \
                     '  9.3401980E-02   1.523712      0.4090926              0      0.9229373\n' + \
                     '   5.544402        350.892       176.0499       686.9928       3397.977\n' + \
                     '   24.62296              0      -1.240317      0.4397026              0\n' + \
                     '          0      0.3244966      0.8559125      0.4026360     -0.9458869\n' + \
                     '  0.2936299      0.1381286      0.0000000     -0.4256704      0.9048783\n'
    
    copy = get_mars_porb_params()
    copy.default_spin = -1
    copy.body_type = 'unknown'

    print(copy)

    from_str = porb.PorbParams.from_str(test_string)

    assert from_str == copy

