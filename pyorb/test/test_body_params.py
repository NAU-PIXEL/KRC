from pyorb.body_params import add_str_dset, add_num_dset, type_params_dict, planet_flux_dict, krc_params_dict, get_N24, get_satellite_semimajor_axis, get_body_params, write_hdf, get_radius, read_hdf, high_level_write_hdf
import pyorb.defaults as defaults
import pyorb.porb as porb
import pytest
import tempfile
import os
import numpy as np

def dicts_match_keys_and_types(dict1:dict, dict2:dict)->bool:
    '''
    conditional checking if two dicts have the same keys 
    and the same types for the values associated with those keys
    '''
    keys_match = dict1.keys() == dict2.keys()

    types_match = all(isinstance(dict1[k],type(v)) for k, v in dict2.items())

    return keys_match and types_match

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

def get_deimos_porb_params():
    out=porb.PorbParams(
        default_spin        = 1,
        porb_version        = '2000jan01',
        generation_date     = '2000 Jan 01 00:00:00',
        NAME                = 'Deimos',
        body_type           = 'Satellite',

        PLANUM              = 402,

        TC                  = 0.0,
        RODE                = 0.8644665,
        CLIN                = 0.3226901E-01,
        ARGP                = -1.281586,

        XECC                = 0.9340198E-01,
        SJA                 = 1.523712,
        EOBL                = 0.4090926,
        SFLAG               = 0.000000,
        ZBAA                = 0.9339938,

        ZBAB                = 5.526397,
        WDOT                = 350.8920,
        WO                  = 176.0499,
        OPERIOD             = 686.9928,
        TJP                 = 3397.977,

        SIDAY               = 30.29858,
        spar17              = 0.000000,
        TAV                 = -1.240317,
        BLIP                = 0.4397026,
        PBUG                = 0.000000,

        spar21              = 0.000000,
        BFRM = np.array([[ 0.3240992,  -0.9460231,  0.000000 ],
                         [ 0.8621530,   0.2953660, -0.4116443],
                         [ 0.3894250,   0.1334136,  0.9113446]])
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

mars_type_params = type_params_dict(
    body_name   =   'Mars',
    body_type   =   'Planet',
    naifid      =   499,
    parent_body =   ''
)

mars_planet_flux = planet_flux_dict(
    BT_Avg      = 220.,
    BT_Max      = 280.,
    BT_Min      = 160.,
    Dis_AU      = 1.523712,
    Geom_alb    = 0.15,
    Mut_Period  = -999.,
    Orb_Radius  = -999.,
    Radius      = 3396.19
)

mars_krc_params = krc_params_dict(
    ARC2_G0 = 0.5,
    DUSTA   = 0.9,
    TAURAT  = 0.22,
    PTOTAL  = 546.,
    GRAV    = 3.7131376704574803,
    PERIOD  = 24.62296/24.,
    DELJUL  = 686.9928/360.,
    N24     = 96
)

mars_porb = get_mars_porb_params()
europa_porb = get_europa_porb_params()
deimos_porb = get_deimos_porb_params()

kernelsdir = "./test/kernels"

def test_get_body_params_satellite_parent_body_is_correct():
    # deimos case
    (type_params, planet_flux, krc_params) = get_body_params(deimos_porb, f'{kernelsdir}/input/test3/mk/000000401.tm')

    assert type_params['parent_body'] == 'Mars'

def test_get_satellite_semimajor_axis():
    # deimos case
    semimajor_axis = get_satellite_semimajor_axis(402, 4, 6.4171E+23, f'{kernelsdir}/input/test3/mk/000000401.tm')

    assert semimajor_axis == pytest.approx(2.346e+4,rel=1e5)

def test_get_body_params_has_default_radius_given_a_bad_metakernel():
    # Mars, metakernel has no radius info.
    (type_params, planet_flux, krc_params) = get_body_params(mars_porb, f'{kernelsdir}/naif0012.tls')

    assert planet_flux['Radius'] == pytest.approx(defaults.radius)

def test_get_body_params_has_good_radius_from_kernels():
    # Mars case
    (type_params, planet_flux, krc_params) = get_body_params(mars_porb, f'{kernelsdir}/pck00011.tpc')

    assert planet_flux['Radius'] == pytest.approx(3396.19)

def test_get_N24():
    '''
    N24 should always be at least 96, be a multiple of 24, 
    and produce a timestep between 0.25 and 0.5 hours when SIDAY > 48 hours.
    '''
    # europa case
    N24 = get_N24(europa_porb.SIDAY)
    timestep = europa_porb.SIDAY/N24

    assert N24%24 == 0 and timestep <= 0.5 and timestep >= 0.25


def test_get_body_params_typing_is_correct():
    # Mars case
    (type_params, planet_flux, krc_params) = get_body_params(mars_porb, f'{kernelsdir}/pck00011.tpc')

    assert dicts_match_keys_and_types(type_params, mars_type_params) and dicts_match_keys_and_types(planet_flux, mars_planet_flux) and dicts_match_keys_and_types(krc_params, mars_krc_params)

def test_get_body_params_dict_values_correct_mars():
    # Mars case
    mars_params = get_body_params(mars_porb, f'{kernelsdir}/pck00011.tpc')

    assert mars_params == pytest.approx((mars_type_params, mars_planet_flux, mars_krc_params))

def test_get_radius_with_metakernel():
    # Mars case
    mars_mk = f'{kernelsdir}/pck00011.tpc'
    mars_radius = 3396.19
    radius = get_radius(499, mars_mk)

    assert radius == pytest.approx(mars_radius)

def test_get_radius_no_metakernel():
    # using the standard PCK, which does not supply radii for this object:
    mk = f'{kernelsdir}/pck00011.tpc'
    radius = get_radius(2003779, mk)

    assert radius == pytest.approx(defaults.radius)

def test_write_read_hdf_roundtrip():
    temp_dir = tempfile.gettempdir()
    mars_body_params = (mars_type_params, mars_planet_flux, mars_krc_params)
    
    hdf = f'{temp_dir}/MARS.porb.hdf'
    if os.path.exists(hdf):
        os.remove(hdf)
    hdf = write_hdf(mars_porb, mars_body_params, temp_dir)
    (type_params, planet_flux, krc_params, porb_params) = read_hdf(hdf)
    os.remove(hdf)

    np.testing.assert_equal(porb_params.__dict__, mars_porb.__dict__)
    assert type_params == pytest.approx(mars_type_params)
    assert planet_flux == pytest.approx(mars_planet_flux)
    assert krc_params == pytest.approx(mars_krc_params) 

