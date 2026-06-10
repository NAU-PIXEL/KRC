from src.body_params import add_str_dset, add_num_dset, type_params_dict, planet_flux_dict, krc_params_dict, get_body_params, write_hdf
import src.defaults as defaults
import pytest

def dicts_match_keys_and_types(dict1:dict, dict2:dict)->bool:
    '''
    conditional checking if two dicts have the same keys 
    and the same types for the values associated with those keys
    '''
    keys_match = dict1.keys() == dict2.keys()

    types_match = all(isinstance(dict1[k],type(v)) for k, v in dict2.items())

    return keys_match and types_match

def get_mars_porb_params():
    out={}
    out['default_spin']     = 1
    out['porb_version']     = '2000jan01'
    out['generation_date']  = '2000 Jan 01 00:00:00'
    out['NAME']             = 'Mars'
    out['body_type']        = 'Planet'

    out['PLANUM']           = 499

    out['TC']               = 0.0
    out['RODE']             = 0.8644665
    out['CLIN']             = 0.3226901E-01
    out['ARGP']             = -1.281586  

    out['XECC']             = 0.9340198E-01
    out['SJA']              = 1.523712
    out['EOBL']             = 0.4090926
    out['SFLAG']            = 0.000000
    out['ZBAA']             = 0.9229373

    out['ZBAB']             = 5.544402
    out['WDOT']             = 0.000000
    out['WO']               = 0.000000
    out['OPERIOD']          = 686.9928
    out['TJP']              = 3397.977

    out['SIDAY']            = 24.62296
    out['spar17']           = 0.000000
    out['TAV']              = -1.240317
    out['BLIP']             = 0.4397026
    out['PBUG']             = 0.000000

    out['spar21']           = 0.000000
    out['BFRM 1']           = 0.3244966
    out['BFRM 2']           = 0.8559125
    out['BFRM 3']           = 0.4026360
    out['BFRM 4']           = -0.9458869

    out['BFRM 5']           = 0.2936299
    out['BFRM 6']           = 0.1381286
    out['BFRM 7']           = 0.000000
    out['BFRM 8']           = -0.4256704
    out['BFRM 9']           = 0.9048783
    
    return out

def get_europa_porb_params():
    out={}
    out['default_spin']     = 1
    out['porb_version']     = '2000jan01'
    out['generation_date']  = '2000 Jan 01 00:00:00'
    out['NAME']             = 'Europa'
    out['body_type']        = 'Satellite'

    out['PLANUM']           = 502

    out['TC']               = 0.0
    out['RODE']             = 1.753958
    out['CLIN']             = 0.2276282E-01
    out['ARGP']             = -1.496526

    out['XECC']             = 0.4837299E-01
    out['SJA']              = 5.202875
    out['EOBL']             = 0.4090926
    out['SFLAG']            = 0.000000
    out['ZBAA']             = 1.125917

    out['ZBAB']             = 4.678863
    out['WDOT']             = 0.000000
    out['WO']               = 0.000000
    out['OPERIOD']          = 4334.739
    out['TJP']              = -238.1847

    out['SIDAY']            = 85.22835
    out['spar17']           = 0.000000
    out['TAV']              = 2.136860
    out['BLIP']             = 0.5414684E-01
    out['PBUG']             = 0.000000

    out['spar21']           = 0.000000
    out['BFRM 1']           = -0.5363136
    out['BFRM 2']           = -0.8427818
    out['BFRM 3']           = -0.4567862E-01
    out['BFRM 4']           = 0.8440188

    out['BFRM 5']           = -0.5355276
    out['BFRM 6']           = -0.2902550E-01
    out['BFRM 7']           = 0.000000
    out['BFRM 8']           = -0.5412039E-01
    out['BFRM 9']           = 0.9985344
    
    return out

mars_type_params = type_params_dict(
    body_name   =   'Mars',
    body_type   =   'Planet',
    naifid      =   499,
    parent_body =   0
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
    GRAV    = 3.71481570247261607912,
    PERIOD  = 24.62296/24.,
    DELJUL  = 686.9928/360.,
    N24     = 96
)

mars_porb = get_mars_porb_params()
europa_porb = get_europa_porb_params()

kernelsdir = '/home/nsmith/KRC/pyorb/test/kernels'

def test_get_body_params_satellite_parent_body_is_correct():
    # europa case
    (type_params, planet_flux, krc_params) = get_body_params(europa_porb, f'{kernelsdir}/pck00011.tpc')

    assert type_params['parent_body'] == 'Jupiter'

def test_get_body_params_has_default_radius_given_a_bad_metakernel():
    # Mars, metakernel has no radius info.
    (type_params, planet_flux, krc_params) = get_body_params(mars_porb, f'{kernelsdir}/naif0012.tls')

    assert planet_flux['Radius'] == pytest.approx(defaults.radius)

def test_get_body_params_has_good_radius_from_kernels():
    # Mars case
    (type_params, planet_flux, krc_params) = get_body_params(mars_porb, f'{kernelsdir}/pck00011.tpc')

    assert planet_flux['Radius'] == pytest.approx(3396.19)

def test_get_body_params_N24_is_good():
    '''
    N24 should always be at least 96, be a multiple of 24, 
    and produce a timestep between 0.25 and 0.5 hours when SIDAY > 48 hours.
    '''
    # europa case
    (type_params, planet_flux, krc_params) = get_body_params(europa_porb, f'{kernelsdir}/pck00011.tpc')
    timestep = europa_porb['SIDAY']/krc_params['N24']

    assert krc_params['N24']%24 == 0 and timestep <= 0.5 and timestep >= 0.25


def test_get_body_params_typing_is_correct():
    # Mars case
    (type_params, planet_flux, krc_params) = get_body_params(mars_porb, f'{kernelsdir}/pck00011.tpc')

    assert dicts_match_keys_and_types(type_params, mars_type_params) and dicts_match_keys_and_types(planet_flux, mars_planet_flux) and dicts_match_keys_and_types(krc_params, mars_krc_params)

def test_get_body_params_dict_values_correct_mars():
    # Mars case
    mars_params = get_body_params(mars_porb, f'{kernelsdir}/pck00011.tpc')

    assert mars_params == pytest.approx((mars_type_params, mars_planet_flux, mars_krc_params))

