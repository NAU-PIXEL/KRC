from src.body_params import add_str_dset, add_num_dset, type_params_dict, planet_flux_dict, krc_params_dict, get_body_params, write_hdf
import src.defaults as defaults

def get_default_porb_params():
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

out = get_default_porb_params()


def test_get_body_params_typing_is_correct(out):
    # Mars case
    mars_params = get_body_params(out, 'kernels/pck00011.tpc')

    assert isinstance(mars_params, (type_params_dict, planet_flux_dict, krc_params_dict))

def test_get_body_params_has_default_radius_given_a_bad_metakernel(out):
    # Mars, metakernel has no radius info.
    (type_dict, planet_flux, krc_dict) = get_body_params(out, 'kernels/naif0012.tls')

    assert planet_flux['Radius'] == pytest.approx(defaults.radius)

def test_get_body_params_has_good_radius_from_kernels(out):
    # Mars case
    (type_dict, planet_flux, krc_dict) = get_body_params(out, 'kernels/pck00011.tpc')

    assert planet_flux['Radius'] == pytest.approx(3396.19)

def test_get_body_params_dict_values_correct_mars(out):
    # Mars case
    mars_params = get_body_params(out, 'kernels/pck00011.tpc')

    default_type_params = type_params_dict(
        body_name   =   'Mars',
        body_type   =   'Planet',
        naifid      =   499,
        parent_body =   0
    )

    default_planet_flux = planet_flux_dict(
        BT_Avg      = 220.,
        BT_Max      = 280.,
        BT_Min      = 160.,
        Dis_AU      = 1.523712,
        Geom_alb    = 0.15,
        Mut_Period  = -999.,
        Orb_Radius  = -999.,
        Radius      = 3396.19
    )

    default_krc_params = krc_params_dict(
        ARC2_G0 = 0.5,
        DUSTA   = 0.9,
        TAURAT  = 0.22,
        PTOTAL  = 546.,
        GRAV    = 3.71481570247261607912,
        PERIOD  = 24.62296/24.,
        DELJUL  = 686.9928/360.,
        N24     = 96
    )

    assert mars_params == pytest.approx((default_type_params, default_planet_flux, default_krc_params))

