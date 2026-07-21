import pyorb.defaults as defaults
import pyorb.constants as const
import pyorb.porb as porb
import pyorb.kernel_mgmt as km
import pyorb.install as install
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

@pytest.fixture(scope="function")
def mars_porb():
    return get_mars_porb_params()
mars_porb_copy = get_mars_porb_params()
europa_porb = get_europa_porb_params()

mars_orb_elems = (0.8644665, 
                  0.9340198E-01, 
                  0.3226901E-01, 
                  -1.281586, 
                  (3397.977 / 686.9928) * -2*np.pi, 
                  1.523712, 
                  const.j2000_JD)

@pytest.fixture(scope="function")
def mars_orb():
    return get_mars_orb_params()

@pytest.fixture(scope="function")
def mars_spin():
 return get_mars_spin_params()


#### OrbParams class tests ####
def test_OrbParams_equality():
    test1 = get_mars_orb_params()
    test2 = get_mars_orb_params()

    assert test1 == test2

def test_OrbParams_from_porb_params(mars_porb, mars_orb):
    from_porb = porb.OrbParams.from_porb_params(mars_porb)

    assert from_porb == mars_orb

def test_OrbParams_from_orb_elems_tuple(mars_orb):
    orb_elems = mars_orb_elems
    from_orb_elems_tuple = porb.OrbParams.from_elems(orb_elems)

    assert from_orb_elems_tuple == mars_orb

def test_OrbParams_from_elems_and_second_params(mars_orb):
    orb_elems = (0.8644665, 0.9340198E-01, 0.3226901E-01, -1.281586, 3397.977 / 686.9928 * -2*np.pi, 1.523712, const.j2000_JD)
    second_params = (686.9928, 3397.977, 0.0)

    from_elems_and_second = porb.OrbParams.from_elems_and_second_params(orb_elems, second_params)

    assert from_elems_and_second == mars_orb

def test_OrbParams_from_modified_params(mars_porb, mars_orb):
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
def test_SpinParams_init(mars_spin):
    assert isinstance(mars_spin, porb.SpinParams)

def test_SpinParams_equality(mars_spin):
    copy = get_mars_spin_params()

    assert copy == mars_spin

    copy.pole_ra = 0.0

    assert copy != mars_spin

def test_SpinParams_from_spin_axis(mars_spin, mars_orb):
    spin_axis = (mars_spin.rotation_period, mars_spin.phase_at_j2000, mars_spin.pole_ra, mars_spin.pole_dec, mars_spin.default_spin_flag) 

    from_spin_axis = porb.SpinParams.from_spin_axis(spin_axis, mars_orb)
    assert from_spin_axis == mars_spin

def test_SpinParams_from_porb_params(mars_porb, mars_spin):
    from_porb = porb.SpinParams.from_porb_params(mars_porb)

    assert from_porb == mars_spin

def test_SpinParams_set_obliq_and_true_anomaly(mars_spin, mars_orb):
    copy = mars_spin.set_obliq_and_true_anomaly(0.0, 0.0, mars_orb)

    copy2 = get_mars_spin_params()
    copy2.obliquity = 0.0
    copy2.true_anomaly_at_vernal_equinox = 0.0
    copy2.pole_ra = 4.771211156136408
    copy2.pole_dec = 1.1400991060229944
    copy2.rotation_matrix_FtoB = np.array([
            [ 1.00000000e+00,  0.00000000e+00,  0.00000000e+00],
            [ 0.00000000e+00,  1.00000000e+00, -5.55111512e-17],
            [ 0.00000000e+00,  5.55111512e-17,  1.00000000e+00]
        ])
    
    assert copy.obliquity == 0.0 and copy.true_anomaly_at_vernal_equinox == 0.0

    assert copy == copy2
    
def test_SpinParams_from_modified_params(mars_porb, mars_orb):
    # check that default spin flag updates or doesn't appropriately
    test_spin = porb.SpinParams.from_modified_params(mars_porb, mars_orb)

    assert test_spin.default_spin_flag == 1

    test_spin = porb.SpinParams.from_modified_params(mars_porb, mars_orb, rotation_period=1.0, phase_at_j2000=2.0)
    
    assert test_spin.default_spin_flag == 0
    assert test_spin.rotation_period == 1.0 and test_spin.phase_at_j2000 == 2.0

    # check errors are raised appropriately
    with pytest.raises(ValueError):
        test_spin = porb.SpinParams.from_modified_params(mars_porb, mars_orb, pole_ra=1.0)
    
    with pytest.raises(ValueError):
        test_spin = porb.SpinParams.from_modified_params(mars_porb, mars_orb, obliquity=1.0)
    
    with pytest.raises(ValueError):
        test_spin = porb.SpinParams.from_modified_params(mars_porb, mars_orb, pole_ra=1.0, obliquity=1.0)
    
    with pytest.raises(NotImplementedError):
        test_spin = porb.SpinParams.from_modified_params(mars_porb, mars_orb, rotation_matrix_FtoB=np.array([[1,2,3],[4,5,6],[7,8,9]]))
    
    # check setting pole_ra and dec
    check_spin = porb.SpinParams(
        rotation_period=24.622960911049553,
        phase_at_j2000=176.0499,
        pole_ra=4.771211156136408,
        pole_dec=1.1400991060229944,
        default_spin_flag=0,
        obliquity=0.0,
        rotation_matrix_FtoB=np.array(
            [[ 1.00000000e+00,  0.00000000e+00,  0.00000000e+00],
             [ 0.00000000e+00,  1.00000000e+00, -5.55111512e-17],
             [ 0.00000000e+00,  5.55111512e-17,  1.00000000e+00]]),
        true_anomaly_at_vernal_equinox=0.0
    )

    test_spin = porb.SpinParams.from_modified_params(mars_porb, mars_orb, pole_ra=-1.5119741510431786+(2*np.pi), pole_dec=1.1400991060229944)

    assert test_spin == check_spin

    # check setting obliquity and TAV
    test_spin = porb.SpinParams.from_modified_params(mars_porb, mars_orb, obliquity=0.0, true_anomaly_at_vernal_equinox=0.0)

    assert test_spin == check_spin


#### PorbParams class tests ####

def test_PorbParams_init(mars_porb):
    assert isinstance(mars_porb, porb.PorbParams)

def test_PorbParams_equality(mars_porb):
    assert mars_porb == mars_porb_copy

    assert mars_porb != europa_porb

def test_PorbParams_string_representation(mars_porb):
    correct_string = 'PORB:2000jan01 2000 Jan 01 00:00:00 IPLAN,TC=   499       0 Mars:Mars\n' + \
                     '        499              0      0.8644665      3.2269010E-02 -1.2815860\n' + \
                     '  9.3401980E-02   1.523712      0.4090926              0      0.9229373\n' + \
                     '   5.544402        350.892       176.0499       686.9928       3397.977\n' + \
                     '   24.62296              0      -1.240317      0.4397026              0\n' + \
                     '          0      0.3244966      0.8559125      0.4026360     -0.9458869\n' + \
                     '  0.2936299      0.1381286      0.0000000     -0.4256704      0.9048783\n'

    assert str(mars_porb) == correct_string

def test_PorbParams_verbose_string_representation(mars_porb):
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

def test_PorbParams_from_str(mars_porb):
    test_string =    'PORB:2000jan01 2000 Jan 01 00:00:00 IPLAN,TC=   499       0 Mars:Mars\n' + \
                     '        499              0      0.8644665      3.2269010E-02 -1.2815860\n' + \
                     '  9.3401980E-02   1.523712      0.4090926              0      0.9229373\n' + \
                     '   5.544402        350.892       176.0499       686.9928       3397.977\n' + \
                     '   24.62296              0      -1.240317      0.4397026              0\n' + \
                     '          0      0.3244966      0.8559125      0.4026360     -0.9458869\n' + \
                     '  0.2936299      0.1381286      0.0000000     -0.4256704      0.9048783\n'
    
    copy = mars_porb
    copy.default_spin = -1
    copy.body_type = 'unknown'

    print(copy)

    from_str = porb.PorbParams.from_str(test_string)

    assert from_str == copy

kernels_dir = install.test_kernels_dir

def test_get_orbital_naifid():
    kernels = f'{kernels_dir}/input/test3'
    epoch_date = defaults.epoch_date

    # cases: Mars, Phobos, (3779) Kieffer
    naifids = [499, 401, 20003779]
    mks = [f'{kernels}/mk/000000401.tm',
           f'{kernels}/mk/000000401.tm',
           f'{kernels}/mk/020003779.tm']
    expected_orbital_naifid = [4, 4, 20003779]

    for i in range(len(naifids)):
        orbit_naifid = porb.get_orbital_naifid(mks[i], naifids[i], epoch_date)

        assert orbit_naifid == expected_orbital_naifid[i]

    # cases: (65803) Didymos, Didymos system barycenter, Dimorphos (satellite of Didymos)
    # This set of cases is probably totally overkill. 
    naifids = [920065803, 120065803, 20065803]
    mks = [f'{kernels}/mk/020065803.tm',
           f'{kernels}/mk/020065803.tm',
           f'{kernels}/mk/020065803.tm']
    expected_orbital_naifid = [20065803, 920065803, 20065803]

    for i in range(len(naifids)):
        orbit_naifid = porb.get_orbital_naifid(mks[i], naifids[i], epoch_date)

        assert orbit_naifid == expected_orbital_naifid[i]


def test_get_orbital_elements():
    kernels = f'{kernels_dir}/input/test3'
    epoch_date = defaults.epoch_date

    # Case: Mars
    naifid = 4
    mk = f'{kernels}/mk/000000401.tm'
    parent = 'SUN'

    orb_elems = porb.get_orbital_elements(mk, naifid, parent, epoch_date)
    test_mars_orb_elems = (0.8637487929270791,
                           0.0933859300993825,
                           0.03224778664798331,
                           5.004295160039962,
                           1.6138903629894965,
                           1.523791628489534,
                           2460615.5)

    assert orb_elems == pytest.approx(test_mars_orb_elems)

def test_get_spin_axis():
    kernels = f'{kernels_dir}/input/test3'

    # case: Mars
    # tests normal behavior
    naifid = 499
    mk = f'{kernels}/mk/000000401.tm'

    spin_axis = porb.get_spin_axis(mk, naifid)

    mars_spin_axis = (
        24.622962143046955,
        176.049863,
        5.5373921900749785,
        0.9500266243444937,
        0)
    
    assert spin_axis == pytest.approx(mars_spin_axis)

    # Case: justitia
    # tests behavior with 8-digit naifid
    naifid = 20000269
    mk = f'{kernels}/mk/020000269.tm'

    spin_axis = porb.get_spin_axis(mk, naifid)

    test_spin_axis = (
        33.12910302167519,
        316.2 ,
        1.4847350410358011,
        -1.0097131800947183,
        0)
    
    assert spin_axis == pytest.approx(test_spin_axis)

def test_get_secondary_orb_params():
    
    # Case: Mars
    secondary_orb_params = porb.get_secondary_orb_params(mars_orb_elems)

    mars_secondary_orb_params = (
        686.9928, 
        3397.977, 
        0.0)

    assert secondary_orb_params == pytest.approx(mars_secondary_orb_params)

def test_get_secondary_spin_params(mars_porb, mars_orb):
    pole_ra = mars_porb.ZBAB
    pole_dec = mars_porb.ZBAA

    (obliquity, rotation_matrix_FtoB, tav) = porb.get_secondary_spin_params(mars_orb, pole_ra, pole_dec)


    assert pole_ra == pytest.approx(mars_porb.ZBAB)
    assert pole_dec == pytest.approx(mars_porb.ZBAA)

    assert obliquity == pytest.approx(mars_porb.BLIP)
    assert tav == pytest.approx(mars_porb.TAV)

    assert np.all(np.isclose(rotation_matrix_FtoB, mars_porb.BFRM))

    pole_ra = 4.771211156136408
    pole_dec = 1.1400991060229944

    identity_matrix = np.array(
        [[1.0, 0.0, 0.0],
         [0.0, 1.0, 0.0],
         [0.0, 0.0, 1.0]]
    )

    (obliquity, rotation_matrix_FtoB, tav) = porb.get_secondary_spin_params(mars_orb, pole_ra, pole_dec)

    assert obliquity == pytest.approx(0.0)
    assert tav == pytest.approx(0.0)

    assert np.all(np.isclose(rotation_matrix_FtoB, identity_matrix))

def test_alt_get_secondary_spin_params(mars_porb, mars_orb):
    obliquity = 0.0
    tav = 0.0

    obliquity = mars_porb.BLIP
    tav = mars_porb.TAV

    (pole_ra, pole_dec, rotation_matrix_FtoB) = porb.alt_get_secondary_spin_params(mars_orb, obliquity, tav)

    assert pole_ra == pytest.approx(mars_porb.ZBAB)
    assert pole_dec == pytest.approx(mars_porb.ZBAA)

    assert obliquity == pytest.approx(mars_porb.BLIP)
    assert tav == pytest.approx(mars_porb.TAV)

    assert np.all(np.isclose(rotation_matrix_FtoB, mars_porb.BFRM))

    obliquity = 0.0
    tav = 0.0

    (pole_ra, pole_dec, rotation_matrix_FtoB) = porb.alt_get_secondary_spin_params(mars_orb, obliquity, tav)

    identity_matrix = np.array(
        [[1.0, 0.0, 0.0],
         [0.0, 1.0, 0.0],
         [0.0, 0.0, 1.0]]
    )

    test_ra = 4.771211156136408
    test_dec = 1.1400991060229944

    assert pole_ra == pytest.approx(test_ra)
    assert pole_dec == pytest.approx(test_dec)

    assert np.all(np.isclose(rotation_matrix_FtoB, identity_matrix))

def test_get_porb_params():
    kernels = f'{kernels_dir}/input/test3'
    epoch_date = defaults.epoch_date

    # case: Mars
    # tests normal behavior
    name = 'Mars'
    naifid = 499
    mk = f'{kernels}/mk/000000401.tm'

    p_params = porb.get_porb_params(name, naifid, mk, epoch_date)

    good_porb_params = porb.PorbParams.from_str(
        'PORB:2025nov20 2000 Jan 01 00:00:00 IPLAN,TC=   499 0.24833 Mars:Mars\n' +\
        '        499      0.2483324      0.8637488      3.2247787E-02  5.0042952\n' +\
        '  9.3385930E-02   1.523792      0.4090926              0      0.9500266\n' +\
        '   5.537392        350.892       176.0499       687.0466       8894.026\n' +\
        '   24.62296              0      -1.203815      0.4174691              0\n' +\
        '          0      0.3587996      0.8532511      0.3784513     -0.9334146\n' +\
        '  0.3279852      0.1454747      0.0000000     -0.4054482      0.9141180\n')

    good_porb_params.default_spin = 0
    good_porb_params.body_type = 'Planet'

    lines = str(p_params).split('\n')
    goodlines = str(good_porb_params).split('\n')

    for i in range(1, len(lines)):
        assert lines[i] == goodlines[i]

    # Case: (3779) Kieffer
    # tests using default spin axis
    name = 'Kieffer'
    naifid = 20003779
    mk = f'{kernels}/mk/020003779.tm'

    p_params = porb.get_porb_params(name, naifid, mk, epoch_date)

    assert isinstance(p_params, porb.PorbParams)
    assert p_params.default_spin == 1

def test_high_level_get_porb_params(download_de442_spk):
    indir = kernels_dir + '/input'
    outdir = kernels_dir + '/output/test13'
    default_mk = outdir+'/mk/krc_default.tm'
    naifid_map_file = outdir+'/naifid_map.csv'

    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    os.makedirs(outdir+'/mk')
    shutil.copy(indir+'/test2/naifid_map.csv', naifid_map_file)
    shutil.copy(indir+'/test2/mk/krc_default.tm', default_mk)

    os.makedirs(outdir+'/lsk')
    os.makedirs(outdir+'/pck')
    os.makedirs(outdir+'/spk')
    shutil.copy(indir+'/test1/lsk/naif0012.tls', outdir+'/lsk/naif0012.tls')
    shutil.copy(indir+'/test1/pck/pck00010.tpc', outdir+'/pck/pck00010.tpc')
    shutil.copy(indir+'/test1/spk/de442.bsp', outdir+'/spk/de442.bsp')
    
    # Mars case
    porb_params = porb.high_level_get_porb_params('Mars', update_kernels=False, kernels_dir=outdir, default_mk=default_mk, naifid_map_file=naifid_map_file)

    good_porb_params = porb.PorbParams.from_str(
        'PORB:2025nov20 2000 Jan 01 00:00:00 IPLAN,TC=   499 0.24833 Mars:Mars\n' +\
        '        499      0.2483324      0.8637488      3.2247787E-02  5.0042952\n' +\
        '  9.3385930E-02   1.523792      0.4090926              0      0.9500266\n' +\
        '   5.537392        350.892       176.0499       687.0466       8894.026\n' +\
        '   24.62296              0      -1.203815      0.4174691              0\n' +\
        '          0      0.3587996      0.8532511      0.3784513     -0.9334146\n' +\
        '  0.3279852      0.1454747      0.0000000     -0.4054482      0.9141180\n')

    good_porb_params.default_spin = 0
    good_porb_params.body_type = 'Planet'

    lines = str(porb_params).split('\n')
    goodlines = str(good_porb_params).split('\n')

    for i in range(1, len(lines)):
        assert lines[i] == goodlines[i]

def test_modify_porb_params(mars_porb):
    mars_porb_params = mars_porb
    europa_porb_params = get_europa_porb_params()

    test_porb_params = porb.modify_porb_params(mars_porb_params,
        long_of_asc_node=europa_porb_params.RODE,
        eccentricity=europa_porb_params.XECC,
        inclination=europa_porb_params.CLIN,
        arg_of_peri=europa_porb_params.ARGP,
        orbit_period=europa_porb_params.OPERIOD,
        perihelion_date=europa_porb_params.TJP,
        centuries_from_j2000=europa_porb_params.TC,
        rotation_period=europa_porb_params.SIDAY,
        phase_at_j2000=europa_porb_params.WO,
        pole_ra=europa_porb_params.ZBAB,
        pole_dec=europa_porb_params.ZBAA
        )

    assert test_porb_params.TC == pytest.approx(europa_porb_params.TC)
    assert test_porb_params.RODE == pytest.approx(europa_porb_params.RODE)
    assert test_porb_params.CLIN == pytest.approx(europa_porb_params.CLIN)
    assert test_porb_params.ARGP == pytest.approx(europa_porb_params.ARGP)
    assert test_porb_params.XECC == pytest.approx(europa_porb_params.XECC)
    assert test_porb_params.SJA == pytest.approx(europa_porb_params.SJA)
    assert np.isclose(test_porb_params.ZBAA, europa_porb_params.ZBAA)
    assert np.isclose(test_porb_params.ZBAB, europa_porb_params.ZBAB)
    # assert test_porb_params.WDOT == pytest.approx(europa_porb_params.WDOT)  <- wdot set to 0 in default europa hdf
    assert test_porb_params.WO == pytest.approx(europa_porb_params.WO)
    assert test_porb_params.OPERIOD == pytest.approx(europa_porb_params.OPERIOD)
    assert test_porb_params.TJP == pytest.approx(europa_porb_params.TJP)
    assert test_porb_params.SIDAY == pytest.approx(europa_porb_params.SIDAY)
    assert np.isclose(test_porb_params.TAV, europa_porb_params.TAV)
    assert np.isclose(test_porb_params.BLIP, europa_porb_params.BLIP) # <- obliquity from pole RA & DEC is correctly calculated, but doesn't match default europa HDF
    assert np.all(np.isclose(test_porb_params.BFRM, europa_porb_params.BFRM))
    