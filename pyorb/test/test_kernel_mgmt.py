import pyorb.defaults as defaults
import pyorb.porb as porb
import pyorb.kernel_mgmt as km
import pyorb.config as config
import pytest

import os
import shutil
import numpy as np
import spiceypy as spice


'''
test/kernels/input/test1/
    lsk/
        naif0012.tls
    pck/
        pck00010.tpc    <-- intentionally out of date.
    spk/
        de442.bsp       <-- keep up to date to reduce unnecessary downloads during testing.

test/kernels/input/test2/
    mk/                 <--- these metakernels may point to kernels that don't exist.
        002000024.tm
        002000052.tm
        020003779.tm
        krc_default.tm
    naifid_map.csv      <--- should contain all satellites, plus 24 Themis, 52 Europa, and 3779 Kieffer

test/kernels/input/test3/
    lsk/
        naif0012.tls
    mk/
        000000401.tm
        020000269.tm
        020003779.tm
        020065803.tm
    pck/
        20000269.tpc
        pck00011.tpc
    spk/
        20000269.bsp
        20003779.bsp
        de442.bsp       <-- symlink to test1/spk/de442.bsp
        didymos_barycenter_s205_v01.bsp
        didymos_system_s501_v01.bsp
        mar099s.bsp


'''


kernels_dir = config.test_kernels_dir

output_kernels_dir = f'{kernels_dir}/output'
input_kernels_dir = f'{kernels_dir}/input'


def test_download_target_lsk_pck_spk():
    outdir = output_kernels_dir + '/test1'
    lsk_target = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/latest_leapseconds.tls'
    pck_target = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/a_old_versions/pck00009.tpc'
    spk_target = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/a_old_versions/de410s.bsp'

    lsk_dest = f'{outdir}/lsk/latest_leapseconds.tls'
    pck_dest = f'{outdir}/pck/pck00009.tpc'
    spk_dest = f'{outdir}/spk/de410s.bsp'

    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    km.download_target(lsk_target, kernels_dir=outdir)
    km.download_target(pck_target, kernels_dir=outdir)
    km.download_target(spk_target, kernels_dir=outdir)

    assert os.path.exists(lsk_dest)
    assert os.path.exists(pck_dest)
    assert os.path.exists(spk_dest)

def test_download_target_specified_dest():
    outdir = output_kernels_dir+'/test2'
    target = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/aareadme.txt'
    destination = f'{outdir}/test.txt'
    
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    km.download_target(target, dest=destination, kernels_dir=outdir)

    assert os.path.exists(destination)

def test_update_naif_kernel():
    outdir = output_kernels_dir+'/test3'
    source = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/'
    regex = 'naif\\d{4}\\.tls$'

    # Clear out the destination directory
    dest_dir = f'{outdir}/lsk'
    if os.path.exists(dest_dir):
        shutil.rmtree(dest_dir)
    assert not os.path.exists(dest_dir)
    
    os.makedirs(dest_dir)

    # Case where kernel needs an update:
    # set up destination directory...
    file1 = f'{dest_dir}/naif0008.tls'
    file2 = f'{dest_dir}/naif0009.tls'
    with open(file1, 'w') as f:
        f.write('')
    with open(file2, 'w') as f:
        f.write('')

    current = km.update_naif_kernel(source, regex, kernels_dir=outdir)

    assert os.path.exists(current) and current != file1 and current != file2

    # Clear out the destination directory
    dest_dir = f'{outdir}/lsk'
    shutil.rmtree(dest_dir)
    assert not os.path.exists(dest_dir)

    # Case where no kernels exist:
    current = km.update_naif_kernel(source, regex, kernels_dir=outdir)

    assert os.path.exists(current)

    # Case where no update is needed:
    current2 = km.update_naif_kernel(source, regex, kernels_dir=outdir)

    assert current2 == current

def test_write_metakernel(download_de442_spk):
    kernel_list = ['lsk/naif0012.tls', 
                   'pck/pck00010.tpc', 
                   'spk/de442.bsp']
    naifid = 499
    outdir = output_kernels_dir+'/test4/mk'
    indir = input_kernels_dir+'/test1'
    
    # Clear out the destination directory
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    # Check that an error is raised when name is castable as an int.
    with pytest.raises(RuntimeError):
        mk = km.write_metakernel(kernel_list, 20003779, name='3779', outdir=outdir, kernels_dir=indir)

    # # Check behavior when name=None and body is not covered by default kernels?
    # mk = km.write_metakernel(kernel_list, 20003779, name=None, outdir=outdir, kernels_dir=indir)
    # # Check that the header contains the correct body name.
    # with open(mk, 'r') as m:
    #     lines = m.readlines()
    # assert lines[3] == 'KIEFFER\n'

    # Mars case...
    mk = km.write_metakernel(kernel_list, naifid, outdir=outdir, kernels_dir=indir)

    # Check that the mk was generated successfully and written to the correct location.
    assert os.path.exists(mk)

    # Check that the header contains the correct body name.
    with open(mk, 'r') as m:
        lines = m.readlines()
    assert lines[3] == 'MARS\n'

    # Check that the mk can be loaded and contains the correct kernel info.
    spice.kclear()
    spice.furnsh(mk)

    # There should be 4 total kernels loaded (including the metakernel itself)
    n_kernels = spice.ktotal('ALL')
    assert n_kernels == 4

    loaded_kernels = []
    for i in range(n_kernels):
        kernel, _, _, _ = spice.kdata(i, 'ALL')
        loaded_kernels.append(kernel)

    assert f'{indir}/lsk/naif0012.tls' in loaded_kernels
    assert f'{indir}/pck/pck00010.tpc' in loaded_kernels
    assert f'{indir}/spk/de442.bsp'    in loaded_kernels

def test_update_default_kernels():
    outdir = output_kernels_dir+'/test5'
    default_mk = outdir+'/mk/krc_default.tm'
    indir = input_kernels_dir+'/test1'
    
    # set up working kernels directory, ensure the outdir matches indir.
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    shutil.copytree(indir,outdir)

    # Check that a newer version of pck00010.tpc is downloaded and added to the default mk.
    # pck00010.tpc has been superseded by pck00011.tpc since 2022-12-27.
    km.update_default_kernels(kernels_dir=outdir)

    ##### change to use read_default_mk()?

    spice.kclear()
    spice.furnsh(default_mk)
    
    n_kernels = spice.ktotal('ALL')

    pck = ''
    for i in range(n_kernels):
        kernel, _, _, _ = spice.kdata(i, 'ALL')
        if kernel[-3:] == 'tpc':
            pck = kernel
    
    pck_version = int(pck[-9:-4])

    assert pck_version > 10
    assert os.path.exists(pck)

def test_read_default_mk():
    mk = input_kernels_dir+'/test2/mk/krc_default.tm'
    intended_kernels_list = ['lsk/naif0012.tls',
                             'pck/pck00011.tpc',
                             'spk/de442.bsp']

    kernels_list = km.read_mk(mk)

    assert kernels_list == intended_kernels_list

def test_query_naifid_map():
    searches = ['Sun', 'Mars', 'Europa', 'kore', '52 Europa', 'Themis', 'THEMISTO', 'Kieffer']
    results = [10, 499, 502, 549, 2000052, 2000024, 518, 20003779]
    map_file = input_kernels_dir+'/test2/naifid_map.csv'

    for i in range(len(searches)):
        naifid = km.query_naifid_map(searches[i], naifid_map_file=map_file)
        assert isinstance(naifid, int)
        assert naifid == results[i]

    with pytest.raises(RuntimeError):
        naifid = km.query_naifid_map('This test string should fail', naifid_map_file=map_file)

def test_update_name_naifID_map():
    outdir = output_kernels_dir+'/test6'
    naifid_map_file = outdir+'/naifid_map.csv'
    indir = input_kernels_dir+'/test2'
    
    try:
        os.remove(naifid_map_file)
    except FileNotFoundError:
        pass
    
    if not os.path.exists(outdir):
        os.makedirs(outdir)

    assert os.path.exists(outdir)
    assert not os.path.exists(naifid_map_file)

    fileout = km.update_name_naifID_map(naifid_map_file=naifid_map_file, kernels_dir=indir)
    print(fileout)

    assert os.path.exists(fileout)

    reference_naifid_map_file = indir+'/naifid_map.csv'
    reference_map = np.genfromtxt(reference_naifid_map_file, delimiter=',', names=True, 
                                  encoding='utf-8', dtype=['U32', int])
    
    for i in range(len(reference_map['name'])):
        name = reference_map['name'][i]
        naifid = km.query_naifid_map(name, naifid_map_file=naifid_map_file)
        assert naifid == reference_map['naifid'][i]

def test_update_small_body_kernel():
    outdir = output_kernels_dir+'/test7'
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    naifids = [20000001, 20000002, 20059980, 20000052, 20003779]

    spks = ['20000001.bsp', '20000002.bsp', '20059980.bsp', '20000052.bsp', '20003779.bsp']
    
    for i in range(len(naifids)):
        km.update_small_body_kernel(naifids[i], kernels_dir=outdir)
        assert os.path.exists(outdir+'/spk/'+spks[i])

def test_get_body_type():
    naifids = [499, 401, 1000132, 20003779]
    types = ['Planet', 'Satellite', 'Comet', 'Minor']

    for i in range(len(types)):
        body_type = km.get_body_type(naifids[i])
        assert body_type == types[i]

def test_query_sbdb():
    # Test many different ways of returning the same object.
    queries = ['3779', 'kieffer', '3779 kieffer', '1985jv1', '1985 jv1', '20003779']
    for query in queries:
        naifid = km.query_sbdb(query)
        assert isinstance(naifid, int)
        assert naifid == 20003779

    # Test that queries returning multiple results will raise an error
    with pytest.raises(RuntimeError):
        naifid = km.query_sbdb('AA*')
    
    # Test that queries returning no results will raise an error
    with pytest.raises(RuntimeError):
        naifid = km.query_sbdb('This test string will fail')

def test_get_naifid():
    indir = input_kernels_dir+'/test2'
    outdir = output_kernels_dir+'/test8'
    default_mk = indir+'/mk/krc_default.tm'
    naifid_map_file = outdir+'/naifid_map.csv'

    # start with fresh naifid file from test assets
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    os.makedirs(outdir)
    shutil.copy(indir+'/naifid_map.csv', naifid_map_file)
    

    bodies = ['Mars', 'Phobos', 'Kore', 'Kieffer', 'ceres']
    naifids = [499, 401, 549, 20003779, 2000001]

    for i in range(len(bodies)):
        naifid = km.get_naifid(bodies[i], default_mk=default_mk, naifid_map_file=naifid_map_file)
        print(f'body: {bodies[i]}, naifid: {naifid}')
        assert naifid == naifids[i]

    # this should fail to find a naifid.
    with pytest.raises(RuntimeError):
        naifid = km.query_naifid_map('1985 JV1', naifid_map_file=naifid_map_file)

    naifid = km.get_naifid('1985 JV1', default_mk=default_mk, naifid_map_file=naifid_map_file)
    assert naifid == 20003779

    # 1985 JV1 should now be added to naifid map file
    naifid = km.query_naifid_map('1985 JV1', naifid_map_file=naifid_map_file)
    assert naifid == 20003779

def test_make_sb_mk():
    indir = input_kernels_dir+'/test2'
    outdir = output_kernels_dir+'/test9'
    default_mk = indir+'/mk/krc_default.tm'
    naifid_map_file = outdir+'/naifid_map.csv'
    kernels_dir = outdir

    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    os.makedirs(outdir)
    shutil.copy(indir+'/naifid_map.csv', naifid_map_file)

    # case 1: input naifid not small body
    with pytest.raises(RuntimeError):
        km.make_sb_mk('Europa', default_mk=default_mk, naifid_map_file=naifid_map_file, kernels_dir=kernels_dir)
    
    # case 2-#:
    # Note that Ceres and 52 Europa have their NAIF IDs set by default in SPICE, using 7-digit naifids,
    # while horizons and the SBDB return 8 digit codes. 
    # I believe this discrepancy will be resolved eventually as later SPICE versions are released??
    # Note also that because 'CERES' and '52 EUROPA' are known bodies to the SPICE system, 
    # Those bodies will not reach the inner portion of get_naifid() that appends to the 
    # naifid_map_file. Testing those bodies (or any other similar case) will fail the final
    # assert in this loop. 
    bodies = ['Kieffer', '1985jv1', '3779 Kieffer', '1985 JV1']
    naifids = [20003779, 20003779, 20003779, 20003779]
    for i in range(len(bodies)):
        mk_path = km.make_sb_mk(bodies[i], default_mk=default_mk, naifid_map_file=naifid_map_file, kernels_dir=kernels_dir)
        assert mk_path == kernels_dir+f'/mk/{naifids[i]:09d}.tm'

        kernel_list = km.read_mk(metakernel=mk_path)
        spk_path = kernels_dir+'/'+kernel_list[-1]
        assert os.path.exists(spk_path)

        naifid = km.query_naifid_map(bodies[i], naifid_map_file=naifid_map_file)
        assert naifid == naifids[i]

def test_update_satellite_kernel():
    outdir = output_kernels_dir+'/test10'
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    spk_path = km.update_satellite_kernel('Deimos', kernels_dir=outdir)
    assert os.path.exists(outdir+'/'+spk_path)

    spk_path = km.update_satellite_kernel('S/2023_S_60', kernels_dir=outdir)
    assert os.path.exists(outdir+'/'+spk_path)

    spk_path = km.update_satellite_kernel('Deimos', kernels_dir=outdir)
    assert os.path.exists(outdir+'/'+spk_path)

    with pytest.raises(RuntimeError):
        spk_path = km.update_satellite_kernel('This string will fail', kernels_dir=outdir)


def test_make_satellite_mk():
    outdir = output_kernels_dir+'/test11'
    default_mk = input_kernels_dir+'/test2/mk/krc_default.tm'
    naifid_map_file = input_kernels_dir+'/test2/naifid_map.csv'
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    satellites  = ['Deimos', 'Phobos', 'S/2023_S_60']
    naifids     = [402, 401, 65300]
    for i in range(len(satellites)):
        mk_path = km.make_satellite_mk(satellites[i], default_mk=default_mk, kernels_dir=outdir, naifid_map_file=naifid_map_file)
        assert mk_path == outdir+f'/mk/{naifids[i]:09d}.tm'

        kernel_list = km.read_mk(metakernel=mk_path)
        spk_path = outdir+'/'+kernel_list[-1]
        assert os.path.exists(spk_path)


def test_cached_mk_exists():
    # Case 1: mk exists
    exists = [2000024, 2000052, 20003779]
    for naifid in exists:
        assert km.cached_mk_exists(naifid, kernels_dir=input_kernels_dir+'/test2')
    
    # Case 2: mk does not exist
    not_exists = [7, 20001234, 499]
    for naifid in not_exists:
        assert not km.cached_mk_exists(naifid, kernels_dir=input_kernels_dir+'/test2')

def test_get_cached_mk():
    naifids = [2000024, 2000052, 20003779]
    mks = [input_kernels_dir + '/test2/mk/002000024.tm',
           input_kernels_dir + '/test2/mk/002000052.tm',
           input_kernels_dir + '/test2/mk/020003779.tm']
    for i in range(len(naifids)):
        mk_path = km.get_cached_mk(naifids[i], kernels_dir=input_kernels_dir+'/test2')
        assert mk_path == mks[i]

def test_get_mk():
    indir = input_kernels_dir+'/test2'
    outdir = output_kernels_dir+'/test12'
    default_mk = outdir+'/mk/krc_default.tm'
    naifid_map_file = outdir+'/naifid_map.csv'

    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    os.makedirs(outdir+'/mk')
    shutil.copy(indir+'/naifid_map.csv', naifid_map_file)
    shutil.copy(indir+'/mk/krc_default.tm', default_mk)

    os.makedirs(outdir+'/lsk')
    os.makedirs(outdir+'/pck')
    os.makedirs(outdir+'/spk')
    shutil.copy(input_kernels_dir+'/test1/lsk/naif0012.tls', outdir+'/lsk/naif0012.tls')
    shutil.copy(input_kernels_dir+'/test1/pck/pck00010.tpc', outdir+'/pck/pck00010.tpc')
    shutil.copy(input_kernels_dir+'/test1/spk/de442.bsp', outdir+'/spk/de442.bsp')

    shutil.copy(input_kernels_dir+'/test3/mk/020003779.tm', outdir+'/mk/020003779.tm')

    # Case: Cached metakernel exists
    mk = porb.get_mk('Kieffer', update_kernels=False, kernels_dir=outdir, default_mk=default_mk, naifid_map_file=naifid_map_file)
    assert mk == f'{outdir}/mk/020003779.tm'
    # when using a cached metakernel, the underlying kernels are not updated.
    assert not os.path.exists(outdir+'/spk/20003779.bsp')
    
    # Case: body_type is 'Planet'
    mk = porb.get_mk('Mars', update_kernels=False, kernels_dir=outdir, default_mk=default_mk, naifid_map_file=naifid_map_file)
    assert mk == f'{outdir}/mk/000000499.tm'
    assert os.path.exists(outdir+'/spk/mar099s.bsp')

    # Case: body_type is 'Satellite'
    mk = porb.get_mk('phobos', update_kernels=False, kernels_dir=outdir, default_mk=default_mk, naifid_map_file=naifid_map_file)
    assert mk == f'{outdir}/mk/000000401.tm'
    assert os.path.exists(mk)

    # Case: small body (body_type is 'Comet' or 'Minor')
    mk = porb.get_mk('Kieffer', update_kernels=True, kernels_dir=outdir, default_mk=default_mk, naifid_map_file=naifid_map_file)
    assert mk == f'{outdir}/mk/020003779.tm'
    assert os.path.exists(outdir+'/spk/20003779.bsp')




