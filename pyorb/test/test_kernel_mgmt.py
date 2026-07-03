import src.defaults as defaults
import src.porb as porb
import src.kernel_mgmt as km
import src.install as install
import pytest
import filecmp
import tempfile
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

'''


kernels_dir = install.test_kernels_dir

output_kernels_dir = f'{kernels_dir}/output'
input_kernels_dir = f'{kernels_dir}/input'

def test_download_target_lsk_pck_spk():
    lsk_target = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/latest_leapseconds.tls'
    pck_target = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/a_old_versions/pck00009.tpc'
    spk_target = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/a_old_versions/de410s.bsp'

    lsk_dest = f'{output_kernels_dir}/lsk/latest_leapseconds.tls'
    pck_dest = f'{output_kernels_dir}/pck/pck00009.tpc'
    spk_dest = f'{output_kernels_dir}/spk/de410s.bsp'

    shutil.rmtree(output_kernels_dir)
    assert not os.path.exists(output_kernels_dir)

    km.download_target(lsk_target, kernels_dir=output_kernels_dir)
    km.download_target(pck_target, kernels_dir=output_kernels_dir)
    km.download_target(spk_target, kernels_dir=output_kernels_dir)

    assert os.path.exists(lsk_dest)
    assert os.path.exists(pck_dest)
    assert os.path.exists(spk_dest)

def test_download_target_specified_dest():
    target = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/aareadme.txt'
    destination = f'{output_kernels_dir}/test.txt'
    
    os.remove(destination)
    assert not os.path.exists(destination)

    km.download_target(target, dest=destination, kernels_dir=output_kernels_dir)

    assert os.path.exists(destination)

def test_update_naif_kernel():
    source = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/'
    regex = 'naif\\d{4}\\.tls$'

    # Clear out the destination directory
    dest_dir = f'{output_kernels_dir}/lsk'
    shutil.rmtree(dest_dir)
    assert not os.path.exists(dest_dir)
    
    # Case where kernel needs an update:
    # set up destination directory...
    file1 = f'{dest_dir}/naif0008.tls'
    file2 = f'{dest_dir}/naif0009.tls'
    with open(file1, 'w') as f:
        f.write('')
    with open(file2, 'w') as f:
        f.write('')

    current = km.update_naif_kernel(source, regex, kernels_dir=output_kernels_dir)

    assert os.path.exists(current) and current != file1 and current != file2

    # Clear out the destination directory
    dest_dir = f'{output_kernels_dir}/lsk'
    shutil.rmtree(dest_dir)
    assert not os.path.exists(dest_dir)

    # Case where no kernels exist:
    current = km.update_naif_kernel(source, regex, kernels_dir=output_kernels_dir)

    assert os.path.exists(current)

    # Case where no update is needed:
    current2 = km.update_naif_kernel(source, regex, kernels_dir=output_kernels_dir)

    assert current2 == current

def test_write_metakernel():
    kernel_list = ['lsk/naif0012.tls', 
                   'pck/pck00010.tpc', 
                   'spk/de442.bsp']
    naifid = 499
    outdir = output_kernels_dir+'/mk'
    indir = input_kernels_dir+'/test1'
    
    # Clear out the destination directory
    shutil.rmtree(outdir)
    assert not os.path.exists(outdir)

    mk = km.write_metakernel(kernel_list, naifid, outdir=outdir, kernels_dir=indir)

    # Check that the mk was generated successfully and written to the correct location.
    assert os.path.exists(mk)

    # Check that the header contains the correct body name.
    with open(mk, 'r') as m:
        lines = m.readlines()
    assert lines[3] == 'MARS'

    # Check that the mk can be loaded and contains the correct kernel info.
    spice.kclear()
    spice.furnsh(mk)

    n_kernels = spice.ktotal('ALL')
    assert n_kernels == 3

    loaded_kernels = []
    for i in range(n_kernels):
        kernel, _, _, _ = spice.kdata(i, 'ALL')
        loaded_kernels.append(kernel)

    assert f'{indir}/lsk/naif0012.tls' in loaded_kernels
    assert f'{indir}/pck/pck00010.tpc' in loaded_kernels
    assert f'{indir}/spk/de442.bsp'    in loaded_kernels

def test_update_default_kernels():
    default_mk = output_kernels_dir+'/mk/krc_default.tm'
    indir = input_kernels_dir+'/test1'
    outdir = output_kernels_dir

    # set up working kernels directory, ensure the outdir matches indir.
    shutil.rmtree(outdir)
    shutil.copytree(indir,outdir)

    comparison = filecmp.dircmp(indir, outdir)
    assert comparison.same_files == comparison.left_list

    # Check that a newer version of pck00010.tpc is downloaded and added to the default mk.
    # pck00010.tpc has been superseded by pck00011.tpc since 2022-12-27.
    km.update_default_kernels(default_mk=default_mk, kernels_dir=outdir)

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

    pass

def test_update_name_naifID_map():

    pass

def test_update_satellite_kernel():

    pass

def test_update_small_body_kernel():

    pass

def test_make_sb_mk():

    pass

def test_query_sbdb():

    pass

def test_make_satellite_mk():

    pass

def test_query_naifid_map():

    pass

def test_get_naifid():

    pass

def test_cached_mk_exists():

    pass

def test_get_cached_mk():

    pass




