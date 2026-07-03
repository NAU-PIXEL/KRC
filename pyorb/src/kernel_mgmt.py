#! /bin/bash/python

# Manages spice kernels for pyorb system.


import numpy as np
import spiceypy as spice
import datetime
import os
import os.path as path
import glob
import re
import json
import base64
import requests
import sys

from urllib.request import urlretrieve
from bs4 import BeautifulSoup

from . import constants as const
from . import defaults 
from . import install

# set some locations
kernels_dir = install.kernels_dir
naif_source = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels'
default_mk = f'{kernels_dir}/mk/krc_default.tm'
naifid_map_file = f'{kernels_dir}/naifid_map.csv'
satellite_source = f'{naif_source}/spk/satellites/'

def download_target(target:str, dest:str=None, kernels_dir:str=kernels_dir) -> str:
    '''
    Downloads a target file from a specified location, handles possible errors.
    Places target file in correct kernel subdir based on file extension.

    :param target: Full URL of target file to download. 
    :type target: str
    :param dest: Optional. Full path of location to download target to. Default is 
    'None', which sorts .tls, .tpc, and .bsp kernels into their appropriate locations, 
    or otherwise puts the file in /tmp/. 
    :return: Full path of location of downloaded file. 
    :rtype: str
    '''
    root, ext = path.splitext(target)
    basename = path.basename(target)

    write_dir = {'.tls' : f'{kernels_dir}/lsk',
                 '.tpc' : f'{kernels_dir}/pck',
                 '.bsp' : f'{kernels_dir}/spk'}
    
    if ext in write_dir.keys():
        destination = f'{write_dir[ext]}/{basename}'
    elif dest is not None:
        destination = dest
    else:
        destination = f'/tmp/{basename}'
        ### probably should throw an exception here?
        print('I just threw that download in /tmp/ for some reason!')

    if not path.isdir(path.dirname(destination)):
        os.mkdir(path.dirname(destination))

    try:
        urlretrieve(target, destination)
    except Exception as e:
        print(f'Error downloading {basename}: {e}')

    return destination

def update_naif_kernel(source:str, regex:str, kernels_dir=kernels_dir) -> str:
    '''
    Checks a naif source location for updated kernels, compares them to latest currently
    available kernel, and if necessary, downloads the updated version.
    
    :param source: Full URL of the NAIF source directory to check.
    :type source: str
    :param regex: regular expression fully matching the desired kernel for all version numbers.
    :type regex: str
    :return: basename of updated current kernel.
    :rtype: str
    '''
    kernel_type = regex[-3:-1]+'k'
    ### search the source location, get the index of files
    index = download_target(source, dest='/tmp/index.html')

    with open(index) as fp:
        soup = BeautifulSoup(fp, 'html.parser')
    
    ### determine which files match the file of interest
    matches = soup.find_all("a", string=re.compile(regex))
    match_strings = []
    for match in matches:
        match_strings.append(match.text)

    ### determine the newest file available from the source
    newest = sorted(match_strings)[-1]

    ### determine which file in our kernels dir is newest currently available. 
    all_current = np.array(glob.glob(f"{kernels_dir}/{kernel_type}/*"))              
    is_a_match=[bool(re.fullmatch(regex, path.basename(i))) for i in all_current]
    
    if len(all_current[is_a_match])>0:
        current = path.basename(np.sort(all_current[is_a_match])[-1]) 
    else: current = 'None'

    ### compare newest available to current existing in kernels_dir
    if current != newest:
        download_target(f'{source}{newest}')
        print(f'updated to {newest} from {current}.')
        current = newest
    else: print(f'{current} is already up to date.')

    return current

def update_default_kernels(default_mk:str=default_mk, kernels_dir:str=kernels_dir):
    '''
    Checks canonical sources for updated versions of the following common kernels:
    Leap Seconds Kernel (LSK): naif####.tls
    Planetary Constants Kernel (PCK): pck#####.tpc
    Solar System SPK: de###.bsp
    Once these kernels are downloaded, the default metakernel is updated with the new 
    kernel names.
    '''

    kernel_names = {'lsk'            : 'naif\\d{4}\\.tls$', 
                    'pck'            : 'pck\\d{5}\\.tpc$', 
                    'spk/planets'    : 'de\\d{3}\\.bsp$'}
    
    default_kernel_list=[]
    
    for kernel in kernel_names.keys():
        source = f'{naif_source}/{kernel}/'
        regex = kernel_names[kernel]
        kernel_type = regex[-3:-1]+'k'
    
        current = update_naif_kernel(source, regex, kernels_dir=kernels_dir)
        default_kernel_list.append(f'{kernel_type}/{current}')
    
    write_metakernel(default_kernel_list, default_mk)

    return

def update_name_naifID_map(naifid_map_file:str = naifid_map_file) -> str:
    '''
    create a fresh name-naifid mapping file, with all available satellites,
    and all currently downloaded small bodies.
    '''
    # Use the summary file to list all covered satellites and their NAIF IDs.
    summary = download_target(f'{satellite_source}/aa_summaries.txt', dest='/tmp/summary.txt')
    with open(summary, 'r') as s:
        summary_lines = s.readlines()

    satellite_IDs = {}
    for line in summary_lines:
        if 'w.r.t.' in line:
            pair = line.split('w.r.t.')[0].split(':')[-1].strip().strip(')').split(' (')
            name = pair[0]
            naifid = int(pair[-1])
            satellite_IDs[name] = naifid
    
    with open(naifid_map_file, 'w') as f:
        f.write('name,naifid\n')
        f.writelines([f'{name},{naifid}' for name,naifid in iter(satellite_IDs)])


    # Add lines for any small bodies that currently have metakernels.
    small_body_lines = []
    all_metakernels = glob.glob(f'{kernels_dir}/mk/*.tm')
    for mk in all_metakernels:
        naifid = int(mk.split('/')[-1][:-3])
        if naifid >= 100000:
            with open(mk, 'r') as m:
                name = m.readlines()[3]
            small_body_lines.append(f'{name.upper()},{naifid}')
    
    with open(naifid_map_file, 'a') as f:
        f.writelines(small_body_lines)

    return naifid_map_file

def update_satellite_kernel(satellite:str) -> str:
    '''
    Checks canonical sources for updated versions of SPK for a planetary system. 
    Working as of 2025.11.25 for all listed satellites. I suspect over time, NAIF
    naming conventions will evolve, and this may need updating.
    
    :param satellite: Name of the target satellite to update the SPK for.
    :type satellite: str
    :return: basename of updated latest kernel.
    :rtype: str
    '''
    ### Uff da none of these things are gonna be easy, are they?
    ### So, looking at the source folder, it's a total mess of options. The best way
    ### to get what I want seems to be, check the aaa_summaries.txt, parse the output
    ### and find what file/ files cover whatever moon I'm interested in (not planet),
    ### then, that doesn't narrow it down entirely.

    ### as of 2025.11.25, the source folder contains these satellite spks:
    ### jupiter, two options, no overlaps.
    ### mars, two overlapping options, short time range has "s" appended.
    #   (though, that's a new convention, who knows if they'll stick with it.)
    ### neptune, nep095.bsp has all satellites, short time range, possibly high res, based on file size.
    #   nep097.bsp: triton, nep104.bsp: minor satellites, nep105.bsp: nereid, all medium time range. 
    #   then, several xl options with massive time ranges I don't want. 
    ### pluto, one option. great.
    ### Saturn, non-overlapping: sat393_daphnis, sat415, sat441, sat455, sat456, sat457
    #   two xl options (past and future) for the major satellites (sat441)
    ### Uranus, 7 xl options (individual bodies, those in ura111, 
    #   and then some small ones from 116xl, which doesn't have a non-xl version)
    #   then, ura184_part-1, 2, & 3, which are non-overlapping and only two centuries. 

    ### so the strategy should be, check the summary, get all kernels containing target satellite,
    #   (can chuck all the "xl" ones) 
    #   then select the one with the smallest file size. (need to parse that from the index page)
    #   That always gets me one file, containing what I need. 

    ### I need to somehow track what version is most up to date, and compare with what I have.
    #   unfortunately, the numbers associated with each file are not version numbers, 
    #   or at least can't be treated that way. So the only knowledge I have of if a version
    #   is up to date is if it matches the canonical source. 

    ### Also unfortunately, I can't use my "update_naif_kernel()" function to do all that, 
    #   so it will only ever be called for updating the defaults. 

    
    satellite = satellite.upper()

    # Use the summary file to determine which spks from NAIF have coverage of our target.
    summary = download_target(f'{satellite_source}/aa_summaries.txt', dest='/tmp/summary.txt')
    with open(summary, 'r') as s:
        summary_lines = s.readlines()
    
    # parse the summary file for the line indices naming each spk.
    spk_indices = []
    spks = []
    for i, line in enumerate(summary_lines):
        if 'Summary for:' in line:
            spk_indices.append(i)
            spks.append(line.split(' ')[-1])
    spk_indices.append(len(summary_lines))

    # for each named SPK, check if that SPK contains our target.
    spks_containing_target = []
    for j in range(len(spk_indices) - 1):
        for i in range(spk_indices[j], spk_indices[j+1]):
            if f' {satellite} ' in summary_lines[i]:
                spks_containing_target.append(spks[j].strip())
    
    # throw an error if the target wasn't in any of the spks.
    try: 
        assert spks_containing_target > 0
    except AssertionError as e:
        print(f'Error: No spks found containing target from source {satellite_source}')
        print(f'Double-check the target is a valid planetary satellite?')
        print(f'{e}')

    ### search the source location, get the index of files
    index = download_target(satellite_source, dest='/tmp/index.html')

    with open(index) as fp:
        soup = BeautifulSoup(fp, 'html.parser')
    
    ### get the file size of each spk of interest.
    table = soup.find('pre')
    bsps = table.find_all('a', string=re.compile('.*\\.bsp$'))

    file_sizes=[]
    for bsp in bsps:
        size_str = bsp.next_sibling.string.strip().split(' ')[-1]
        if size_str[-1:]=='K':
            multiplier = 1e3
        elif size_str[-1:]=='M':
            multiplier = 1e6
        elif size_str[-1:]=='G':
            multiplier = 1e9
        else: multiplier = 1

        file_sizes.append(float(size_str[:-1])*(multiplier))
    
    # determine the filesizes of each spk from the source which contains our target.
    contains_target = []
    for b in bsps:
        contains_target.append(b.string in spks_containing_target)

    file_sizes = np.ma.masked_array(file_sizes, mask=(np.logical_not(contains_target)))

    # newest is the basename of the spk we want, the latest available from the 
    # canonical source with coverage of our target, at the smallest filesize.
    newest = bsps[np.argmin(file_sizes)].string

    ### determine if this file is already available in our kernels dir 
    fullpath_current = np.array(glob.glob(f"{kernels_dir}/spk/*.bsp"))
    all_current = []
    for spk in fullpath_current:
        all_current.append(path.basename(spk))

    ### download the desired file, if it is not already up to date.
    if newest not in all_current:
        download_target(f'{satellite_source}{newest}')
        print(f'updated satellite spk to {newest}.')
    else: print(f'{newest} is already up to date.')

    return f'spk/{newest}'

def update_small_body_kernel(sb_search_str: str) -> str:
    '''
    Downloads a fresh kernel from Horizons for a small body. sb_search_str should be a
    name, IAU number, or NAIF ID uniquely identifying the body of interest. 
    However, the exact query term being used here can do more tricks than this function
    assumes. For a full description of usage, see the JPL Horizons documentation:
    https://ssd-api.jpl.nasa.gov/doc/horizons.html#command

    Note that this function hard-codes the ";" in the query, forcing a search only over 
    small bodies (i.e., excluding planets and moons).

    When using a provisional designation (e.g., 1999 SG6), the search string is case-
    sensitive (possibly because of the space?). Otherwise, 'ceres', 'Ceres', and 'CERES'
    all match 1 Ceres.

    :param sb_search_str: Name, IAU number, or NAIF ID of target body.
    :type sb_search_str: str
    :return: filename of the generated spk
    :rtype: str
    '''

    # Define API URL and SPK filename:
    url = 'https://ssd.jpl.nasa.gov/api/horizons.api'
    spk_path = f'{kernels_dir}/spk'
    spk_filename = f'{spk_path}/default_horizons_spk.bsp'

    # Define the time span:
    start_time = '2024-06-01'
    stop_time = '2025-06-01'

    # Build the appropriate URL for this API request:
    # IMPORTANT: You must encode the "=" as "%3D" and the ";" as "%3B" in the
    #            Horizons COMMAND parameter specification.
    url += "?format=json&EPHEM_TYPE=SPK&OBJ_DATA=NO"
    url += f"&COMMAND='{sb_search_str}%3B'&START_TIME='{start_time}'&STOP_TIME='{stop_time}'"

    # Submit the API request and decode the JSON-response:
    response = requests.get(url)
    try:
        data = json.loads(response.text)
    except ValueError:
        print("Unable to decode JSON results")

    # If the request was valid...
    if (response.status_code == 200):
        # If the SPK file was generated, decode it and write it to the output file:
        if "spk" in data:
            # If a suggested SPK file basename was provided, use it:
            if "spk_file_id" in data:
                spk_filename = f'{spk_path}/{data["spk_file_id"]}.bsp'
            try:
                f = open(spk_filename, "wb")
            except OSError as err:
                print(f"Unable to open SPK file '{spk_filename}': {err}")
            # Decode and write the binary SPK file content:
            f.write(base64.b64decode(data["spk"]))
            f.close()
            print(f"wrote SPK content to {spk_filename}")
            return f'spk/{path.basename(spk_filename)}'
        
        # Otherwise, the SPK file was not generated so output an error:
        else:    
            print("ERROR: SPK file not generated")
            if "result" in data:
                print(data["result"])
            else:
                print(response.text)
            raise RuntimeError('"spk" not in decoded JSON. SPK file not generated.')
        
    # If the request was invalid, extract error content and display it:
    if (response.status_code == 400):
        data = json.loads(response.text)
        if "message" in data:
            print(f"MESSAGE: {data['message']}")
        else:
            print(json.dumps(data, indent=2))

    # Otherwise, some other error occurred:
    print("response code: {0}".format(response.status_code))
    raise RuntimeError(f'Invalid request: {url} \nMaybe "{sb_search_str}" is a bad sb_search_str?')

def write_metakernel(kernel_list: list, naifid: int, outdir:str=f'{kernels_dir}/mk', kernels_dir:str=kernels_dir, comments: str = ''):
    '''
    Writes a metakernel. Items in kernel_list should be the path of each kernel to 
    include, relative to kernels_dir, e.g.:
    ["lsk/naif0012.tls", "pck/pck00010.tpc", "spk/de440.bsp"] 
    
    :param kernel_list: list of kernels to write to metakernel.
    :type kernel_list: list (of strs)
    :param outdir: Path of directory to write metakernel to.
    :type outdir: str
    :param kernels_dir: Path of directory containing kernels.
    :type kernels_dir: str
    :param naifid: NAIF ID code identifying the object.
    :type naifid: int

    '''
    if not path.isdir(outdir):
        os.mkdir(outdir)

    filename = f'{outdir}/{naifid:09d}.tm'

    header = f'Metakernel for use with KRC.\n' \
             f'Object: \n' \
             f'{spice.bodc2s(naifid)}\n' \
             f'Kernels up-to-date as of {datetime.datetime.now().strftime("%Y.%m.%d")}\n' \
             f'Generated by kernel_mgmt.py\n'
    
    if comments != '':
        header+= comments
    
    with open(filename, 'w') as mk:
        mk.write("\\begintext\n")
        mk.write(header)
        mk.write("\\begindata\n")
        mk.write("PATH_VALUES=(\n")
        mk.write(f"    '{kernels_dir}',\n")
        mk.write(")\n")
        mk.write("PATH_SYMBOLS=(\n")
        mk.write("    'k',\n")
        mk.write(")\n")
        mk.write("KERNELS_TO_LOAD=(\n")
        for kernel in kernel_list:
            mk.write(f"'$k/{kernel}',\n")
        mk.write(")\n")

    return filename

def read_default_mk():
    '''
    Get a list of up to date default kernels from the default metakernel.
    '''

    with open(default_mk, 'r') as f:
        lines = f.readlines()
    default_kernels = []
    for line in lines:
        if line[:3] == "'$k":
            default_kernels.append(line[4:-3])

    return default_kernels

def testing():
    '''
    Currently just in use for testing.
    '''

    # update_default_kernels()
    default_kernel_list = read_default_mk()

    satellites = ['phobos', 'Io', 'Europa']
    for satellite in satellites:
        current = update_satellite_kernel(satellite)
        kernel_list = default_kernel_list + [current]
        # write_metakernel(kernel_list, f'{kernels_dir}/mk/{satellite.upper()}.tm')



    small_bodies = ['Ceres', 'CERES', 'cErEs', '1', '269', 'europa', '1999 sg6', '1999sg6', '1999 SG6', 'mars']
    for sb in small_bodies:
        spkname = update_small_body_kernel(sb)
        kernel_list = default_kernel_list + [spkname]
        # write_metakernel(kernel_list, f'{kernels_dir}/mk/{sb.upper()}.tm')

    kernel_list = read_default_mk()
    return

def make_sb_mk(sb_search_str:str):
    '''
    sb_search_str should be a name, IAU number, or NAIF ID uniquely identifying the body of interest.
    '''
    # update_default_kernels()
    default_kernel_list = read_default_mk()

    sb=sb_search_str
    spkname = update_small_body_kernel(sb)
    kernel_list = default_kernel_list + [spkname]
    naifid = get_naifid(sb)
    
    spice.furnsh(f'{kernels_dir}/{spkname}')

    # add entry in naifid map file for this object
    body_name = spice.bodc2s(naifid)
    with open(naifid_map_file, 'a') as f:
        f.writelines(f'{body_name},{naifid}')
    
    mk_path = write_metakernel(kernel_list, naifid)

    return mk_path

def query_sbdb(search_str:str) -> int:
    # Define API URL
    url = 'https://ssd-api.jpl.nasa.gov/sbdb.api'
    url += f'?sstr={search_str}'

    # Submit the API request and decode the JSON-response:
    response = requests.get(url)
    try:
        data = json.loads(response.text)
    except ValueError as err:
        print("Unable to decode JSON results")
        raise err
    
    # If the request was valid...
    if (response.status_code == 200):
        if "object" in data:
            naifid = data['object']['spkid']
            return naifid
        
        # Otherwise, output an error:
        else:    
            # not sure if this is possible.
            print("ERROR: no object data in results")
            print(json.dumps(data, indent=2))
            raise RuntimeError(f'"object" not in SBDB query results. \nMaybe "{search_str}" is a bad search_str?')

    # If the request returned multiple matches:
    if (response.status_code == 300):
        matches = data["list"]
        names = []
        for match in matches:
            names.append(match["name"])
        raise RuntimeError(f'Search string {search_str} matched multiple SBDB records:\n{names}')

    # If the request was invalid, extract error content and display it:
    if (response.status_code == 400):
        data = json.loads(response.text)
        if "message" in data:
            print(f"MESSAGE: {data['message']}")
        else:
            print(json.dumps(data, indent=2))

    # Otherwise, some other error occurred:
    print("response code: {0}".format(response.status_code))
    raise RuntimeError(f'Invalid request: {url} \nMaybe "{search_str}" is a bad search_str?')

def make_satellite_mk(satellite:str) -> str:
    default_kernel_list = read_default_mk()

    current = update_satellite_kernel(satellite)
    kernel_list = default_kernel_list + [current]

    spice.furnsh(f'{kernels_dir}/{current}')

    naifid = get_naifid(satellite)

    mk_path = write_metakernel(kernel_list, naifid)

    return mk_path

def query_naifid_map(search_str:str) -> int:
    naifid_map = np.genfromtxt(naifid_map_file, delimiter=',', names=True, encoding='utf-8',
                               dtype=['U32', int])
    
    if search_str.upper() in naifid_map['name']:
        naifid = naifid_map['naifid'][naifid_map['name']==search_str.upper()]
        return naifid
    else:
        raise RuntimeError(f'No object matching search string {search_str} found in naifid map file {naifid_map_file}')

def get_naifid(search_str:str) -> int:
    # load default mk
    spice.furnsh(default_mk)
    try:
        naifid = spice.bods2c(search_str)
    except spice.utils.exceptions.NotFoundError:
        print(f'String "{search_str}" matched no objects in default metakernel {default_mk}.')
        # if that fails, try the local naifid map file
        try:
            naifid = query_naifid_map(search_str)
        except RuntimeError as e:
            print(e)    
            # if it fails, use small body db api?
            print(f'Searching JPL Small Body Database')
            naifid = query_sbdb(search_str)

    return naifid

def cached_mk_exists(naifid:int) -> bool:
    mk_path = f'{kernels_dir}/mk/{naifid:09d}.tm'
    return path.exists(mk_path)

def get_cached_mk(naifid:int) -> str:
    mk_path = f'{kernels_dir}/mk/{naifid:09d}.tm'
    return mk_path
    
if __name__ == '__main__':
    target_name = sys.argv[1]

    # get_mk(target_name)

    # # Include headers in output?
    # verbose = True

    # body_names      = ['Ceres', 'Mars', 'Deimos', 'Didymos', 'Dimorphos', 'Chimaera']
    # body_naifids    = [20000001, 499, 402, 920065803, 120065803, 20000623]

    # # epoch at which to calculate orbital params (must be covered by available kernels)
    # epoch_date = datetime.datetime(2024,11,1,0,0,0)
    # metakernel = f'{defaults.kernels_dir}/mk/krc_default.tm'
    
    # for i in range(len(body_names)):
    #     print()
    #     print(main(body_names[i], body_naifids[i], metakernel, epoch_date, verbose=verbose))