#! /bin/bash/python

# Manages spice kernels for pyorb system.


import numpy as np
import spiceypy as spice
import datetime
import os.path as path
import glob
import re
import json
import base64
import requests

from urllib.request import urlretrieve
from bs4 import BeautifulSoup

import constants as const
import defaults 

# set some locations
kernels_dir = '/home/nsmith/KRC/pyorb/kernels'
naif_source = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels'

def download_target(target:str, dest='None') -> str:
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
    elif dest != 'None':
        destination = dest
    else:
        destination = f'/tmp/{basename}'
        ### probably should thrown an exception here?
        print('I just threw that download in /tmp/ for some reason!')

    try:
        urlretrieve(target, destination)
    except Exception as e:
        print(f'Error downloading {basename}: {e}')

    return destination

def update_naif_kernel(source:str, regex:str) -> str:
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

def update_default_kernels():
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
    
        current = update_naif_kernel(source, regex)
        default_kernel_list.append(f'{kernel_type}/{current}')
    
    default_mk = f'{kernels_dir}/mk/krc_default.tm'
    write_metakernel(default_kernel_list, default_mk)

    return

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

    satellite_source = f'{naif_source}/spk/satellites/'
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

    return newest

def update_small_body_kernel(sb_search_str: str) -> tuple[int, str]:
    '''
    Downloads a fresh kernel from Horizons for a small body. sb_search_str should be a
    name, IAU number, or NAIF ID uniquely identifying the body of interest. 
    However, the exact query term being used here can do more tricks than this function
    assumes. For a full description of usage, see the JPL Horizons documentation:
    https://ssd-api.jpl.nasa.gov/doc/horizons.html#command

    Note that this function hard-codes the ";" in the query, forcing a search only over 
    small bodies (i.r., excluding planets and moons).

    When using a provisional designation (e.g., 1999 SG6), the search string is case-
    sensitive (possibly because of the space?). Otherwise, 'ceres', 'Ceres', and 'CERES'
    all match 1 Ceres.

    :param sb_search_str: Name, IAU number, or NAIF ID of target body.
    :type sb_search_str: str
    :return: 
            - exit code: 
                - 0: SPK generated successfully.
                - 1: valid request, error generating spk. 
                - 2: Some other error.
            - spk_filename: 
                - filename of the generated spk, assuming it was produced successfully.
    :rtype: tuple[int, str]
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
            return 0, spk_filename
        
        # Otherwise, the SPK file was not generated so output an error:
        print("ERROR: SPK file not generated")
        if "result" in data:
            print(data["result"])
        else:
            print(response.text)
        return 1, 'None'

    # If the request was invalid, extract error content and display it:
    if (response.status_code == 400):
        data = json.loads(response.text)
        if "message" in data:
            print(f"MESSAGE: {data['message']}")
        else:
            print(json.dumps(data, indent=2))

    # Otherwise, some other error occurred:
    print("response code: {0}".format(response.status_code))
    return 2, 'None'


def update_all_kernels():
    '''
    seems useful
    '''
    # TODO

    return

def update_needed(kernel):
    '''
    Returns boolean indicating if a kernel needs updating.
    '''
    # TODO?

    return

def write_metakernel(kernel_list, filename):
    '''
    Writes a metakernel. Items in kernel_list should be the path of each kernel to 
    include, relative to kernels_dir, e.g.:
    ["lsk/naif0012.tls", "pck/pck00010.tpc", "spk/de440.bsp"]
    filename should be the full path of the output metakernel file. 
    
    :param kernel_list: list of kernels to write to metakernel.
    :type kernel_list: list (of strs)
    :param filename: Full path of metakernel to write.
    :type filename: str
    '''

    header = f'    Metakernel for use with KRC.\n' \
             f'    Kernels up-to-date as of {datetime.datetime.now().strftime('%Y.%m.%d')}\n' \
             f'    Generated by kernel_mgmt.py\n'
    
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

    return

def main():
    '''
    Currently just in use for testing.
    '''

    # update_default_kernels()
    
    # satellites = ['phobos', 'Io', 'Europa']
    # for satellite in satellites:
    #     current = update_satellite_kernel(satellite)

    small_bodies = ['Ceres', 'CERES', 'cErEs', '1', '269', 'europa', '1999 sg6', '1999sg6', '1999 SG6', 'mars']
    for sb in small_bodies:
        exitcode, spkname = update_small_body_kernel(sb)

    return

if __name__ == '__main__':
    main()

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