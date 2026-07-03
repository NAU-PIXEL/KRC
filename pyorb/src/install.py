#! /bin/bash/python

planet_params_file = '/home/nsmith/KRC/pyorb/src/planet_params.csv'

# PORB defaults directory for porb defaults hdfs
porb_defaults_dir = '/nfs/software/davinci_install/share/davinci/library/script_files/krc_support/porb_defaults'

# set some locations
kernels_dir = '/home/nsmith/KRC/pyorb/kernels'
naif_source = 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels'
default_mk = f'{kernels_dir}/mk/krc_default.tm'
test_kernels_dir = '/home/nsmith/KRC/pyorb/test/kernels'