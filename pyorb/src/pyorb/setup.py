from os import symlink, makedirs
import os.path as path

from . import config
from . import kernel_mgmt

def initial_setup():
    """
    High-level script to configure pyorb and create necessary files and directories 
    before working with pyorb for the first time. 
    """
    # configure davinci porb HDF and kernels cache directories
    if not path.exists(config.config_file):
        config.install_config()

    # create these directories if they do not yet exist
    if not path.exists(config.porb_defaults_dir):
        makedirs(config.porb_defaults_dir)

    if not path.exists(config.kernels_dir):
        makedirs(config.kernels_dir)

    # create the naifid_map.csv
    kernel_mgmt.update_name_naifID_map()

    # create the default metakernel
    kernel_mgmt.update_default_kernels()

    # copy/symlink de442.bsp into the test inputs.
    dest_file = f"{config.test_kernels_dir}/input/test1/spk/de442.bsp"
    if not path.exists(dest_file):
        solar_system_spk = f"{config.kernels_dir}/spk/de442.bsp"
        symlink(solar_system_spk, dest_file)

    # run tests, confirm they pass. 
    # TODO: add tests here?
    print("pyorb setup complete.")

    return

if __name__ == "__main__":
    initial_setup()