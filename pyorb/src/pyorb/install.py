import pathlib

folder = pathlib.Path(__file__).parent.resolve()

planet_params_file = folder / "planet_params.csv"

# PORB defaults directory for porb defaults hdfs
porb_defaults_dir = (
    "/home/csaluski/dv_krc/library/script_files/krc_support/porb_defaults"
)

# set some locations
kernels_dir = "./kernels"
naif_source = "https://naif.jpl.nasa.gov/pub/naif/generic_kernels"
default_mk = f"{kernels_dir}/mk/krc_default.tm"
test_kernels_dir = "./test/kernels"
naifid_map_file = f"{kernels_dir}/naifid_map.csv"
