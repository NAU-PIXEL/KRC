import requests
import sys
import tomllib

import tomli_w

from pathlib import Path
from platformdirs import user_config_dir

folder = Path(__file__).parent.resolve()

planet_params_file = folder / "planet_params.csv"

# set some locations
# naif_source = "https://naif.jpl.nasa.gov/pub/naif/generic_kernels"
kernels_dir = "./kernels"
default_mk = f"{kernels_dir}/mk/krc_default.tm"
naifid_map_file = f"{kernels_dir}/naifid_map.csv"
test_kernels_dir = "./test/kernels"

config_dir = Path(user_config_dir("pyorb"))
config_file = config_dir / "config.toml"

def check_de442_spk():
    dest = Path(kernels_dir + "/spk")
    file_dest = dest / "de442.bsp"
    return file_dest.is_file()

def download_de442_spk():
    dest = Path(kernels_dir + "/spk")
    file_dest = dest / "de442.bsp"
    url = "https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de442.bsp"
    if not dest.is_dir():
        dest.mkdir(parents=True)
    if not file_dest.is_file():
        r = requests.get(url)
        with open(file_dest, "wb") as f:
            f.write(r.content)

def load_config() -> Path:
    # Davinci interface does not have consistent install locations, must be set by a user with a config file

    try:
        with open(config_file, "rb") as f:
            config = tomllib.load(f)
    except FileNotFoundError:
        raise FileNotFoundError(
            f"Config file not found at {config_file}. Please run `python -m pyorb.install` to create the config file."
        )

    if config["porb_defaults_dir"] is None:
        raise ValueError("porb_defaults_dir must be set in config.toml")
    porb_defaults_dir = Path(config["porb_defaults_dir"])

    return porb_defaults_dir


def install_config(install_dir: Path | None):
    config_dir.mkdir(parents=True, exist_ok=True)

    if install_dir is None:
        install_dir = Path(
            input(
                "Enter the path to the root of the Davinci library, e.g. /usr/share/davinci/library/:"
            )
        )
    davinci_porb = install_dir / "script_files/krc_support/porb_defaults"

    if not davinci_porb.is_dir():
        davinci_porb.mkdir(parents=True)

    config = {}

    config["porb_defaults_dir"] = str(davinci_porb.absolute())
    with open(config_file, "wb") as f:
        tomli_w.dump(config, f)
    print(f"Updated {config_file}")


if __name__ == "__main__":
    install_dir = sys.argv[-1]
    if install_dir != __file__:
        install_path = Path(install_dir)
    else:
        install_path = None
    install_config(install_path)
    if not check_de442_spk():
        print("Downloading DE442 SPK")
        download_de442_spk()

porb_defaults_dir = load_config()