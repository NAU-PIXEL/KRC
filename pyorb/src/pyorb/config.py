import os
import readline
import tomllib
import tomli_w

from pathlib import Path
from platformdirs import user_config_dir

src_dir = Path(__file__).parent.resolve()
install_dir = Path(src_dir.parent.parent)
os.chdir(install_dir)

# set some locations
planet_params_file = src_dir / "planet_params.csv"
# Note: Running tests will produce ~ 766 MB of output in this directory.
test_kernels_dir = install_dir / "test/kernels"

config_dir = Path(user_config_dir("pyorb"))
config_file = config_dir / "config.toml"


def load_config(config_file:Path=config_file) -> tuple[Path, Path]:    
    """
    reads the config file to get the directories for 
        1. the davinci porb default HDFs
        2. the pyorb SPICE kernel cache
    
    These locations are set by the user during installation.  

    Args:
        config_file (Path, optional): Path to the config file.

    Raises:
        FileNotFoundError: Returned when no config file is found at the expected location.  
        ValueError: Returned when the config file has porb_defaults_dir set to None
        ValueError: Returned when the config file has kernels_dir set to None
        KeyError: Returned when the config file has no value set for porb_defaults_dir
        KeyError: Returned when the config file has no value set for kernels_dir

    Returns:
        Path: Path of the porb defaults directory
        Path: Path of the SPICE kernel cache directory
    """
    try:
        with open(config_file, "rb") as f:
            config = tomllib.load(f)
    except FileNotFoundError:
        raise FileNotFoundError(
            f"Config file not found at {config_file}. Please run `python -m pyorb.config` to create the config file."
        )

    try:
        if config["porb_defaults_dir"] is None:
            raise ValueError("porb_defaults_dir must be set in config.toml")
    except KeyError:
        raise KeyError(
            f"porb_defaults_dir must be set in config.toml"
        )
    
    try:
        if config["kernels_dir"] is None:
            raise ValueError("kernels_dir must be set in config.toml")
    except KeyError:
        raise KeyError(
                f"kernels_dir must be set in config.toml"
            )

    porb_defaults_dir = Path(config["porb_defaults_dir"])
    kernels_dir = Path(config["kernels_dir"])

    return porb_defaults_dir, kernels_dir


def install_config(config_file:Path=config_file):
    """
    Prompts the user for the paths of two directories, containing:
        1. the davinci porb default HDFs
        2. the pyorb SPICE kernel cache
    
    These locations are then saved in the pyorb config.toml.

    Args:
        config_file (Path, optional): Path to the config file.
    """
    config_dir.mkdir(parents=True, exist_ok=True)

    # Set Davinci porb defaults directory.
    readline.set_completer_delims(" \t\n=")
    readline.parse_and_bind("tab: complete")
    davinci_dir = Path(
        input(
            "Enter the path to the root of the Davinci library, e.g. /usr/share/davinci/library/:"
        )
    )
    davinci_porb = davinci_dir / "script_files/krc_support/porb_defaults"

    if not davinci_porb.is_dir():
        davinci_porb.mkdir(parents=True)

    # Set kernels cache directory.
    readline.set_completer_delims(" \t\n=")
    readline.parse_and_bind("tab: complete")
    kernels_cache = Path(
        input(
            "Enter the path to the kernels cache directory, e.g., /home/<user>/KRC/pyorb/kernels/:"
        )
    )
    if not kernels_cache.is_dir():
        kernels_cache.mkdir(parents=True)

    config = {}
    config["porb_defaults_dir"] = str(davinci_porb.absolute())
    config["kernels_dir"] = str(kernels_cache.absolute())

    with open(config_file, "wb") as f:
        tomli_w.dump(config, f)
    print(f"Updated {config_file}")


if __name__ == "__main__":
    install_config()

if not Path.exists(config_file):
    install_config()

porb_defaults_dir, kernels_dir = load_config()
default_mk = f"{kernels_dir}/mk/krc_default.tm"
naifid_map_file = f"{kernels_dir}/naifid_map.csv"