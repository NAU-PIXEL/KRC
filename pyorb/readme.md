# Pyorb
Pyorb is a reimplementation and replacement for the Porb system in KRC. 
It is used to generate the planetary parameters matrix used in KRC input files, and to generate the default parameters for the Davinci and Python interfaces, on a per-body basis.
This reimplementation is of the calculations and outputs of the original Porb system, and does not reimplement the interactive command line interface.
Instead, Pyorb provides a simple command line tool, and a more extensive Python interface, which can be used to generate the required parameters for the KRC interfaces.

## Installation
Pyorb is designed for use as a Python module, but is currently only distributed with the main KRC repository, not as a PyPI package.
To install Pyorb from source, clone the KRC repository, create or activate the desired virtual environment, navigate to `krc/pyorb/`, and run: 
> `pip install -e .`

This will install Pyorb as a local package, and will make the `pyorb` command available while the virtual environment is active.

### Setup
Before using Pyorb for the first time, you must first run a script to configure the paths for two locations used by the system: 
    1. The Davinci PORB defaults HDFs cache, where HDFs are output by default. 
    2. The kernels cache, where SPICE kernels are managed.

To do this, activate the virtual environment, then run this command:
> `python -m pyorb.setup`

The script will prompt the user for the path to the Davinci library directory (e.g., `/usr/share/davinci/library/`) and the path to a location for the kernels cache directory. 
These locations will be stored in a config file in the user's home directory.
Once the user specifies these locations, the script will also initialize these directories with the appropriate contents, if they are not already prepared.

Note that, because the config files are local, each user must run the setup command before using Pyorb, even if they are sharing the same virtual environment.

## Usage
### Command Line
To use the command line, ensure that the virtual environment is active, and run `pyorb` to verify that the command is available.

To download a body, run `pyorb update <body_name>` , where body_name should be the body's name or alphanumeric IAU provisional designation (case-insensitive).
Note that you can't use any designation that's entirely numeric for the body_name, to prevent potential collisions with object NAIF ID codes. 
For small bodies, this means the object's IAU number should not be used, except when paired with its name. 
For example, the following strings are each valid identifiers, and all map to the same object:
```
    Kieffer
    kIeFfEr
    3779 kieffer
    1985jv1
    1985 jv1
```
If searching using both the number and name of an object, e.g. `3779 Kieffer`, the space must be included. 
If searching a provisional designation, e.g. `1985 JV1`, the space is optional.

To use custom orbital orbital parameters based off a body, run `pyorb custom <body_name>` , and specify the parameters you are changing with `--<parameter> <value>`, e.g. `pyorb custom Mars --inclination 0.125`.
Any number of parameters can be changed this way, by adding more `--<parameter> <value>` pairs.

To update all HDFs in the default HDF directory, run `pyorb update_all`. 

### Python Interface
<!-- @nmsplanets could you explain the way you envision basic usage of the direct Python interface? -->
<!-- The group also needs to discuss how this could be used with Haberle's Python wrapper around KRC -->