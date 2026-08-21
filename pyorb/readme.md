# Pyorb
Pyorb is a reimplementation and replacement for the Porb system in KRC. 
It is used to generate the planetary parameters matrix used in KRC input files, and to generate the default parameters for the Davinci and Python interfaces, on a per-body basis.
This reimplementation is of the calculations and outputs of the original Porb system, and does not reimplement the interactive command line interface.
Instead, Pyorb provides a simple command line tool, and a more extensive Python interface, which can be used to generate the required parameters for the KRC interfaces.

## Setup
Pyorb is designed for use as a Python module, but is currently only distributed with the main KRC repository, not as a PyPI package.
To install Pyorb from source, clone the KRC repository, create or activate the desired virtual environment, navigate to `krc/pyorb/`, and run `pip install -e .`.
This will install Pyorb as a local package, and will make the `pyorb` command available while the virtual environment is active.

### Davinci Setup
Pyorb must be configured to output its parameters to the correct location for the Davinci interface to discover them.
Because Davinci does not have a standard installation or library location, the user must specify this location themselves.
Pyorb provides a utility command to configure this path, `python -m pyorb.install`, which then prompts the user for the location of the Davinci library directory.
This utility will prompt the user for the location of the davinci directory, and store the path in a config file in the user's home directory.

Each user of Pyorb with Davinci, even if they are using the same virtual environment, must run `python -m pyorb.install` to specify the location of the davinci directory.

## Usage
### Command Line
To use the command line, ensure that the virtual environment is active, and run `pyorb` to verify that the command is available.

To download a body, run `pyorb update <body_name>` , where body_name should be the body's English name or <!-- @nmsplanets explain the way to specify asteroids> -->.

To use custom orbital orbital parameters based off a body, run `pyorb custom <body_name>` , and specify the parameters you are changing with `--<parameter> <value>`, e.g. `pyorb custom Mars --inclination 0.125`.
Any number of parameters can be changed this way, by adding more `--<parameter> <value>` pairs.

### Python Interface
<!-- @nmsplanets could you explain the way you envision basic usage of the direct Python interface? -->
<!-- The group also needs to discuss how this could be used with Haberle's Python wrapper around KRC -->