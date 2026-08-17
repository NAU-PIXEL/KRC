# KRC
KRC is a 1D thermal model intended for use in modeling planetary surfaces, especially Martian environments. 
This repository holds not only the core KRC thermal model, but also several related tools, including `porbmn`, an interface to KRC in IDL, a Davinci interface to KRC and porbmn, a Python interface to KRC, and a Python reimplementation of `porbmn` with additional features.

## Quickstart
KRC is compatible and tested with macOS and Linux. Windows support is unknown, and it is recommended to use WSL (Windows Subsystem for Linux) for a Linux environment on Windows.

### Pre-requisites
Compiling the core KRC program only requires the Gnu Fortran compiler, Gnu C compiler, both version 8 or later, and the Make build system.

These can be installed on Ubuntu and Debian systems with the following commands:
```
sudo apt-get install gfortran make
```
Other distributions should have similar package names.

On macOS Homebrew can be used to install the build tools. 
```
brew install gcc make
```


### Compilation
Once these packages are installed, KRC can be compiled by running `make` in the main KRC directory.
This is all that is required for KRC to run.

KRC does not necessarily need to be added to the PATH, as most tools prefer to use the absolute path of the executable. However, if using it directly from the command line, it will be more convenient, as the input and output files must be in the local directory.

To verify that KRC is functioning correctly, enter the `docs/flux_table_example` directory, run `../../bin/krc`, and enter "krc_tab" as the name of input file, and "/" for the default output file. This example utilizes a flux table, further information in the [Flux Tables](docs/flux_tables.md) documentation. This will generate a printout of the KRC results, and a `b52` file containing the output data. This file is not human readable, and requires a separate program, such as the `process_bin52` function from the Davinci interface, to be used to view the data.

## Usage
KRC's documentation is mainly written in LaTeX, and should be compiled using the `make docs` command.

For general documentation on KRC, refer to the [Version 3.4 User Guide](doc_output/V34UG.pdf). This user guide was written for KRC v3.4.5, but is still largely accurate for version 4.0.0.

For documentation on `probmn`, refer to the [probmn documentation](doc_output/PUG.pdf).

For information on each interface, see their readmes and further documentation under interfaces/{interface_name}/README.md.

# License
Although previously not released under an open source license, the original author Hugh Kieffer agreed to release KRC under the [GNU GPLv2 license](LICENSE) in April 2026. 
