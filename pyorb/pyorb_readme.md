# Pyorb: a python replacement for PORB
KRC's PORB subsystem is used to pre-calculate the parameters used as inputs by KRC which describe or are derived from a body's orbit and rotation. 
Originally built to replace only the KRC fortran PORB functionality, this python subsystem has grown to encompass the management of cached PORB outputs used by the KRC Davinci interface.
The latest orbital information for each target is gathered automatically from JPL's Navigation and Ancillary Information Facility (NAIF) and Solar System Dynamics Group (SSD).
SSD's Horizons system is used to query ephemerides for all small solar system bodies, with NAIF providing ephemerides for planets and their satellites.
NAIF's SPICE software is then used to compute the relevant parameters used by KRC.

# Overview
## UI/UX: to-do
Some user interface will be used to kick off updating the cached outputs.
I'm not clear currently how frequently this will happen, but either it happens on a set schedule, or every time KRC is called, or somewhere in between, like once/day per body, or whenever KRC is called with a particular flag set. 

Anyway, the user will specify a body, and if that's all they specify, the system should be able to complete the process without additional input. 
The system should interpret the input body in an unambiguous way (may need to specify target is a small body to solve the Europa problem). 
Information not available from the data sources should be supplemented with reasonable assumptions (no atmospheres on small bodies, default spin axes and periods).

The user should also be able to input parameters manually.
I'll need to work on implementing that to see exactly how that will work, but inputting some parameters will allow others to just be calculated appropriately (e.g., semimajor_axis can be used to compute orbit_period), while others won't work unless additional math is done (e.g., I'll need to write another function to take orbit_period and get semimajor_axis back), and others will conflict entirely when both are specified (e.g., orbit_period and semimajor_axis) and should throw an error. 
Some params might be required to be input as sets (e.g., pole_ra and pole_dec).

The user should be able to supply sufficient inputs to completely specify all parameters, without the need for any SPICE kernels. 
This should cover the cases of synthetic objects and exoplanets.

For version 1.0, I think having just one method of specifying inputs is fine. 
That will probably be fully specifying a set of orbital elements like so:

```
(long_of_asc_node, eccentricity, inclination, arg_of_peri, mean_anomaly, semimajor_axis, epoch_JD) = orb_elems
```

and fully specifying a set of spin axis parameters like so:

```
(rotation_period, phase_at_j2000, pole_ra, pole_dec) = spin_axis 
```

Later versions can add functionality to individually overwrite parameters and allow for alternative specifications, like supplying obliquity and true anomaly instead of pole_ra and pole_dec. 

## Processing
In the default case, the input body name will be passed to a kernel management function (part of `kernel_mgmt.py`), which will automatically generate an up-to-date metakernel (or supply an existing one).

Next, in `porb.py`, a series of SPICE calls is used to extract orbital elements and spin axis parameters.
Additional functions use these parameters to compute secondary, derived parameters. 
All the relevant output parameters are collected, and passed to an output function

## Output: to-do
Currently, output can be returned as a formatted string, conforming to relevant portion of the standard KRC input file.

Another option, not yet implemented, will be to output an HDF containing the relevant data, which will be cached and used by the Davinci (and python?) interface.
This should conform to the existing HDF format used for this purpose. 

The user can either copy the formatted string output into a KRC input file, or use an interface to pull the necessary data from the cached output. 

# To-do
- `kernel_mgmt.py`
    - [x] `update_small_body_kernel()`: change from exit-codes to raising exceptions for invalid responses (i.e., no spk generated)
    - [ ] user interface: decide what input is needed (target string, is_smallbody flag?), generate a metakernel for that input. This does not independently assess if updates are needed.
    - [ ] Some logic to only call `update_default_kernels()` once a day max, and just pull from the default metakernel otherwise when building a per-body mk? (check the mod date on the default mk? read the comment line that has the date in it?)
    - [ ] function to force-update all kernels, or a list of kernels?
    - [ ] testing?
- `porb.py`
    - [ ] generate formatted output as cacheable HDF (per body). 
    - [ ] user interface: specify body, get a metakernel using `kernel_mgmt.py`, options to force params to user input values. 
    - [ ] function to derive spin pole from obliquity and true anomaly, set spin_axis and secondary_spin_params based on that method? (seems more user friendly to have that option)
    - [ ] generate formatted output as entry in `planetary_params3.csv`?
    - [ ] testing?
- other
    - [ ] Find additional source for small-body periods and spin-poles. The standard PCK doesn't have nearly enough of those. 
    - [ ] Find additional info for satellite rotation periods and spin poles. Which ones are tidally locked and which aren't would be good enough, I don't care about precession & nutation.
    - [ ] If it makes sense to manage that info with `kernel_mgmt.py`, do that. Otherwise, figure out a way to manage updating that info separately.
    - [ ] Incorporate this additional spin info into `porb.py`.




