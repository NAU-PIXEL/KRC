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

Another option, currently only implemented for body type: "Minor", will be to output an HDF containing the relevant data, which will be cached and used by the Davinci (and python?) interface.
This should conform to the existing HDF format used for this purpose. 

The user can either copy the formatted string output into a KRC input file, or use an interface to pull the necessary data from the cached output. 

# To-do
- `kernel_mgmt.py`
    - [x] `update_small_body_kernel()`: change from exit-codes to raising exceptions for invalid responses (i.e., no spk generated)
    - [ ] function to take body name/ number as a string, get the naifid.
    - [ ] function to take... I guess the naifid? and see if there's a cached metakernel for it.
    - [ ] user interface: decide what input is needed (target string, is_smallbody flag?), generate a metakernel for that input. This does not independently assess if updates are needed.
    - [ ] Some logic to only call `update_default_kernels()` once a day max, and just pull from the default metakernel otherwise when building a per-body mk? (check the mod date on the default mk? read the comment line that has the date in it?)
    - [ ] function to force-update all kernels, or a list of kernels?
    - [ ] testing?

- `porb.py`
    - [x] generate formatted output as cacheable HDF (per body). 
        - [x] for body type: "Minor"
        - [x] for body type: "Planet"
            - I think I can break out non-orbital params from `planetary_params3.csv`, and just have a canonical table for dealing with atmospheres and planetshine, that then populates the HDFs. Hopefully that order of precedence makes sense. 
        - [x] for body type: "Satellite"
        - [x] for body type: "Comet"
        - [x] for body type: other/general?? could I use negative naifids for stuff like that? 
    - [ ] user interface: specify body, get a metakernel using `kernel_mgmt.py`, options to force params to user input values. 
    - [x] function to derive spin pole from obliquity and true anomaly, set spin_axis and secondary_spin_params based on that method? (seems more user friendly to have that option)
    - [ ] high-level function to take a body name, get the metakernel and naifid, (optionally updating kernels) and manage any kwargs to modify default values, then return a porb_params object.
    - [ ] testing

- `body_params.py`
    - [ ] high-level function to attach other params for writing output to a defaults hdf.
    - [ ] high-level function to run the above function for a standard list of bodies? or maybe every body already in the cache? while forcing a kernel update. 
    - [ ] high-level function to read the defaults file, extract porb_params object and other objects.
        values in those objects can then be modified. 
            (not recommended to modify from cached defaults directly, as linked values will not update automatically, eg semi-major axis & operiod.) preferred behavior is to construct a fresh instance of the object directly?

        objects can then:
            1. be passed as inputs to pykrc
            2. be used by a python based fortran krc interface (?)

        the defaults HDFs can be read by the existing dv interface to work with fortran krc

- other (additional rotation info)
    - [ ] Find additional source for small-body periods and spin-poles. The standard PCK doesn't have nearly enough of those. 
    - [ ] Find additional info for satellite rotation periods and spin poles. Which ones are tidally locked and which aren't would be good enough, I don't care about precession & nutation.
    - [ ] If it makes sense to manage that info with `kernel_mgmt.py`, do that. Otherwise, figure out a way to manage updating that info separately.
    - [ ] Incorporate this additional spin info into `porb.py`.

# Notes
## `planetary_params3.csv`
- column labels are swapped for orbital and sidereal periods
- orbital period is in Earth Years, sidereal period is in hours.
- inconsistent use of 0 and -999 for undefined params.
    - surface pressure=0 for airless bodies makes sense. 
    - surface pressure=-999 for surface-less bodies makes sense.
    - I don't know what ARC2_PHO is or why it's -999 for titan when it's 0 for other non-mars atmospheres.
- only defined for Mars: ARC2_G0 (ARC2_PHO), DUSTA, TAURAT
    - weird mars atmosphere model stuff
- only defined for Venus, Earth, Mars, Pluto, Titan: PTOTAL 
    - probably for surfaces with atmospheres?
    - surface pressure in Pa?
- only planets: BT_Min, BT_max, BT_avg, Geom_Alb, Dis_AU
    - maybe to do with planetshine on satellites?
    - BT: Bolometric Temperature?
- only satellites: orb_radius, mut_period
    - probably to do with planetshine on satellites? and/or eclipses.

## `porb_defaults/*.porb.hdf`
- body: name
- period: orbital period in Earth Days
- rot: the full PORB output table, as a string
- rot_per: sidereal rotation period in hours
- rot_per_flag: 1 if a rotation period is made up, 0 if it's real. 
- type:
    - body_type: Planet, Satellite, Comet, or Minor
    - id: 0 for planets and satellites. NAIFID for comets. IAU number for asteroids, called Minor.
    - name: object name, same as body.
    - parent_body: parent body name, blank for anything orbiting the Sun.
- planet_flux:
    - all values are -999 for anything that's not a planet or satellite.
    - see planetary_params3.csv "only planets" and "only satellites" notes above.
- krc: 
    - see planetary_params3.csv "only atmospheres" and "only mars" notes above.
    - ARC2_G0:  don't know. Mars atmosphere.
    - DELJUL:   Orbit period / 360, in Earth Days. 
    - DUSTA:    don't know. Mars atmosphere.
    - GRAV:     surface gravity in m/s^2. 0 for anything not in planetary_params3.csv
    - N24:      usually 96. larger for some of jupiter's moons in the examples, always a multiple of 24. Maybe things get weird if the diurnal division is too much real time? Io's 96, and that makes ~26.5 minute timesteps. The others seemed tuned to target ~15 minute timesteps. Weird there's no Luna porb default to compare with. Weird that a default N24 is specified at all rather than just computing it directly from the rotational period somewhere else!!
    - PERIOD:   sidereal rotation period in Earth Days. (why duplicate this so many places????)
    - PTOTAL:   surface atmospheric pressure (atmospheres only)
    - TAUD:     don't know. Mars atmosphere. (record only present for bennu???)
    - TAURAT:   don't know. Mars atmosphere.
    - TFROST:   don't know. Mars atmosphere. (record only present for bennu???)       

## davinci porb tools
### `porb()`
    Run PORB to calculate an appropriate rotation matrix for use in krc
        $1=body - planetary body name/Horizons id or generic_porb/exo_porb generated structure
        Name Formatting:
            For a major body (e.g. \"Mars\"), simply enter the body name
            For a satellite (e.g. \"Phobos\"), simply enter the body name
            For an asteroid (e.g. \"Bennu\"), either enter the body name or JPL Horizon id
            For a comet, enter the full name (e.g. \"1P-Halley\") to avoid ambiguity or the JPL Horizon id
    Options:
        epoch=fraction of the century for start date (Default = 0.10 -> 2010)
        force=force running of PORB (default=0)

    if not UpdateDefaults:
        get the body_type using porb_type()
        if planet or satellite:
            determine the parent body if any.
            check for an existing default.porb.hdf.
            if it's there, load that data, otherwise:
            read in standard files with orbit data.
            search for parent body in data files.
            load relevant data into structs. 
            write relevant data to temp files standish.tab, spinaxis.tab, and porb_[body].run
        else if minor:
            check for an existing default.porb.hdf
            if it's there, load that data, otherwise:
            use body name or number to grep smallbodies data file.
            load relevant data into structs.
            write relevant data to temp files minor.tab and porb_[body].run
        else if comet:
            check for an existing default.porb.hdf
            if it's there, load that data, otherwise:
            use body name or number to grep smallbodies data file.
            load relevant data into structs.
            write relevant data to temp files comet.tab and porb_[body].run
        else if Generic:
            use values in supplied struct to populate new struct.
            write relevant data to temp files minor.tab and porb_[body].run
        else if Exoplanet:
            use values in supplied struct to populate new struct.
            write relevant data to temp files exoplan.tab and porb_[body].run
        else:
            complain.
        
        Run porbmn on porb_[body].run

    if UpdateDefaults:
        use krc_porb_defaults as the list of what to update.
        run this function(!!) for each item in the list, write a [body].porb.hdf file.


### `krc_find_body()`
    > Note: never called.
    Search for the KRC body name to use
	$1=search string (Note: case sensitive)

    grep krc_porb_master for input search string.
    list the matching bodies, their names, types, and parent bodies.


### `porb_type()`
    Return the Body type structure for use with porb functions
	$1 = body - the name or JPL horizons ID # of the body of interet

    grep krc_porb_master for the input body string or int.
    return the name, id, body_type, and parent_body.
    if there's more than one, print all matches.
    if there's none, complain.

    or, if called with make_master=True:
        read all the data files
        copy their names & types, etc into a struct, and return the struct.


### `generic_porb()`
    Generate a generic PORB structure for use with the davinci porb function
    This is formatted in the minor body style for porb and permits the following values described below

    name    = body name (Default=\"None\") will be truncated to 24 characters
    epoch   = epoch in Julian Date (Default = 2451545.0, year 2000)
    a       = Semi-Major Axis in AU (Default=1)
    e       = Eccentricity (Default=0)
    i       = Inclination of mean orbit to ecliptic in degrees  (Default=0)
    node    = Longitude of the asceding node in degrees (Default=0)
    peri    = Argument of perihelion in degrees (Default=0)
    m       = Mean Anomoly at epoch in degrees (Default=0)
    polera  = Right Ascention of the pole in degrees (Default=0)
    poledec = Declination of the pole in degrees (Default=0)
    merid   = prime meridian at epoch in degrees (Default=0)
    rot_per = siderial rotation period in hours (Default=23.9345)
    period  = siderial orbital period in days (Default=365.256)

    populates a struct with default values
    These values can be substituted with whatever kwargs are supplied.
    returns the struct.

### `exo_porb()`
    Generate an exoplanet PORB structure for use with the davinci porb function
    This is formatted in the exoplanet style for porb and permits the following values described below

    name = body name (Default="None") will be truncated to 24 characters
    epoch = Time of periastron as full Julian date ; 2000 Jan 1 noon UTC= 2451545.0
    Vismag = Visual Magnitude of host star (Default = 0.)
    DisEarth = Distance from Earth to host star, in light years (Default = 0.)
    a = Semi-Major Axis, in AU (Default=1)
    period = siderial orbital period, in days (Default=365.256)
    rot_per = siderial rotation period, in hours (Default=23.9345)
    e = Eccentricity (Default=0)
    Obliq = Obliquity of planet pole, in degree (Default=0)
    Lsperi = Season (Ls) at periastron in degree (Default=0)

    Populates a struct with default values.
    These values can be substituted with whatever kwargs are supplied.
    Returns the struct.