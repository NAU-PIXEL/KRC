import argparse

from . import interface
from . import body_params
from . import porb

def update_command(args):
    interface.update_default_hdf(args.body_name, verbose=args.verbose)

def update_all_command(args):
    interface.update_all_default_hdfs(verbose=args.verbose)

def modify_porb_command(args):
    kwargs = vars(args)
    kwargs.pop('command')
    kwargs.pop('func')
    output_dir = kwargs.pop("output_dir")
    modified_params = interface.get_and_modify_porb_params(**kwargs)
    hdf_file = body_params.high_level_write_hdf(modified_params, output_dir)
    print(f'Wrote custom HDF: {hdf_file}')

def custom_porb_command(args):
    kwargs = vars(args)
    kwargs.pop('command')
    kwargs.pop('func')
    output_dir = kwargs.pop("output_dir")
    custom_params = interface.specify_custom_porb_params(**kwargs)
    hdf_file = body_params.high_level_write_hdf(custom_params, output_dir)
    print(f'Wrote custom HDF: {hdf_file}')

def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest='command', required=True)

    update_parser = subparsers.add_parser('update', help='Update the HDF for a specified body')
    update_parser.set_defaults(func=update_command)
    update_parser.add_argument('body_name', help='Name of body to update.')
    update_parser.add_argument('-v', '--verbose', action='store_true', help='Flag to enable verbose output.')

    modify_porb_parser = subparsers.add_parser('modify', help='Generate a modified HDF for the selected body')
    modify_porb_parser.set_defaults(func=modify_porb_command)
    modify_porb_parser.add_argument('body_name', 
            help='Name of the base body to use for the custom HDF.')
    modify_porb_parser.add_argument('output_dir', 
            help='Directory into which to write the HDF file.')
    modify_porb_parser.add_argument('-v', '--verbose', action='store_true', 
            help='Flag to enable verbose output.')
    modify_porb_parser.add_argument("--long_of_asc_node", type=float,
            help='longitude of the ascending node [radians]. The angle (within the ecliptic plane) from the J2000 ICRF +X vector to this orbit\'s ascending node.')
    modify_porb_parser.add_argument("--eccentricity", type=float,
            help='eccentricity [unitless].')
    modify_porb_parser.add_argument("--inclination", type=float,
            help='inclination of the orbit with respect to the ecliptic plane. [radians].')
    modify_porb_parser.add_argument("--arg_of_peri", type=float,
            help='argument of perihelion [radians]. The angle between the orbit\'s ascending node and its periapsis.')
    modify_porb_parser.add_argument("--semimajor_axis", type=float,
            help='semimajor axis [AU].')
    modify_porb_parser.add_argument("--orbit_period", type=float,
            help='Period of the orbit in Earth days.')
    modify_porb_parser.add_argument("--perihelion_date", type=float,
            help='J2000 date (days past J2000 epoch) of previous perihelion passage.')
    modify_porb_parser.add_argument("--centuries_from_j2000", type=float,
            help='Time of reference epoch from J2000 epoch, in centuries.')
    modify_porb_parser.add_argument("--epoch_JD", type=float,
            help='Julian date of epoch [Julian Date].')
    modify_porb_parser.add_argument("--mean_anomaly", type=float,
            help='mean anomaly at epoch [radians]. Time fraction of the orbit that has elapsed since previous perihelion at the reference epoch, expressed as an angle in radians out of 2pi.')
    modify_porb_parser.add_argument("--rotation_period", type=float,
            help='sidereal rotation period [hours]')
    modify_porb_parser.add_argument("--phase_at_j2000", type=float,
            help='rotational phase (angle of prime meridian) at J2000 epoch [degrees]')
    modify_porb_parser.add_argument("--pole_ra", type=float,
            help='right ascension of spin axis in J2000 frame [radians]')
    modify_porb_parser.add_argument("--pole_dec", type=float,
            help='declination of spin axis in J2000 frame [radians]')
    modify_porb_parser.add_argument("--default_spin_flag", type=float,
            help='Flag indicating if the spin parameters really describe the object\'s spin state, or are default placeholder values. 0: Real, 1: Placeholder.')
    modify_porb_parser.add_argument("--obliquity", type=float,
            help='angle between spin axis and orbit pole [radians].')
    modify_porb_parser.add_argument("--true_anomaly_at_vernal_equinox", type=float,
            help='True anomaly at vernal equinox [radians]. (prograde angle between perihelion and VE vectors)')

    custom_porb_parser = subparsers.add_parser('custom', help='Generate a custom HDF for a fully-specified body')
    custom_porb_parser.set_defaults(func=custom_porb_command)
    custom_porb_parser.add_argument('body_name', help='Name to use for the custom object.')
    custom_porb_parser.add_argument('body_id', type=int, help='Numerical identifier for the custom object. Forced to be negative.')
    custom_porb_parser.add_argument('output_dir', help='Directory into which to write the HDF file.')
    custom_porb_parser.add_argument("long_of_asc_node", type=float,
            help='longitude of the ascending node [radians]. The angle (within the ecliptic plane) from the J2000 ICRF +X vector to this orbit\'s ascending node.')
    custom_porb_parser.add_argument("eccentricity", type=float,
            help='eccentricity [unitless].')
    custom_porb_parser.add_argument("inclination", type=float,
            help='inclination of the orbit with respect to the ecliptic plane. [radians].')
    custom_porb_parser.add_argument("arg_of_peri", type=float,
            help='argument of perihelion [radians]. The angle between the orbit\'s ascending node and its periapsis.')
    custom_porb_parser.add_argument("rotation_period", type=float,
            help='sidereal rotation period [hours]')
    custom_porb_parser.add_argument("phase_at_j2000", type=float,
            help='rotational phase (angle of prime meridian) at J2000 epoch [degrees]')
    custom_porb_parser.add_argument('-v', '--verbose', action='store_true', 
            help='Flag to enable verbose output.')
    custom_porb_parser.add_argument("--semimajor_axis", type=float,
            help='semimajor axis [AU].')
    custom_porb_parser.add_argument("--orbit_period", type=float,
            help='Period of the orbit in Earth days.')
    custom_porb_parser.add_argument("--perihelion_date", type=float,
            help='J2000 date (days past J2000 epoch) of previous perihelion passage.')
    custom_porb_parser.add_argument("--centuries_from_j2000", type=float,
            help='Time of reference epoch from J2000 epoch, in centuries.')
    custom_porb_parser.add_argument("--epoch_JD", type=float,
            help='Julian date of epoch [Julian Date].')
    custom_porb_parser.add_argument("--mean_anomaly", type=float,
            help='mean anomaly at epoch [radians]. Time fraction of the orbit that has elapsed since previous perihelion at the reference epoch, expressed as an angle in radians out of 2pi.')
    custom_porb_parser.add_argument("--pole_ra", type=float,
            help='right ascension of spin axis in J2000 frame [radians]')
    custom_porb_parser.add_argument("--pole_dec", type=float,
            help='declination of spin axis in J2000 frame [radians]')
    custom_porb_parser.add_argument("--obliquity", type=float,
            help='angle between spin axis and orbit pole [radians].')
    custom_porb_parser.add_argument("--true_anomaly_at_vernal_equinox", type=float,
            help='True anomaly at vernal equinox [radians]. (prograde angle between perihelion and VE vectors)')

    update_all_parser = subparsers.add_parser('update_all', help='Update all HDFs in the default directory.')
    update_all_parser.set_defaults(func=update_all_command)
    update_all_parser.add_argument('-v', '--verbose', action='store_true', help='Flag to enable verbose output.')
    
    args = parser.parse_args()
    args.func(args)

