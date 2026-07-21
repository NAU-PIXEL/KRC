import argparse

from . import interface

def update_command(args):
    interface.update_default_hdf(args.body_name)

def modify_porb_command(args):
    print(args)
    kwargs = vars(args)
    kwargs.pop('command')
    kwargs.pop('func')
    interface.get_and_modify_porb_params(**kwargs)

def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest='command', required=True)

    update_parser = subparsers.add_parser('update', help='Update the HDF for a body')
    update_parser.set_defaults(func=update_command)
    update_parser.add_argument('body_name', help='Name of body to update.')

    modify_porb_parser = subparsers.add_parser('custom', help='Generate a custom HDF for the selected body')
    modify_porb_parser.set_defaults(func=modify_porb_command)
    modify_porb_parser.add_argument('body_name', help='Name of the base body to use for the custom HDF.')
    modify_porb_parser.add_argument('--long_of_asc_node')
    modify_porb_parser.add_argument('--eccentricity')
    modify_porb_parser.add_argument('--inclination')
    modify_porb_parser.add_argument('--arg_of_peri')
    modify_porb_parser.add_argument('--semimajor_axis')
    modify_porb_parser.add_argument('--orbit_period')
    modify_porb_parser.add_argument('--perihelion_date')
    modify_porb_parser.add_argument('--centuries_from_j2000')
    modify_porb_parser.add_argument('--epoch_JD')
    modify_porb_parser.add_argument('--mean_anomaly')
    modify_porb_parser.add_argument('--rotation_period')
    modify_porb_parser.add_argument('--phase_at_j2000')
    modify_porb_parser.add_argument('--pole_ra')
    modify_porb_parser.add_argument('--pole_dec')
    modify_porb_parser.add_argument('--default_spin_flag')
    modify_porb_parser.add_argument('--obliquity')
    modify_porb_parser.add_argument('--rotation_matrix_FtoB')
    modify_porb_parser.add_argument('--true_anomaly_at_vernal_equinox')
    
    args = parser.parse_args()
    args.func(args)

