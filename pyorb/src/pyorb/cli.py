import argparse

from . import interface
from . import body_params

def update_command(args):
    interface.update_default_hdf(args.body_name)

def update_all_command(args):
    interface.update_all_default_hdfs()

def modify_porb_command(args):
    kwargs = vars(args)
    kwargs.pop('command')
    kwargs.pop('func')
    output_dir = kwargs.pop("output_dir")
    modified_params = interface.get_and_modify_porb_params(**kwargs)
    hdf_file = body_params.high_level_write_hdf(modified_params, output_dir)
    print(f'Wrote custom HDF: {hdf_file}')

def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest='command', required=True)

    update_parser = subparsers.add_parser('update', help='Update the HDF for a specified body')
    update_parser.set_defaults(func=update_command)
    update_parser.add_argument('body_name', help='Name of body to update.')

    modify_porb_parser = subparsers.add_parser('custom', help='Generate a custom HDF for the selected body')
    modify_porb_parser.set_defaults(func=modify_porb_command)
    modify_porb_parser.add_argument('body_name', help='Name of the base body to use for the custom HDF.')
    modify_porb_parser.add_argument('output_dir', help='Directory into which to write the HDF file.')
    modify_porb_parser.add_argument("--long_of_asc_node", type=float)
    modify_porb_parser.add_argument("--eccentricity", type=float)
    modify_porb_parser.add_argument("--inclination", type=float)
    modify_porb_parser.add_argument("--arg_of_peri", type=float)
    modify_porb_parser.add_argument("--semimajor_axis", type=float)
    modify_porb_parser.add_argument("--orbit_period", type=float)
    modify_porb_parser.add_argument("--perihelion_date", type=float)
    modify_porb_parser.add_argument("--centuries_from_j2000", type=float)
    modify_porb_parser.add_argument("--epoch_JD", type=float)
    modify_porb_parser.add_argument("--mean_anomaly", type=float)
    modify_porb_parser.add_argument("--rotation_period", type=float)
    modify_porb_parser.add_argument("--phase_at_j2000", type=float)
    modify_porb_parser.add_argument("--pole_ra", type=float)
    modify_porb_parser.add_argument("--pole_dec", type=float)
    modify_porb_parser.add_argument("--default_spin_flag", type=float)
    modify_porb_parser.add_argument("--obliquity", type=float)
    modify_porb_parser.add_argument("--true_anomaly_at_vernal_equinox", type=float)

    update_all_parser = subparsers.add_parser('update_all', help='Update all HDFs in the default directory.')
    update_all_parser.set_defaults(func=update_all_command)
    
    args = parser.parse_args()
    args.func(args)

