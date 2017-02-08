#!/usr/bin/env python2
"""
Scons file for second_year_paper.
(Currently very incomplete)

Written with scons v2.5.0, python v2.7.12+
Scons docs here: http://scons.org/doc/production/PDF/scons-user.pdf
Note: this is a simple SConstruct file that must be put in the top level.
Don't fuss around with SConscript files!

"""

from __future__ import division, absolute_import, print_function, unicode_literals
import os
from platform import system
# just pass along our regular environment.
# print(os.environ)
# env = Environment(os.environ)
env = Environment(ENV = os.environ)
# first check file timestamps, and only compute the MD5 hash if the timestamp has changed
env.Decider('MD5-timestamp')


def box_home(default = None):
    _system = system()
    use_default = False
    if _system in ('Windows', 'cli'):
        info_path = os.path.join(os.getenv('APPDATA'), 'Box Sync', 'info.json')
        if not os.path.isfile(info_path):
            info_path = os.path.join(os.getenv('LOCALAPPDATA'), 'Box Sync', 'info.json')

    elif _system in ('Darwin'):
        user_home = os.path.expanduser('~')
        info_path = os.path.join(user_home, 'Library', 'Application Support',
                                 'Box', 'Box Sync', 'sync_root_folder.txt')
    elif _system in ('Linux'):
        if default is None:
            raise RuntimeError("Box doesn't run on Linux.")
        else:
            use_default = True
    else:
        if default is None:
            raise RuntimeError('Unknown system: {}'.format(_system))
        else:
            use_default = True
    if not use_default:
        if not os.path.isfile(info_path):
            err_msg = ("Could not find the Box sync_root_folder.txt file! (Should be" +
                       "here: '" + info_path + "')")
            raise FileNotFoundError(err_msg)
        with open(info_path, 'r') as f:
            box_dir = f.readline().rstrip('\n\r')
    else:
        box_dir = os.path.expanduser(default)
    print(box_dir)
    if not os.path.isdir(box_dir):
        err_msg = ("Box configuration indicated the Box directory was '" + box_dir +
                   "', but that doesn't exist.")
        # raise FileNotFoundError(err_msg)
        raise OSError(err_msg)
    return box_dir

box_dir = box_home('~/Everything_else/Box_local/second_year_paper_data/Polk')


state_data = env.Command(
    target = 'Data/us_state_gdp.rda',
    source = None,
    action = 'Rscript --vanilla Code/download_state_data.r',
    )

polk_data = env.Command(
    target = "Text/Plots/vehicle_registrations_alaska_vs_notitle.pdf",
    source = os.path.join(box_dir, 'OPP-12322003_University of California Berkley.xlsx'),
    action = "Code/polk_registrations.r"
)
