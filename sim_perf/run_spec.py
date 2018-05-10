#!/usr/bin/python

import os
import argparse
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('--ff', dest = 'ff_insts', required = False, default = 40000000000)
parser.add_argument('--sim', dest = 'sim_insts', required = False, default = 20000000000)
parser.add_argument('--lat', dest = 'lat', required = True, help = 'load to use latency')
parser.add_argument('--bbldir', dest = 'bbl_dir', required = True)
parser.add_argument('--outdir', dest = 'out_dir', required = True)
args = parser.parse_args()

sim_bin = os.path.join(os.environ['RISCY_HOME'], 'sim_perf', 'build', 'sim')
in_file = os.path.join(os.environ['RISCY_HOME'], 'sim_perf', 'in.txt')
bbl_dir = os.path.abspath(args.bbl_dir)
out_dir = os.path.abspath(args.out_dir)

if not os.path.isdir(out_dir):
    os.makedirs(out_dir)

all_bench = [
    '401.bzip2-source',
    '401.bzip2-program',
    '401.bzip2-combined',
    '401.bzip2-chicken',
    '401.bzip2-liberty',
    '401.bzip2-text',
    '403.gcc-166',
    '403.gcc-200',
    '403.gcc-c-typeck',
    '403.gcc-cp-decl',
    '403.gcc-expr',
    '403.gcc-expr2',
    '403.gcc-g23',
    '403.gcc-s04',
    '403.gcc-scilab',
    '429.mcf',
    '445.gobmk-13x13',
    '445.gobmk-nngs',
    '445.gobmk-score2',
    '445.gobmk-trevorc',
    '445.gobmk-trevord',
    '456.hmmer-swiss41',
    '456.hmmer-retro',
    '458.sjeng',
    '462.libquantum',
    '464.h264ref-freb',
    '464.h264ref-frem',
    '464.h264ref-sem',
    '471.omnetpp',
    '473.astar-lakes',
    '473.astar-rivers',
    '483.xalancbmk',
]

for bench in all_bench:
    cmd = ('screen -dmS ' + bench + '_lat' + str(args.lat) + ' bash -c \'' +
           'cd ' + os.environ['RISCY_HOME'] + '; source setup.sh; ' +
           sim_bin + ' BW' +
           ' --ff ' + str(args.ff_insts) +
           ' --sim ' + str(args.sim_insts) +
           ' --out ' + os.path.join(out_dir, bench + '.perf') +
           ' --lat ' + str(args.lat) +
           ' -- ' + os.path.join(bbl_dir, 'bbl_spec_' + bench + '_ref') +
           ' < ' + in_file +
           ' &> ' + os.path.join(out_dir, bench + '.out\''))
    if not os.path.isfile(os.path.join(bbl_dir, 'bbl_spec_' + bench + '_ref')):
        raise Exception(bench + ' not found')
    print cmd
    subprocess.check_call(cmd, shell = True)
    
