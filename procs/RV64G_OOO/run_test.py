#!/usr/bin/python

import argparse
import os
import glob

#test_src_dir = os.path.join(os.environ['RISCY_HOME'], 'tools', 'riscv-tests')
test_bin_dir = os.path.join(os.environ['RISCY_TOOLS'], 'riscv64-unknown-elf', 'share', 'riscv-tests')

def extract_asm_name(file_path):
    return os.path.splitext(os.path.basename(file_path))[0]

# legal test types
class TestType:
    assembly = 'assembly'
    assembly_fp = 'assembly_fp'
    benchmarks = 'benchmarks'

# assembly test programs
assembly_tests = map(extract_asm_name,
    glob.glob(os.path.join(test_bin_dir, 'isa', 'rv64ui-p-*.dump')))
# assembly_fp test programs
assembly_fp_tests = map(extract_asm_name,
    glob.glob(os.path.join(test_bin_dir, 'isa', 'rv64uf-p-*.dump')))
# benchmarks test programs
benchmarks_tests = [
    'median.riscv',
    'multiply.riscv',
    'qsort.riscv',
    'vvadd.riscv',
    'towers.riscv',
    'dhrystone.riscv',
    'sort.riscv',
    'rsort.riscv',
    #'spmv.riscv'
    ]

# parse command line args
parser = argparse.ArgumentParser()
parser.add_argument('-e', required = True, metavar = 'UBUNTU_EXE', dest = 'exe')
parser.add_argument('-t', required = True, metavar = 'TEST_TYPE', dest = 'test',
    choices = [TestType.assembly, TestType.assembly_fp, TestType.benchmarks])
args = parser.parse_args()

# set up the tests to run
out_dir = os.path.join('out', args.test)
test_dir = ''
test_arg = ''
tests = []
if args.test == TestType.assembly:
    test_dir = os.path.join(test_bin_dir, 'isa')
    test_arg = ' --assembly-tests '
    tests = assembly_tests
elif args.test == TestType.assembly_fp:
    test_dir = os.path.join(test_bin_dir, 'isa')
    test_arg = ' --assembly-tests '
    tests = assembly_fp_tests
elif args.test == TestType.benchmarks:
    test_dir = os.path.join(test_bin_dir, 'benchmarks')
    test_arg = ' --just-run '
    tests = benchmarks_tests

# create output log folder and go to it
if not os.path.exists(out_dir):
    os.makedirs(out_dir)
os.chdir(out_dir)

for t in tests:
    test_prog = os.path.join(test_dir, t)
    test_log = t + '.out'
    if not os.path.isfile(test_prog):
        print '[WARNING] {} does not exist'.format(test_prog)
    else:
        print 'Run ' + test_prog
        cmd = args.exe + test_arg + ' -- ' + test_prog + ' > ' + test_log
        #print cmd
        os.system(cmd)
