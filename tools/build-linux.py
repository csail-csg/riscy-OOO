#!/usr/bin/python

import os
import stat
import shutil
import argparse
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('--testdir', dest = 'test_dir',
                    required = False, default = '')
parser.add_argument('--jobs', dest = 'jobs', required = False, default = 1)
parser.add_argument('--depld', dest = 'dep_ld_fence', action = 'store_true',
                    help = 'add fence between dependent loads')
args = parser.parse_args()

root = os.path.join(os.environ['RISCY_HOME'], 'tools')
linux_dir = os.path.join(root, 'riscv-linux')
config_dir = os.path.join(root, 'configs')
build_dir = os.environ['RISCY_TOOLS']
test_dir = '' if args.test_dir == '' else os.path.abspath(args.test_dir)

# copy initramfs.txt and .config
shutil.copy(os.path.join(config_dir, 'linux_config'),
            os.path.join(linux_dir, '.config'))
with open(os.path.join(linux_dir, '.config'), 'a') as fp:
    if args.dep_ld_fence:
        fp.write('CONFIG_DEP_LD_REORDER=y\n')
    else:
        fp.write('CONFIG_DEP_LD_REORDER=n\n')
shutil.copy(os.path.join(config_dir, 'initramfs.txt'), linux_dir)

# append to initramfs.txt with contents in test folder
if test_dir != '':
    with open(os.path.join(linux_dir, 'initramfs.txt'), 'a') as fp:
        def writeTree(ramfs_dir, src_dir):
            for f in os.listdir(src_dir):
                ramfs_path = os.path.join(ramfs_dir, f)
                src_path = os.path.join(src_dir, f)
                mode = os.stat(src_path).st_mode
                perm = oct(stat.S_IMODE(mode))[-3:]
                if stat.S_ISDIR(mode):
                    fp.write('dir {} {} 0 0\n'.format(ramfs_path, perm))
                    writeTree(ramfs_path, src_path)
                elif stat.S_ISREG(mode):
                    fp.write('file {} {} {} 0 0\n'.format(ramfs_path,
                                                        src_path,
                                                        perm))
                else:
                    raise Exception('unknown file type ' + src_path)
        writeTree('/test', test_dir)

# compile vmlinux
cmd = 'cd {}; make ARCH=riscv -j{}'.format(linux_dir, args.jobs)
print 'Running: {}'.format(cmd)
subprocess.check_call(cmd, shell = True)

# compile bbl
cmd = ('cd {}; '.format(build_dir) +
       'rm -rf build-pk; mkdir -p build-pk; cd build-pk; ' +
       '../../riscv-pk/configure ' +
       '--prefix={} '.format(build_dir) +
       '--host=riscv64-unknown-elf ' +
       '--with-payload={}; '.format(os.path.join(linux_dir, 'vmlinux')) +
       'make -j{}; make install'.format(args.jobs))
print 'Running: {}'.format(cmd)
subprocess.check_call(cmd, shell = True)

