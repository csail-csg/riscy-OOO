
# Copyright (c) 2017 Massachusetts Institute of Technology
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#!/usr/bin/python

import sys
import re

if len(sys.argv) != 2:
    print 'Usage: {} mkXXX.sched'.format(sys.argv[0])
    raise

with open(sys.argv[1], 'r') as f:
    rule_name = ''
    block_rule_start = False
    for line in f:
        line = line.rstrip('\n')

        if re.match(r'Rule:', line):
            block_rule_start = False # all blocking rule prev rule is printed
            rule_name = line # start a new rule
        elif re.match(r'Blocking rules:', line):
            if not re.match(r'Blocking rules: \(none\)', line):
                # current rule has blocking rules, so print it
                print rule_name
                print line
                block_rule_start = True
        elif re.match(r'Logical execution order', line):
            break # end of blocking rule section
        elif block_rule_start:
            print line # this line is a blocking rule of current rule, print it
