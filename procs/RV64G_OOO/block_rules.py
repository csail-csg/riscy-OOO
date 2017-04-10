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
