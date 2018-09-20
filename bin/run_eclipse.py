#!/usr/bin/env python
import sys
from res.fm.ecl import simulate
version=sys.argv[1]
data_file=sys.argv[2]
simulate('ecl100',version,data_file)

