import os
import sys
import tempfile

from bzt.engine import Configuration


fp, filename = tempfile.mkstemp()
os.write(fp, sys.stdin.read())

conf = Configuration()
conf.load([filename])
conf.dump(filename, Configuration.YAML)

sys.stdout.write(open(filename).read())