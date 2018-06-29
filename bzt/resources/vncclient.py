import sys

from bzt.modules.wdgrid import start_vnc

if __name__ == '__main__':
    args = sys.argv[1:5]
    start_vnc(args)
