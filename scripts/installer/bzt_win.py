#!python3.6
import os, sys
sys.path.insert(0, 'pkgs')
os.environ["PATH"] += os.pathsep + os.path.join(os.path.dirname(sys.executable), 'Scripts')


def main():
    sys.exit(os.system("cmd /k bzt --help"))
