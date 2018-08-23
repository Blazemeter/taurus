#!python3.6
import os, subprocess, sys
sys.path.insert(0, 'pkgs')
os.environ["PATH"] += os.pathsep + os.path.join(os.path.dirname(sys.executable), 'Scripts')

def main():
    args = sys.argv[1:]
    process = subprocess.run(args)
    sys.exit(process.returncode)
