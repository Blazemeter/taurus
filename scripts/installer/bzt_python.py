#!python3.6
import os, subprocess, sys
sys.path.insert(0, 'pkgs')
os.environ["PATH"] += os.pathsep + os.path.dirname(sys.executable)

def main():
    args = ["python"] + sys.argv[1:]
    process = subprocess.run(args, shell=True)
    sys.exit(process.returncode)
