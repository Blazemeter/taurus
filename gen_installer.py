import os
import subprocess
import sys
import bzt
import configparser


def run_cmd(label, cmdline, **kwargs):
    print(label + " " + str(cmdline))
    subprocess.run(cmdline, check=True, **kwargs)


def generate_pynsist_config(dependencies, wheel_dir, cfg_location):
    print("Generating pynsist config")
    cfg = configparser.ConfigParser()
    cfg['Application'] = {
        'name': 'Taurus',
        'version': bzt.VERSION,
        'entry_point': 'bzt.cli:main',
        'console': 'true',
        'icon': 'site/img/taurus.ico',
    }

    cfg['Command bzt'] = {
        'entry_point': 'bzt.cli:main',
    }

    cfg['Command jmx2yaml'] = {
        'entry_point': 'bzt.jmx2yaml:main',
    }

    cfg['Command soapui2yaml'] = {
        'entry_point': 'bzt.soapui2yaml:main',
    }

    cfg['Python'] = {
        'bitness': 64,
        'version': '3.5.4',
    }

    wheels_list = ["%s==%s" % (package_name, version) for package_name, version in dependencies]
    cfg['Include'] = {
        'packages': '\n'.join(['tkinter', '_tkinter']),
        'pypi_wheels': "\n".join(wheels_list),
        'extra_wheel_sources': wheel_dir,
        'files': '\n'.join([
            'README.md',
            'lib',
        ])
    }

    with open(cfg_location, 'w') as fds:
        cfg.write(fds)


def run_pynsist(cfg_location):
    run_cmd("Running pynsist", ["pynsist", cfg_location])


def fetch_all_wheels(for_package, wheel_dir):
    run_cmd("Fetching wheels", [
        "pip-custom-platform", "wheel", "--wheel-dir", wheel_dir, "--platform", "win_amd64", for_package
    ])


def extract_all_dependencies(wheel_dir):
    """Extract all (package, version) pairs from a directory with wheels"""
    print("Extracting dependent package list")
    packages = []
    for filename in os.listdir(wheel_dir):
        if filename.endswith('.whl'):
            parts = filename.split('-')
            package_name, version = parts[0], parts[1]
            packages.append((package_name, version))
    return packages


def main():
    if len(sys.argv) < 2:
        print("Usage: %s <bzt-wheel>" % sys.argv[0])
        sys.exit(1)
    bzt_dist = sys.argv[1]
    pynsist_config = "installer-gen.cfg"
    wheel_dir = "build/wheels"
    fetch_all_wheels(bzt_dist, wheel_dir)
    dependencies = extract_all_dependencies(wheel_dir)
    generate_pynsist_config(dependencies, wheel_dir, pynsist_config)
    run_pynsist(pynsist_config)


if __name__ == '__main__':
    main()
