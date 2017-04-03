# Convert Existing JMX File into Taurus YAML

Command-line tool named `jmx2yaml` is used to convert simple JMX scripts into Taurus configuration file. Tool options are:

  - `-h, --help` - show help message and exit
  - `-q, --quiet` - log only error messages
  - `-v, --verbose` - include debugging messages into logs
  - `-j, --json` - use JSON as output format instead of YAML
  - `-o FILE\_NAME, --out=FILE\_NAME` - change output file name, by default is input file name + `.yml`
  - `-d DUMP\_JMX, --dump-jmx=DUMP\_JMX` - dumps modified JMX etree into file
  - `-l LOG, --log LOG` - specify log file location

Usage:
  - `jmx2yaml sample.jmx`
  - `jmx2yaml sample.jmx -q -n -j` 
  - `jmx2yaml sample.jmx -v -o out.yml -d modified.jmx` 

Make note that only flat JMX structure is supported. Several Thread Groups will be transformed into several executions. 
