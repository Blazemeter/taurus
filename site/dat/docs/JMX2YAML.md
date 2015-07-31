# Convert Existing JMX File into Taurus YAML

Command-line tool named `jmx2yaml` is used to convert simple JMX scripts into Taurus configuration file. Tool options are:

  - `-h, --help` - show help message and exit
  - `-q, --quiet` - only errors printed to console
  - `-v, --verbose` - prints all logging messages to console
  - `-n, --no-dump` - prints result to console
  - `-j, --json` - use json format instead of yaml
  - `-o FILE_NAME, --out=FILE_NAME` - change output file name, by default is input file name + `.yml` in current directory
  - `-d DUMP_JMX, --dump-jmx=DUMP_JMX` - dumps modified JMX etree into file

Usage:
  - `jmx2yaml sample.jmx`
  - `jmx2yaml sample.jmx -q -n -j` 
  - `jmx2yaml sample.jmx -v -o out.yml -d modified.jmx` 
  
Make note that only flat JMX structure is supported. Several Thread Groups will be transformed into several executions. 