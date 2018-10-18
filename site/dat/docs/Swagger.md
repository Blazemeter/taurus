# Swagger

## swagger2yaml — Convert Swagger specs into Taurus YAML

Taurus includes a command-line utility named `swagger2yaml` that can be used to convert existing Swagger definitions
into Taurus YAML-based configs.

Supported command-line options:
  - `-h, --help` - show help message and exit
  - `-q, --quiet` - only errors printed to console
  - `-v, --verbose` - prints all logging messages to console
  - `-j, --json` - use json format instead of yaml
  - `-o FILE\_NAME, --out=FILE\_NAME` - change output file name, by default is input file name + `.yml` in current directory
  - `--scenarios-from-paths` - generate a scenario per Swagger path (instead of generating a single scenario). disabled by default.
  - `--parameter-interpolation` - setup interpolation for templated parameters. Valid values are 'values', 'variables', 'none'. Default is 'values'.
  
Usage:
  - `swagger2yaml swagger.json` - convert Swagger spec
  - `swagger2yaml swagger.json -o swagger-converted.yml --scenarios-from-paths` - convert Swagger spec, creating a scenario per path, and save it to a specific file

Notes about Swagger to YAML translation process:
1. The whole spec can be converted either into a single scenario or to multiple scenarios (`--scenarios-from-paths`)
3. Templated paths (e.g. `/api/users/{userId}`) are converted to real paths by replacing path parameters with placeholder values accoding to parameter type
4. All types of parameters (except for `body`) are supported: `path`, `query`, `header`, `formData`.
5. While `swagger2yaml` can generate dummy data for parameters using `type` and `format` fields, the generation of dummy data from `schema` field (containing JSON Schema definition) isn't supported (yet).
6. Some authentication types are not supported yet
