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
  - `-a, --all-http-methods` - extract all combinations of paths and HTTP methods.
  
By default Taurus will extract only GET requests. `--all-http-methods` switch can be used to extract all kinds of requests
from Swagger spec.

Usage:
  - `swagger2yaml swagger.json` - convert Swagger spec
  - `swagger2yaml swagger.json -a -o swagger-converted.yml` - convert Swagger spec, extracting all HTTP requests, and save it to a specific file

Notes about Swagger to YAML translation process:
1. The whole spec is converted into single scenario
2. Only GET requests are extracted by default (you can use `-a` option to extract all kinds of requests)
3. Templated paths (e.g. `/api/users/{userId}`) are converted to real paths by replacing path parameters with placeholder values accoding to parameter type
4. All types of parameters (except for `body`) are supported: `path`, `query`, `header`, `formData`.
5. While `swagger2yaml` can generate dummy data for parameters using `type` and `format` fields, the generation of dummy data from `schema` field (containing JSON Schema definition) isn't supported (yet).
