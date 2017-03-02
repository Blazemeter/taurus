# YAML Tutorial

This document is intended to be a short YAML tutorial, sufficient enough to get you started with YAML language.

YAML is an indentation-based markup language which aims to be both easy to read and easy to write. Many projects use
it because of its readability, simplicity and good support for many programming languages.

Here’s an example of YAML document (which is actually a Taurus config):

```yaml
execution:
- concurrency: 10
  hold-for: 5m
  ramp-up: 2m
  scenario: yaml_example
  
scenarios:
  yaml_example:
    retrieve-resources: false
    requests:
      - http://example.com/

reporting:
- module: final_stats
- module: console

settings:
  check-interval: 5s
  default-executor: jmeter

provisioning: local
```

As you can see, YAML uses indentation to represent document structure (as opposed to JSON, which uses brackets and
braces). Other than that, JSON and YAML are very similar. Here's the same YAML document converted to JSON:

```json
{
    "execution": [
        {
            "concurrency": 10,
            "hold-for": "5m",
            "ramp-up": "2m",
            "scenario": "json_example" 
        }
    ],
    "scenarios": {
        "json_example": {
            "retrieve-resources": false,
                "requests": [
                    "http://example.com/"
                ]
        }            
    },
    "reporting": [
        {
            "module": "final_stats"
        },
        {
            "module": "console"
        }
    ],
    "settings": {
        "check-interval": "5s",
        "default-executor": "jmeter"
    },
    "provisioning": "local"
}
```


## YAML Syntax

YAML document consists of the following elements.

### Scalars

Scalars are ordinary values: numbers, strings, booleans.
```yaml
number-value: 42
floating-point-value: 3.141592
boolean-value: true
# strings can be both 'single-quoted` and "double-quoted"
string-value: 'Bonjour'
```

YAML syntax also allows unquoted string values for convenience reasons:
```yaml
unquoted-string: Hello World
```

### Lists and Dictionaries

Lists are collections of elements:

```yaml
jedis:
  - Yoda
  - Qui-Gon Jinn
  - Obi-Wan Kenobi
  - Luke Skywalker
```

Every element of the list is indented and starts with a dash and a space.

Dictionaries are collections of `key: value` mappings. All keys are case-sensitive.

```yaml
jedi:
  name: Obi-Wan Kenobi
  home-planet: Stewjon
  species: human
  master: Qui-Gon Jinn
  height: 1.82m
```

Note that a space after the colon is mandatory.

Dictionaries can be nested in lists (and vice versa) to create more complex structures:

```yaml
requests:
  # first item of `requests` list is just a string
  - http://example.com/

  # second item of `requests` list is a dictionary
  - url: http://example.com/
    method: GET
```

You can also use inline syntax for lists and dictionaries, if you want:

```yaml
episodes: [1, 2, 3, 4, 5, 6, 7]
best-jedi: {name: Obi-Wan, side: light} 
```

## YAML Multi Documents

YAML format allows multiple documents to be embedded in a single file. They only have to be separated with a line containing triple-dash separator `---`.

```yaml
document: this is document 1
---
document: this is document 2
```

When reading multi-document YAML, Taurus will treat multiple documents as multiple configs and will load them one by one.

## YAML Debugging tips

There's a number of tools you can use to help you to locate and fix syntactical errors in your YAML document.

1. You can use online [services](http://yamltojson.com/) to convert it to JSON to check the structure (can be useful if you don’t have much experience with indentation-based languages)
2. You can use [yamllint](https://github.com/adrienverge/yamllint) to see if there're any errors or issues with your document


## YAML Gotchas

Due to the format aiming to be easy to write and read, there're some ambiguities in YAML.

### Special characters in unquoted strings
YAML has a number of special characters you cannot use in unquoted strings. For example, parsing the following sample
will fail:
```yaml
unquoted-string: let me put a colon here: oops
```

Quote the string value makes this value unambiguous:
```yaml
unquoted-string: "let me put a colon here: oops"
```

Generally, you should quote all strings that contain any of the following characters: `[] {} : > |`.

### Tabs versus spaces for indentation

Do *not* use tabs for indentation. While resulting YAML can still be valid, this can be a source of many subtle
parsing errors. Just use spaces.


## Additional YAML resources

1. [Online YAML Parser](http://yaml-online-parser.appspot.com/) can be used to analyze YAML documents
1. [Jesse Noller's article](http://jessenoller.com/blog/2009/04/13/yaml-aint-markup-language-completely-different) is a good introduction to YAML syntax
1. [Wikipedia](https://en.wikipedia.org/wiki/YAML) has a really good and comprehensive description of all major YAML features
1. Short, but very concise [YAML Tutorial](https://docs.saltstack.com/en/latest/topics/yaml/) by SaltStack
1. [yamllint](https://github.com/adrienverge/yamllint) is a linter for YAML files, which checks for many common mistakes and cosmetic issues
1. [YAML Lint](http://www.yamllint.com/) is another online YAML checker
