import re
import os

VARIABLE_EXPAND_DEPTH = 2


def expand_envs_with_os(envs):
    for var_name in envs:
        if envs[var_name] is not None:
            envs[var_name] = expand_variable_with_os(envs[var_name])
    return envs


def expand_variable_with_os(variable):
    for i in range(VARIABLE_EXPAND_DEPTH):
        variable = os.path.expandvars(str(variable))
    return variable


def custom_expandvars(value, envs):
    for i in range(VARIABLE_EXPAND_DEPTH):
        parts = re.split(r'(\$\{.*?\})', value)
        value = ''
        for item in parts:
            if item and item.startswith("${") and item.endswith("}"):
                key = item[2:-1]
                if key in envs:
                    item = envs[key]
            if item is not None:
                value += str(item)
    return value
