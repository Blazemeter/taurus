import logging
import os
import re
import sys
import traceback

import yaml

IGNORED_KEYS = ('variables', 'headers', 'properties', 'locations')
IGNORED_FIRST_LEVEL = ('scenarios', 'criteria')


def get_keys(struct, ignore_first_level=False):
    res = []
    if isinstance(struct, dict):
        if not ignore_first_level:
            res.extend(struct.keys())
        for key in struct:
            if key in IGNORED_KEYS:
                logging.debug("Ignored: %s: %s", key, struct[key])
                continue
            res.extend(get_keys(struct[key], key in IGNORED_FIRST_LEVEL))
    elif isinstance(struct, list):
        for item in struct:
            res.extend(get_keys(item))
    return res


def index_file(fname):
    with open(fname) as fhd:
        content = fhd.read()
    blocks_re = re.compile(r'```yaml([^`]+)```')
    blocks = blocks_re.findall(content)
    keys = set()
    for block in blocks:
        try:
            for struct in yaml.load_all(block):
                keys.update(get_keys(struct))
        except BaseException:
            logging.warning("Failed to parse block: %s", traceback.format_exc())
            logging.warning("The block was: %s", block)

    logging.debug("%s: %s", fname, keys)
    return keys


logging.basicConfig(level=logging.DEBUG)
docs_dir = sys.argv[1]
keys = set()
for fname in os.listdir(docs_dir):
    if not fname.endswith('.md'):
        logging.warning("Ignored path: %s", fname)
        continue
    keys.update(index_file(os.path.join(docs_dir, fname)))

print sorted(keys)

with open(sys.argv[2]) as fhr:
    table = "\n".join(['* `%s` - %s' % (kwrd, '') for kwrd in sorted(keys)])
    text = fhr.read() + table

with open(sys.argv[2], 'wt') as fhw:
    fhw.write(text)
