import logging
import os
import re
import sys
import traceback

import yaml

IGNORED_KEYS = (
    'variables', 'headers', 'properties', 'locations',
    'set-prop', 'env', 'body', 'globals', 'set-variables', 'system-properties'
)
IGNORED_FIRST_LEVEL = (
    'scenarios', 'criteria', 'cli-aliases', 'extract-regexp', 'extract-xpath', 'extract-jsonpath', 'extract-css-jquery'
)


def get_keys(struct, ignore_first_level=False):
    res = []
    if isinstance(struct, dict):
        if not ignore_first_level:
            keys = [x.split('(')[0] for x in struct.keys()]
            res.extend(keys)
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
            for struct in yaml.safe_load_all(block):
                keys.update(get_keys(struct))
        except BaseException:
            logging.warning("Failed to parse block: %s", traceback.format_exc())
            logging.warning("The block was: %s\n%s", fname, block)

    for key in keys:
        assert "(" not in key, key + "\t" + fname

    logging.debug("%s: %s", fname, keys)
    return keys


def main(docs_dir, outfile):
    keys = {}
    for fname in os.listdir(docs_dir):
        if not fname.endswith('.md') or fname == 'YAMLTutorial.md':
            logging.warning("Ignored path: %s", fname)
            continue
        keywords = index_file(os.path.join(docs_dir, fname))
        for kwrd in keywords:
            if kwrd not in keys:
                keys[kwrd] = set()
            keys[kwrd].add(fname)

    with open(outfile) as fhr:
        items = []
        for kwrd in sorted(keys.keys()):
            pages = ', '.join(['[%s](%s)' % (x.replace('.md', ''), x.replace('.md', '')) for x in sorted(keys[kwrd])])
            items.append('* `%s`: %s' % (kwrd, pages))
        text = fhr.read() + "\n".join(items)

    with open(outfile, 'wt') as fhw:
        fhw.write(text)


logging.basicConfig(level=logging.DEBUG)
main(sys.argv[1], sys.argv[2])
