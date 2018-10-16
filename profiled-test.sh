#!/usr/bin/env bash
python -m cProfile -o /tmp/taurus.profile /home/undera/.local/bin/nosetests --nologcapture tests.modules.test_external:TestExternalResultsLoader.test_plain
python -m gprof2dot -f pstats /tmp/taurus.profile | dot -Tpng -o /tmp/profile.png