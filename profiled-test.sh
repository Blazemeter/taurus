#!/bin/bash -x
python -m cProfile -o /tmp/taurus.profile `which nosetests` --nologcapture tests.modules.test_external:TestExternalResultsLoader.test_errors_jtl2
python -m gprof2dot -f pstats /tmp/taurus.profile | dot -Tpng -o /tmp/profile.png