import os
import time
from sys import stdout, stderr, stdin
from bzt.utils import BetterDict, shell_exec

env = BetterDict()
env.merge(dict(os.environ))
cmd_line = env.get('T_CMD_LINE', '')
delay = env.get('T_DELAY', 0)
print("T_CMD_LINE: %s\n" % cmd_line)
print("T_DELAY: %s\n" % delay)
time.sleep(int(delay))
shell_exec(cmd_line, stdout=stdout, stderr=stderr, stdin=stdin, cwd=os.getcwd(), env=env).wait()
