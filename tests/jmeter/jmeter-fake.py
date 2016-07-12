import os
import re
import socket
import shutil
import sys


def get_mode():
    return os.environ.get("TEST_MODE")


def get_artifacts_dir():
    return os.environ.get("TAURUS_ARTIFACTS_DIR", ".")


def udp_server():
    udp_sock = socket.socket(type=socket.SOCK_DGRAM)
    udp_sock.bind(('localhost', 8089))
    print("UDP Server started..")
    while True:
        buf, _ = udp_sock.recvfrom(1024)
        if len(buf) > 0:
            print('Received: "%s"\n' % buf)
            if buf == b"Shutdown":
                break
    print("UDP Server stopped.")


def files():
    artifacts_dir = get_artifacts_dir()
    jmeter_path = os.path.dirname(__file__)
    jtl_file = jmeter_path + '/jtl/tranctl.jtl'
    jmx = [x for x in sys.argv if x.endswith('.jmx')][0]
    with open(jmx, 'r') as fds:
        jmx_content = fds.read()
    matches = re.findall(r'<stringProp name="filename">(.+kpi(?:\-\d+)?\.jtl)<', jmx_content)
    shutil.copy(jtl_file, os.path.join(artifacts_dir, matches[0]))


mode = get_mode()

# mode is gotten via environment variable $TEST_MODE
if mode == 'files':     # test_engine
    files()
elif mode == 'server':  # test_JMeterExecutor.test_shutdown_soft
    udp_server()
elif mode == 'heap':    # test_JMeterExecutor.test_jvm_heap*
    print(os.environ['JVM_ARGS'])
else:                   # test if jmeter is installed
    print('JMeter is installed')
