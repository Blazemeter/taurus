import os


# import logging


def get_mode():
    return os.environ.get("TEST_MODE")


def udp_server():
    import socket

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
    import shutil
    jmeter_path = os.path.dirname(__file__)
    jtl_file = jmeter_path + '/jtl/tranctl.jtl'
    file_name = 'kpi'
    suffix = ''
    i = 0
    while os.path.isfile('./' + file_name + suffix + '.jtl'):
        i += 1
        suffix = '-%i' % i
    shutil.copy(jtl_file, './' + file_name + suffix + '.jtl')


# logging.basicConfig(level=logging.ERROR, filename='/tmp/bzt/fake_tool.log')
mode = get_mode()

# logging.debug('mode=%s', mode)

if mode == 'files':
    files()
elif mode == 'server':
    udp_server()
elif mode == 'heap':
    print(os.environ['JVM_ARGS'])
else:
    print('JMemeter is installed')
