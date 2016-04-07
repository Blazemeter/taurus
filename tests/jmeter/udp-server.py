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
