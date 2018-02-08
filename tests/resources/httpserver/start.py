#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""launch small http server
"""
import os
import argparse

try:
    from SimpleHTTPServer import SimpleHTTPRequestHandler
except ImportError:
    from http.server import SimpleHTTPRequestHandler

try:
    from SocketServer import TCPServer as HTTPServer
except ImportError:
    from http.server import HTTPServer


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--port', '-p', default=8000, type=int)
    parser.add_argument('--dir', '-d', default=os.path.dirname(os.path.realpath(__file__)), type=str)
    args = parser.parse_args()

    os.chdir(args.dir)

    server_address = ('', args.port)
    httpd = HTTPServer(server_address, SimpleHTTPRequestHandler)
    sa = httpd.socket.getsockname()
    print("Serving HTTP on %s port %s ..." % (sa[0], sa[1]))
    httpd.serve_forever()


if __name__ == "__main__":
    main()
