#!/usr/bin/env python

"""Simple HTTP server for Robot Framework web testing demo.

Usage:  server.py [port]

This server serves HTML pages under `html` directory. Server is started simply
by running this script from the command line or double-clicking it in a file
manager. In the former case the server can be shut down with Ctrl-C and in the
latter case by closing the opened window.

By default the server uses port 7272, but a custom port can be given as
an argument from the command line.
"""

from __future__ import print_function

from os import chdir
from os.path import abspath, dirname, join
try:
    from SocketServer import ThreadingMixIn
    from BaseHTTPServer import HTTPServer
    from SimpleHTTPServer import SimpleHTTPRequestHandler
except ImportError:
    from socketserver import ThreadingMixIn
    from http.server import SimpleHTTPRequestHandler, HTTPServer


ROOT = join(dirname(abspath(__file__)), 'html')
PORT = 7272


class DemoServer(ThreadingMixIn, HTTPServer):
    allow_reuse_address = True

    def __init__(self, port=PORT):
        HTTPServer.__init__(self, ('localhost', int(port)),
                            SimpleHTTPRequestHandler)

    def serve(self, directory=ROOT):
        chdir(directory)
        print('Demo server starting on port %d.' % self.server_address[1])
        try:
            server.serve_forever()
        except KeyboardInterrupt:
            server.server_close()
        print('Demo server stopped.')


if __name__ == '__main__':
    import sys
    try:
        server = DemoServer(*sys.argv[1:])
    except (TypeError, ValueError):
        print(__doc__)
    else:
        server.serve()
