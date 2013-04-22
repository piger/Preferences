#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Quick python WWW server

NO(C) Daniel Kertesz <daniel@spatof.org>
"""
import sys
import getopt
import BaseHTTPServer, SimpleHTTPServer

def main():
    address = "127.0.0.1"
    port = 8888
    try:
        opts, args = getopt.getopt(sys.argv[1:], "a:p:h")
    except getopt.GetoptError, e:
        print "ERROR:", e
        sys.exit(1)
    for option, value in opts:
        if option == "-a":
            address = value
        elif option == "-p":
            port = int(value)
        elif option == "-h":
            print "Usage: %s [-a address] [-p port]" % sys.argv[0]
            sys.exit(0)

    server = BaseHTTPServer.HTTPServer(
        (address, port),
        SimpleHTTPServer.SimpleHTTPRequestHandler)
    print "Serving at http://%s:%d" % (address, port)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    main()
