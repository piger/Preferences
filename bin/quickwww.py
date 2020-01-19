#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Quick python WWW server

NO(C) Daniel Kertesz <daniel@spatof.org>
"""
import sys
import getopt
from http.server import HTTPServer, SimpleHTTPRequestHandler

def main():
    address = "127.0.0.1"
    port = 8888
    try:
        opts, args = getopt.getopt(sys.argv[1:], "a:p:h")
    except getopt.GetoptError as exc:
        print(f"ERROR: {exc}")
        sys.exit(1)
    for option, value in opts:
        if option == "-a":
            address = value
        elif option == "-p":
            port = int(value)
        elif option == "-h":
            print(f"Usage: {sys.argv[0]} [-a address] [-p port]")
            sys.exit(0)

    server = HTTPServer((address, port), SimpleHTTPRequestHandler)
    print(f"Serving at http://{address}:{port}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    main()
