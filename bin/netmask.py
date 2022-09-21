#!/usr/bin/env python3

import os
import sys
import ipaddress


def usage():
    print(f"Usage: {os.path.basename(sys.argv[0])}: <netmask> [address]")

def main():
    narg = len(sys.argv)
    if narg == 1:
        usage()
        sys.exit(1)

    try:
        prefix = ipaddress.ip_network(sys.argv[1], strict=False)
    except ValueError as err:
        print(f"error: {err}")
        sys.exit(1)

    if narg == 2:
        for host in prefix:
            print(host)
    elif narg == 3:
        try:
            ip = ipaddress.ip_address(sys.argv[2])
        except ValueError as err:
            print(f"error: {err}")
            sys.exit(1)

        if ip not in prefix:
            print(f"{prefix} does not contain {ip}")
            sys.exit(1)
    else:
        usage()
        sys.exit(1)

if __name__ == "__main__":
    main()
