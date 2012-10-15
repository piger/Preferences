#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Template di base per un modulo python"""
import logging
import optparse


def main():
    LOGGING_LEVELS = {
        'CRITICAL': logging.CRITICAL,
        'ERROR': logging.ERROR,
        'WARNING': logging.WARNING,
        'INFO': logging.INFO,
        'DEBUG': logging.DEBUG
    }
    logfmt = '%(asctime)s %(name)s %(levelname)s: %(message)s'
    datefmt = '%d-%m-%Y %H:%M:%S'

    parser = optparse.OptionParser()
    parser.add_option('-l', '--logging-level', help='Logging level',
                      choices=LOGGING_LEVELS.keys())
    parser.add_option('-f', '--logging-file', help='Log to a file')
    parser.add_option('--debug', action='store_true', default=False,
                      help='Set log level to DEBUG')
    (opts, args) = parser.parse_args()
    logging_level = opts.debug or LOGGING_LEVELS.get(opts.logging_level, logging.NOTSET)
    logging.basicConfig(level=logging_level, filename=opts.logging_file,
                        format=logfmt, datefmt=datefmt)

    # Your code here...


if __name__ == '__main__':
    main()
