#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Descrizione del problema.

L'installazione e la manutenzione della directory `Preferences` hanno bisogno
di alcune operazioni:

- link di file o directory
- download e update di repository git

-----------------------------------
Formato del file di configurazione:
-----------------------------------

preferences root = /Users/sand/tmp/pref
destination directory = ~

vim = src:vimrc dst: [nodot]

"""

import os, sys
import re
import string
from pyparsing import (Word, dblQuotedString, Combine, CaselessLiteral,
                       Optional, ZeroOrMore, alphas, alphanums, removeQuotes,
                       ParseException, StringEnd)
from colorfmt import colorize

ERROR = 1
SUCCESS = 0

def error(text):
    print colorize("{red}ERROR{default}: %s" % text)

def success(text):
    print colorize("{green}${default} %s" % text)

class Parser(object):
    def __init__(self):
        # pyparsing
        simple = Word(string.letters + string.digits + string.punctuation)
        quoted = dblQuotedString.setParseAction(removeQuotes)
        single = quoted | simple

        src = CaselessLiteral("src:") + single.setResultsName("src")
        dst = CaselessLiteral("dst:") + single.setResultsName("dst")
        operator = CaselessLiteral("[nodot]") | CaselessLiteral("[nofoo]")

        query = src + Optional(dst) + ZeroOrMore(operator) + StringEnd()
        # end pyparsing
        self.parser = query

    def parse(self, text):
        return self.parser.parseString(text)
parser = Parser()

def parse_config_file(filename):
    config = {}

    with open(filename) as fd:
        for i, line in enumerate(fd):
            line = line.strip()
            if not line or line.startswith('#'): continue

            try:
                option, value = [x.strip() for x in line.split('=', 1)]
            except ValueError:
                print "ConfigError: malformed line at %d:\n%s" % (i+1, line)
                print "^" * len(line)
                sys.exit(1)

            # Espande i path relativi per alcune opzioni.
            if option in ['destination directory', 'preferences root']:
                if value.startswith('~'):
                    value = os.path.expanduser(value)

            config[option] = value
    return config

def parse_config_element(element):
    result = None

    try:
        result = parser.parse(element)
    except ParseException, err:
        print err.line
        print " " * (err.column-1) + "^"
        print err
        sys.exit(1)

    nodot = '[nodot]' in result.asList()
    ret = dict(src=result['src'], dst=result.get('dst', None),
               nodot=nodot)

    return ret


def pref_symlink(filename, destination):
    if not os.path.exists(filename):
        error("Non trovo il file sorgente: %s" % (filename,))
        return

    if os.path.exists(destination):
        if os.path.islink(destination):
            abs_link = os.path.abspath(os.readlink(destination))
            if abs_link != filename:
                error("Il symlink esiste gia', ma e' sbagliato:")
                print "%s -> %s" % (destination, abs_link)
        else:
            error("E' gia' presente un file con quel nome e non e' quello "
                  "giusto: %s" % destination)
    else:
        mkdir_recursive(destination)
        success("ln -s %s %s" % (filename, destination))
        os.symlink(filename, destination)

def mkdir_recursive(filename):
    parent = os.path.dirname(filename)
    if not os.path.exists(parent):
        success("mkdir -p %s" % (filename,))
        os.makedirs(parent)

def main():
    config = parse_config_file('maggiordomo.cfg')
    pref_root = config.pop('preferences root')
    pref_dest = config.pop('destination directory')

    for name, element in config.items():
        print "Processing: %s" % (name,)
        item = parse_config_element(element)

        if item['dst'] is None:
            if item['nodot'] is True:
                item['dst'] = item['src']
            else:
                item['dst'] = '.' + item['src']

        source = os.path.join(pref_root, item['src'])
        destination = os.path.join(pref_dest, item['dst'])
        pref_symlink(source, destination)

if __name__ == '__main__':
    import sys
    sys.exit(main())

