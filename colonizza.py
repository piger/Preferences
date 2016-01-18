#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Finalmente, l'impero è arrivato.

                _                                       |
               ;P'                         .........,-- |
               8P                                       |
             ,8P _,-....__                         __    \_
            o8'-'     o.  `-.._              ,o--''        ._
          ,dP'        `YYbooo__`'`--------=           .      `-..__,,.-
         ,88             _[8888P        _,'         ,'
        d88'        _,oo8PP''         Y`       _,,-'       /
       ,88/      ,d88"'             b-'        '          ,'      |
       88/,b-'`>. '`YYbooo__       ./                    /       |
      ]8['       `..   `'`"PP8    ,-                   ,'        |
      ]8[           `-.__         \                   /         .'
      d8|                `''-------`
     ]8[
     ,8.                    'finalmente una configurazione seria!'
     `P'
  ,dFcccd_.._oo._      "madonna!"
cSFb888P8j8888?88b               'CRISTO, ERA ORA!'     "DAJE Zì!"
J888888888888Ybo?$8o.
 'i;::'""F?hc8c888d$8b.
''           `''`"?FF$88cccccccccccccccc.__
Il colonizzatore.     `'YP''''''''''`-::;;?F?hccc.
Colui che ficca la configurazione nel          '''`-..._
posto giusto, facendo dei backup e usando i colori      ``-..___
per farti capire quando iniziare a strippare.                   `''

<<Il colonizzatore non è un coione e nfatti usa i symlink verso 'sta directory co
tutti i puntofile, e si trova un file co 'o stesso nome je accolla n'altro nome
de backup e se lo leva dar cazzo.>>
         -- Anonimo

"""
import os
import sys
import optparse


def link(dotsrc, dotdest, opts):
    """Create a symlink for the source file in the destination directory/filename"""

    src = os.path.join(os.path.abspath(opts.source), dotsrc)
    if not dotdest:
        base_src = os.path.basename(src)
        dest = os.path.join(os.path.abspath(opts.target), '.' + base_src)
    else:
        dest = os.path.join(os.path.abspath(opts.target), dotdest)

    if os.path.islink(dest):
        lpath = os.readlink(dest)
        if lpath.lower() != src.lower():
            if opts.force:
                os.unlink(dest)
            else:
                print "[*] '%s' exists and it's a symlink to '%s', not '%s'" % (
                    dest, lpath, src)
                return False
        else:
            return False
    elif os.path.exists(dest):
        print "[*] '%s' already exists" % dest
        return False

    if not os.path.exists(os.path.dirname(dest)):
        os.makedirs(os.path.dirname(dest), mode=0770)

    print "[+] %s -> %s" % (dest, src)
    os.symlink(src, dest)
    return True

def read_config(filename):
    """Read a simple configuration file and returns a list of tuples containing
    the source file and the destination file."""

    links = []
    with open(filename) as fd:
        for line in fd:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            if ' ' in line:
                src, dest = line.split(' ', 1)
            else:
                src, dest = line, None
            links.append((src, dest))
    return links

def main():
    parser = optparse.OptionParser(description="Create symlinks for dotfiles.")
    parser.add_option('-f', '--force', action='store_true',
                      help="Overwrite wrong symlinks")
    parser.add_option('-t', '--target', default=os.environ.get('HOME'),
                      metavar='DIR',
                      help="Destination directory (default: %s)" % os.environ.get('HOME'))
    parser.add_option('-s', '--source', default='.', metavar='DIR',
                      help="Source directory (default: .)")
    parser.add_option('-c', '--config', default='dotfiles.cfg', metavar='FILE',
                      help="Path to the configuration file (default: dotfiles.cfg)")
    opts, args = parser.parse_args()

    if not os.path.exists(opts.config):
        parser.error("Cannot find configuration file %s." % opts.config)

    for dotsrc, dotdest in read_config(opts.config):
        link(dotsrc, dotdest, opts)


if __name__ == '__main__':
    main()
