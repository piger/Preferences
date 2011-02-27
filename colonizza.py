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

from __future__ import with_statement
import os
import sys
import re
import filecmp
from os.path import expanduser, join, exists

from termcolor import colored, cprint

__author__ = 'sand <daniel@spatof.org>'
__version__ = '0.1'

# Dove mettero' il tutto?
#DEFAULT_DESTINATION = "~/Preferences"
DEFAULT_DESTINATION = "~/tmp/colonizza"

# Lista dei file da colonizzare.
DOTFILES = [
    'gitconfig',
    'gvimrc',
    'lftprc',
    'tmux.conf',
    'vim',
    'vimrc',
    'zsh',
    'zshenv',
]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# COLOR JIZZ
def title(name):
    cprint(name, 'green', attrs=['underline', 'bold'])
    cprint("Siamo i puntofile di Borg, la resistenza è inutile.\n", 'yellow')

s_warn = colored('***', 'red', attrs=['bold'])
s_error = colored('!!!', 'red', attrs=['bold'])
s_notice = colored('---', 'green', attrs=['bold'])
s_link = colored('->', 'green', attrs=['bold'])

warning = lambda x: cprint("{0} {1}".format(s_warn, x))
error = lambda x: cprint("{0} {1}".format(s_error, x))
notice = lambda x: cprint("{0} {1}".format(s_notice, x))

biancone = lambda x: colored(x, 'white', attrs=['bold'])

def lynkmsg(source, dest):
    csource = biancone(source)
    cdest = biancone(dest)

    print "{0} {1} {2} {3}".format(
        s_notice,
        csource,
        s_link,
        cdest
    )

def linka(source, dest):
    source_base = os.path.basename(source)
    lynkmsg(source_base, dest)
    os.symlink(source, dest)

def borg():
    destination = expanduser(DEFAULT_DESTINATION)
    cwd = os.getcwd()

    for dotfile in DOTFILES:
        fsource = join(cwd, dotfile)
        fdest = join(destination, '.' + dotfile)

        # Non esiste, posso linkare
        if not exists(fdest):
            linka(fsource, fdest)

        # E' un link
        elif os.path.islink(fdest):

            # Punta al file giusto ?
            ldest = os.readlink(fdest)
            if os.path.abspath(ldest) == fsource:
                continue

            warning("Removing link: %s -> %s" % (fdest, ldest))
            os.remove(fdest)
            linka(fsource, fdest)

        elif not filecmp.cmp(dotfile, fdest):
            if os.path.exists(fdest + '.bak'):
                warning("Non posso procedere: esiste già un file .bak di %s" % fdest)
                continue

            warning("Backuppin' %s" % fdest)
            os.rename(fdest, fdest + '.bak')
            linka(fsource, fdest)

        else:
            warning("Cosa devo fare con %s?" % dotfile)


def main():
    title("Colonizza %s" % __version__)
    borg()

if __name__ == '__main__':
    import sys
    sys.exit(main())
