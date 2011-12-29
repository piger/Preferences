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
import sys
import os
import string
from colorfmt import colorize
from pyparsing import (Word, dblQuotedString, CaselessLiteral, Optional,
                       ZeroOrMore, removeQuotes, ParseException, StringEnd)


def error(text):
    """Stampa un messaggio di errore."""
    print colorize("{red}ERROR{default}: %s" % text)

def success(text):
    """Stampa un messaggio di successo."""
    print colorize("{green}OK{default} %s" % text)

def notice(text):
    """Stampa un messaggio informativo."""
    print colorize("{green}NOTICE{default} %s" % text)


class Parser(object):
    """Il parser che usa PyParsing per creare oggetti DotFile leggendo il file
    di configurazione.

    """
    def __init__(self):
        self.parser = self._create_parser()

    def _create_parser(self):
        """Assembla il parser PyParsing e ritorna un oggetto `Parser`."""
        simple = Word(string.letters + string.digits + string.punctuation)
        quoted = dblQuotedString.setParseAction(removeQuotes)
        single = quoted | simple

        src = CaselessLiteral("src:") + single.setResultsName("src")
        dst = CaselessLiteral("dst:") + single.setResultsName("dst")
        operator = CaselessLiteral("[nodot]") | CaselessLiteral("[nofoo]")

        parser = src + Optional(dst) + ZeroOrMore(operator) + StringEnd()
        return parser

    def parse(self, name, text):
        """Parsa una stringa di testo e ritorna un `DotFile`."""
        try:
            result = self.parser.parseString(text)
        except ParseException, err:
            print err.line
            print " " * (err.column-1) + "^"
            print err
            return None

        src = result['src']
        assert src is not None
        assert len(src) > 0
        dst = result.get('dst', None)
        nodot = '[nodot]' in result.asList() # True or False
        return DotFile(name, src, dst, nodot)

parser = Parser()


class Maggiordomo(object):
    """La classe che gestisce la creazione dei symlink.

    L'interfaccia pubblica e' il singolo metodo `work()`, che processa ogni
    dotfile presente in `self.dotfiles` e crea i symlink.

    Qui si potrebbero implementare altri elementi, ad esempio dei repository
    *git* da pullare...
    """
    def __init__(self, pathDotFiles, pathDestination, dotfiles):
        self.pathDotFiles = pathDotFiles
        self.pathDestination = pathDestination
        self.dotfiles = list(dotfiles)

    def work(self):
        """Processa i dotfile creando i symlink."""
        for dotfile in self.dotfiles:
            print "+ %s" % dotfile.name
            srcBasePath = dotfile.get_source()
            srcPath = os.path.join(self.pathDotFiles, srcBasePath)

            dstBasePath = dotfile.get_destination()
            dstPath = os.path.join(self.pathDestination, dstBasePath)

            self._create_symlink(srcPath, dstPath)

    def _create_symlink(self, filename, destination):
        """Crea un symlink tra due path assoluti."""
        if not os.path.exists(filename):
            error("Non trovo il file sorgente: %s" % (filename,))
            return

        # Il file destinazione esiste gia'
        if os.path.exists(destination):
            # E' un symlink?
            if os.path.islink(destination):
                # Controlla il symlink
                self._check_symlink(filename, destination)
            else:
                error("E' gia' presente un file con quel nome e non e' quello "
                      "giusto: %s" % destination)
        else:
            # Crea le directory necessarie
            mkdir_recursive(destination)
            # E il symlink
            success("ln -s %s %s" % (filename, destination))
            os.symlink(filename, destination)

    def _check_symlink(self, srcFilename, dstFilename):
        """Controlla che il symlink `dstFilename` punti a `srcFilename`."""
        absLinkPath = os.path.abspath(os.readlink(dstFilename))
        if srcFilename != absLinkPath:
            error("Il symlink esiste gia', ma e' sbagliato:")
            print "%s -> %s" % (dstFilename, absLinkPath)
        else:
            notice("Il symlink esiste gia' ed e' corretto.")


class DotFile(object):
    """Un singolo dotfile.

    Per ottenere i path sorgente e destinazione bisogna usare `get_source()` e
    `get_destination()`, che sono l'interfaccia pubblica.

    Methods:

    Dotfile.get_source()
        Ritorna il filename del dotfile sorgente.

    Dotfile.get_destination()
        Ritorna il filename del dotfile destinazione, aggiungendo un '.' davanti
        al nome se self._nodot e' False.
    """
    def __init__(self, name, src, dst, nodot):
        self.name = name
        self._src = src
        self._dst = dst
        self._nodot = nodot

    def get_source(self):
        return self._src

    def get_destination(self):
        if self._dst is None:
            destination = self._src
        else:
            destination = self._dst
        if self._nodot is True:
            return destination
        else:
            return ".%s" % destination

    def __repr__(self):
        return "%s(name=%r, src=%r, dst=%r, nodot=%r)" % (
            self.__class__.__name__, self.name, self._src, self._dst,
            self._nodot)


def parse_config_file(filename):
    """Parsa il file di configurazione e ritorna un dict.
    
    Il file di configurazione e' nel banal formato var=value; due opzioni
    necessarie sono `destination directory` e `preferences root`.

    Esempio:

    # Commento
    preferences root = /Users/sand/preferences
    destination directory = ~

    tmux = src:tmux.conf.test dst:tmux.conf
    pythonrc = src:pythonrc.py
    vimrc = src:vimrc
    gvimrc = src:gvimrc
    vim = src:vim
    zshenv = src:zshenv
    zsh = src:zsh
    lftp = src:lftprc dst:foobar.cfg [nodot]
    """
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

def mkdir_recursive(filename):
    """Prende il path di `filename` e crea le directory intermedie."""
    parent = os.path.dirname(filename)
    if not os.path.exists(parent):
        success("mkdir -p %s" % (filename,))
        os.makedirs(parent)

def main():
    config = parse_config_file('maggiordomo.cfg')
    pref_root = config.pop('preferences root')
    pref_dest = config.pop('destination directory')
    dotfiles = []

    for name, element in config.items():
        dotfile = parser.parse(name, element)
        if dotfile is None:
            error("Invalid dotfile: %s" % name)
            sys.exit(1)
        dotfiles.append(dotfile)

    m = Maggiordomo(pref_root, pref_dest, dotfiles)
    m.work()

if __name__ == '__main__':
    main()
