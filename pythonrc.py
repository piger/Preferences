# -*- encoding: utf-8 -*-
"""
Abilita la completion con <Tab> per l'interprete Python. Credo che sia utile
installare anche `readline` con brew.

Importa anche la comoda libreria `see` se presente.
"""

import readline
import rlcompleter

if 'libedit' in readline.__doc__:
    readline.parse_and_bind("bind ^I rl_complete")
else:
    readline.parse_and_bind("tab: complete")

# Import see()
try:
    from see import see
except ImportError:
    pass

# fancy prompt and pretty print
import pprint
import sys

sys.ps1 = "\033[0;34m>>> \033[0m"
sys.ps2 = "\033[1;34m... \033[0m"

sys.displayhook = pprint.pprint
