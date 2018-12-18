# -*- encoding: utf-8 -*-
import sys
import readline
import rlcompleter
from pprint import pprint

# Import `see()` (better alternative to `dir()`) when it's available.
# https://ljcooke.github.io/see/
try:
    from see import see
except ImportError:
    pass

# Configure the TAB keyboard key to execute the completion function.
if 'libedit' in readline.__doc__:
    readline.parse_and_bind("bind ^I rl_complete")
else:
    readline.parse_and_bind("tab: complete")

# prompt colors
sys.ps1 = "\033[0;34m>>> \033[0m"
sys.ps2 = "\033[1;34m... \033[0m"

# https://docs.python.org/2/library/sys.html#sys.displayhook
# use pprint() to print expressions
sys.displayhook = pprint
