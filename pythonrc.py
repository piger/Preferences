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

# prompt colors (in a format that doesn't mess with the REPL)
# https://stackoverflow.com/a/10953944
# https://stackoverflow.com/a/9468954
sys.ps1 = '\001\033[96m\002>>> \001\033[0m\002'
sys.ps2 = '\001\033[96m\002... \001\033[0m\002'

# https://docs.python.org/2/library/sys.html#sys.displayhook
# use pprint() to print expressions
sys.displayhook = pprint
