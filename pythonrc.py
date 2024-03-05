# some common imports
import os
import sys
import json
from pprint import pprint


# use pprint() to print expressions
# https://docs.python.org/3/library/sys.html#sys.displayhook
sys.displayhook = pprint

# Import `see()` (better alternative to `dir()`) when it's available.
# https://ljcooke.github.io/see/
try:
    from see import see
except ImportError:
    pass
