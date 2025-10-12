# some common imports
import os  # noqa: F401
import sys  # noqa: F401
import json  # noqa: F401
from pprint import pprint
from importlib.util import find_spec


# use pprint() to print expressions
# https://docs.python.org/3/library/sys.html#sys.displayhook
sys.displayhook = pprint

# Import `see()` (better alternative to `dir()`) when it's available.
# https://ljcooke.github.io/see/
if find_spec("see") is not None:
    from see import see  # noqa: F401
