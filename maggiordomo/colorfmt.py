#!/usr/bin/env python
# Terminal color Formatter
#
# Usage:
# cf = ColorFormatter()
# print cf.format("String {yellow}with {green}{bold}{0}{bold_off}{yellow} colors!{default}", 'many')
#
# 25/Jul/2011 Daniel Kertesz <daniel@spatof.org>

import string

class ICaseDict(dict):
    def __getitem__(self, key):
        return super(ICaseDict, self).__getitem__(key.lower())

    def __setitem__(self, key, value):
        super(ICaseDict, self).__setitem__(key.lower(), value)

# a = ICaseDict({
#     'black': 30,
#     'green': 32,
#     })
# print a
# assert "green" in a
# # questo su python 2.7.1+ di ubuntu ritorna un AssertionError!?
# assert "GREEN" in a


COLORS = {
    # Foreground
    'black': 30,
    'red': 31,
    'green': 32,
    'yellow': 33,
    'blue': 34,
    'magenta': 35,
    'cyan': 36,
    'white': 37,
    'default': 39,

    # Background
    'bblack': 40,
    'bred': 41,
    'bgreen': 42,
    'byellow': 43,
    'bblue': 44,
    'bmagenta': 45,
    'bcyan': 46,
    'bwhite': 47,
    'bdefault': 49,

    # Effects ON
    'reset': 0,
    'bold': 1,
    'b': 1,
    'italic': 3,
    'underline': 4,
    'inverse': 7,
    'strikethrough': 9,

    # Effects OFF
    'bold_off': 22,
    '/b': 22,
    'italic_off': 23,
    'underline_off': 24,
    'inverse_off': 27,
    'strikethrough_off': 29,
}

# If you prefer uppercase color names, like {GREEN} and {BOLD_OFF} set this to `True`
UPPERCASE_COLORS = False
if UPPERCASE_COLORS:
    COLORS = dict((x.upper, y) for x, y in COLORS.items())

class ColorFormatter(string.Formatter):
    """
    Terminal color Formatter
    """

    def get_value(self, key, args, kwargs):
        if key in COLORS:
            return '\033[%dm' % COLORS[key]
        else:
            return super(ColorFormatter, self).get_value(key, args, kwargs)
cf = ColorFormatter()

def colorize(*args, **kwargs):
    return cf.format(*args, **kwargs)

if __name__ == '__main__':
    print cf.format("String {magenta}with {green}{bred}{bold}{0}{bdefault}{bold_off}{yellow} colors!{default}", 'many')
    print colorize("Ciao {bblue}{green}Minchia {0} {pippa}{reset}",
                   "Fava",
                   pippa="wilson")
    print colorize("{b}Hello{/b} {blue}there!{default}!")
