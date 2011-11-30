# -*- encoding: utf-8 -*-
import atexit
import os.path
import sys

try:
    import readline
except ImportError:
    try:
        import pyreadline as readline
    except:
        pass
else:
    import rlcompleter

    class IrlCompleter(rlcompleter.Completer):
        def __init__(self, tab='	'):
            self.tab = tab
            rlcompleter.Completer.__init__(self)

        def complete(self, text, state):
            if text == '':
                readline.insert_text(self.tab)
                return None
            else:
                return rlcompleter.Completer.complete(self, text, state)

    if sys.platform == 'darwin':
        readline.parse_and_bind('bind ^I rl_complete')
    else:
        readline.parse_and_bind('tab: complete')

    readline.set_completer(IrlCompleter().complete)

history_path = os.path.expanduser('~/.pyhistory')
if os.path.isfile(history_path):
    readline.read_history_file(history_path)
readline.set_history_length(100)
atexit.register(lambda x=history_path: readline.write_history_file(x))

# import readline
# import rlcompleter
# 
# if 'libedit' in readline.__doc__:
#     readline.parse_and_bind("bind ^I rl_complete")
# else:
#     readline.parse_and_bind("tab: complete")
# 
# print "hello world"

# vim: ft=python
