#!/usr/bin/env python

import atexit
import cloudfiles
from cmd import Cmd
import os
import readline
import sys

banner = """CloudFiles interactive shell
"""

class CloudFilesConsole(Cmd):
    def __init__(self):
        Cmd.__init__(self)
        self.prompt = ">>> "
        self.init_history(os.path.expanduser("~/.cloudfiles_history"))

        self.conn = None

    def init_history(self, histfile):
        if hasattr(readline, "read_history_file"):
            try:
                readline.read_history_file(histfile)
            except IOError:
                pass
            atexit.register(self.save_history, histfile)

    def save_history(self, histfile):
        readline.write_history_file(histfile)

    def help_login(self):
        print "login <username> <api-token>"

    def do_login(self, line):
        try:
            [user, token] = line.split()
        except Exception, e:
            print "Malformed command"
            return False

        try:
            conn = cloudfiles.get_connection(user, token,
                                             authurl = cloudfiles.uk_authurl)
        except Exception, e:
            print "Login failed"
            return False

    def do_EOF(self, _line):
        return True

def main(args):
    console = CloudFilesConsole()
    console.cmdloop(banner)

    print ""
    print "Done"

if __name__ == "__main__":
    main(sys.argv[1:])

