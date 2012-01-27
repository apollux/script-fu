#!/usr/bin/env python

import atexit
import cloudfiles
from cmd import Cmd
import os
import readline
import sys

banner = """CloudFiles interactive shell
"""

def command(fun):
    def f(self, line):
        line = line.strip()
        fun(self, line)
    return f

def requires_login(fun):
    def f(self, line):
        if self.conn is None:
            print "Not logged in yet"
            return False
        fun(self, line)
    return f

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

    def help_ls(self):
        print "ls [<container>]"

    def help_info(self):
        print "info <container>/<object>"

    @command
    def do_login(self, line):
        try:
            [user, token] = line.split()
        except Exception, e:
            print "Malformed command"
            return False

        try:
            self.conn = cloudfiles.get_connection(user, token,
                                                  authurl = cloudfiles.uk_authurl)
        except Exception, e:
            print "Login failed"

    @command
    @requires_login
    def do_ls(self, line):
        cs = self.conn.get_all_containers()
        if line == "":
            for c in cs:
                print "%s/" % (c.name,)
        else:
            for c in cs:
                if c.name.find(line) == 0:
                    objs = c.get_objects()
                    for o in objs:
                        print "%s/%s" % (c.name, o.name)
                    return False
            print "No such folder"

    @command
    @requires_login
    def do_info(self, line):
        o = self.get_object(line)
        if o is None:
            print "No such object"
            return False

        print "%s/%s:" % (o.container.name, o.name)
        print "MD5: %s" % (o.objsum,)
        print "Size: %s bytes" % (o.size,)
        print "Last modified: %s" % (o.last_modified,)
        if o.metadata:
            print "Metadata:"
            for k, v in o.metadata:
                print "    %s: %s" % (k, v)

    def do_EOF(self, _line):
        return True

    def get_container(self, name):
        cs = self.conn.get_all_containers()
        cs = [c for c in cs if c.name == name]
        if len(cs) == 1:
            return cs[0]
        return None

    def get_object(self, path, container = None):
        try:
            if container is None:
                [container, name] = path.split('/')
                container = self.get_container(container)
            else:
                name = path

            if container is not None:
                objs = container.get_objects()
                objs = [obj for obj in objs if obj.name == name]
                if len(objs) == 1:
                    return objs[0]
        except Exception, e:
            pass

        return None

def main(args):
    console = CloudFilesConsole()
    console.cmdloop(banner)

    print ""
    print "Done"

if __name__ == "__main__":
    main(sys.argv[1:])

