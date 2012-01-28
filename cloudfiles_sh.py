#!/usr/bin/env python

import atexit
import cloudfiles
from cmd import Cmd
import math
import os
import readline
import sys

banner = """CloudFiles interactive shell
"""

def command(fun):
    def f(self, line):
        line = line.strip()
        return fun(self, line)
    return f

def requires_login(fun):
    def f(self, line):
        if self.conn is None:
            print "Not logged in yet"
            return False
        return fun(self, line)
    return f

def show_progress(transferred, size):
    done = 50 * float(transferred) / size
    print "\r%.1f%% [%s>%s] %d/%d    " % (2 * done,
                                          "#" * int(math.ceil(done)),
                                          " " * int(50 - done),
                                          transferred, size),

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

    def emptyline(self):
        pass

    def help_login(self):
        print "login <username> <api-token>"

    def help_ls(self):
        print "ls [<container>]"

    def help_info(self):
        print "info <container>/<object>"

    def help_get(self):
        print """get <local-file> <container>/<object>
  if <object> is empty, it defaults to basename(<local-file>)"""

    def help_put(self):
        print """put <container>/<object> <local-file>
  if <local-file> is a directory, the object is saved to <local-file>/<object>"""

    @command
    def do_login(self, line):
        try:
            [user, token] = line.split()
        except Exception, e:
            print "Malformed command"
            return False

        try:
            self.conn = cloudfiles.get_connection(
                            user, token,
                            authurl = cloudfiles.uk_authurl)
        except Exception, e:
            print "Login failed"

    @command
    @requires_login
    def do_ls(self, line):
        if line == "":
            for cont_name in self.conn.list_containers():
                print "%s/" % (cont_name,)
        else:
            container = self.get_container(line)
            if container is None:
                print "No such folder"
            else:
                for obj_name in container.list_objects():
                    print "%s/%s" % (container.name, obj_name)

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

    @command
    @requires_login
    def do_put(self, line):
        try:
            [local, remote] = line.split()
        except Exception, e:
            print "Malformed command"
            return False

        local = os.path.expanduser(local)
        if not os.path.isfile(local):
            print "No such file: %s" % (local,)
            return False

        try:
            [container, obj] = remote.split('/')
        except Exception, e:
            print "Malformed remote: %s" % (remote,)
            return False

        container = self.conn.create_container(container)
        if obj == "":
            obj = os.path.basename(local)
        obj = container.create_object(obj)

        obj.load_from_filename(local, callback=show_progress)
        print

    @command
    @requires_login
    def do_get(self, line):
        try:
            [remote, local] = line.split()
        except Exception, e:
            print "Malformed command"
            return False

        try:
            [container, obj] = remote.split('/')
        except Exception, e:
            print "Malformed remote: %s" % (remote,)
            return False

        container = self.get_container(container)
        if container is None:
            print "Remote does not exist"
            return False

        local = os.path.expanduser(local)
        if os.path.isdir(local):
            local = os.path.join(local, obj)

        obj = self.get_object(obj, container=container)
        if obj is None:
            print "Remote does not exist"
            return False

        obj.save_to_filename(local, callback=show_progress)
        print

    @command
    @requires_login
    def do_copy(self, line):
        try:
            [src, dest] = line.split()
        except Exception, e:
            print "Malformed command"
            return False

        try:
            [dest_container, dest_obj] = dest.split('/')
        except Exception, e:
            print "Malformed remote: %s" % (dest,)
            return False

        src = self.get_object(src)
        if src is None:
            print "Source does not exist"
            return False

        self.conn.create_container(dest_container)
        src.copy_to(dest_container, dest_obj)

    @command
    @requires_login
    def do_remove(self, line):
        try:
            [container, obj] = line.split('/')
        except Exception, e:
            print "Malformed remote: %s" % (dest,)
            return False

        container = self.get_container(container)
        if container is None:
            print "Remote does not exist"
            return False

        try:
            container.delete_object(obj)
        except cloudfiles.errors.ResponseError, e:
            print "CloudFiles error: %s" % (str(e),)

    def do_EOF(self, _line):
        return True

    def get_container(self, name):
        try:
            return self.conn.get_container(name)
        except Exception, e:
            return None

    def get_object(self, path, container = None):
        try:
            if container is None:
                [container, name] = path.split('/')
                container = self.get_container(container)
            else:
                name = path

            if container is not None:
                return container.get_object(name)
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

