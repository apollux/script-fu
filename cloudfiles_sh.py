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
    """Decorator for cmd.Cmd commands."""
    def f(self, line):
        line = line.strip()
        return fun(self, line)
    return f

def requires_login(fun):
    """Decorator that asserts that the user is logged in."""
    def f(self, line):
        if self.conn is None:
            print "Not logged in yet"
            return False
        return fun(self, line)
    return f

def interpret_exceptions(fun):
    """Decorator that catches recoverable exceptions and
prints them nicely."""
    def f(self, line):
        try:
            return fun(self, line)
        except MalformedRemotePath, e:
            print e
        except WrongNumberOfArguments, e:
            print e
        return False
    return f

def command_syntax(num_args):
    """Decorator that checks the number of arguments passed to commands."""
    def wrap(fun):
        def f(self, line):
            args = line.split()
            if len(args) != num_args:
                raise WrongNumberOfArguments(line, num_args)
            return fun(self, *args)
        return f
    return wrap

def show_progress(transferred, size):
    done = 50 * float(transferred) / size
    print "\r%.1f%% [%s>%s] %d/%d    " % (2 * done,
                                          "#" * int(math.ceil(done)),
                                          " " * int(50 - done),
                                          transferred, size),

class WrongNumberOfArguments(TypeError):
    def __init__(self, line, num_args):
        super(WrongNumberOfArguments, self).__init__(self)
        self.line = line
        self.num_args = num_args

    def __str__(self):
        return ("Wrong number of arguments " +
                "in `%s' (expected %d)" % (self.line,
                                           self.num_args))

class MalformedRemotePath(Exception):
    def __init__(self, remote_path):
        super(MalformedRemotePath, self).__init__(self)
        self.remote_path = remote_path

    def __str__(self):
        return "Malformed remote path: %s" % (self.remote_path,)

class RemotePath(object):
    def __init__(self, remote_path, object_optional=True):
        segs = [seg for seg in remote_path.split('/') if len(seg) > 0]
        self.container = None
        self.objekt = None
        if len(segs) == 1 and object_optional:
            self.container = segs[0]
        elif len(segs) == 2:
            (self.container, self.objekt) = (segs[0], segs[1])
        else:
            raise MalformedRemotePath(remote_path)

    def get_container(self):
        return self.container

    def get_object(self, default=None):
        if self.objekt is not None:
            return self.objekt
        return default

    def __str__(self):
        return "%s/%s" % (self.container, self.objekt)

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
        print """get <local-file> <container>[/<object>]
  if <object> is ommited, it defaults to basename(<local-file>)"""

    def help_put(self):
        print """put <container>/<object> <local-file>
  if <local-file> is a directory, the object is saved to <local-file>/<object>"""

    def help_copy(self):
        print """copy <container>/<object> <container>[/<object>]"""

    def help_remove(self):
        print """remove <container>/<object>"""

    @command
    @interpret_exceptions
    @command_syntax(num_args=2)
    def do_login(self, user, token):
        try:
            self.conn = cloudfiles.get_connection(
                            user, token,
                            authurl = cloudfiles.uk_authurl)
        except Exception, e:
            print "Login failed"

    @command
    @requires_login
    def do_ls(self, path):
        if path == "":
            for cont_name in self.conn.list_containers():
                print "%s/" % (cont_name,)
        else:
            container = self.get_container(RemotePath(path).get_container())
            if container is None:
                print "No such folder"
            else:
                for obj_name in container.list_objects():
                    print "%s/%s" % (container.name, obj_name)

    def complete_ls(self, text, line, begidx, endidx):
        return self.complete_last(line)

    @command
    @requires_login
    @interpret_exceptions
    @command_syntax(num_args=1)
    def do_info(self, path):
        remote = RemotePath(path, object_optional=False)
        obj = self.get_object(str(remote))
        if obj is None:
            print "No such object"
            return False

        print "%s/%s:" % (obj.container.name, obj.name)
        print "MD5: %s" % (obj.objsum,)
        print "Size: %s bytes" % (obj.size,)
        print "Last modified: %s" % (obj.last_modified,)
        if obj.metadata:
            print "Metadata:"
            for k, v in obj.metadata:
                print "    %s: %s" % (k, v)

    def complete_info(self, text, line, begidx, endidx):
        return self.complete_last(line)

    @command
    @requires_login
    @interpret_exceptions
    @command_syntax(num_args=2)
    def do_put(self, local, remote):
        local = os.path.expanduser(local)
        if not os.path.isfile(local):
            print "No such file: %s" % (local,)
            return False

        remote = RemotePath(remote)

        container = self.conn.create_container(remote.get_container())
        obj = container.create_object(
                  remote.get_object(default=os.path.basename(local)))

        obj.load_from_filename(local, callback=show_progress)
        print

    def complete_put(self, text, line, begidx, endidx):
        return self.complete_last(line)

    @command
    @requires_login
    @interpret_exceptions
    @command_syntax(num_args=2)
    def do_get(self, remote, local):
        remote = RemotePath(remote, object_optional=False)

        container = self.get_container(remote.get_container())
        if container is None:
            print "Remote does not exist"
            return False

        local = os.path.expanduser(local)
        if os.path.isdir(local):
            local = os.path.join(local, container.get_object())

        obj = self.get_object(remote.get_object(), container=container)
        if obj is None:
            print "Remote does not exist"
            return False

        obj.save_to_filename(local, callback=show_progress)
        print

    def complete_get(self, text, line, begidx, endidx):
        return self.complete_last(line)

    @command
    @requires_login
    @interpret_exceptions
    @command_syntax(num_args=2)
    def do_copy(self, src, dest):
        src = RemotePath(src, object_optional=False)
        dest = RemotePath(dest)

        src_obj = self.get_object(str(src))
        if src is None:
            print "Source does not exist"
            return False

        self.conn.create_container(dest.get_container())
        src_obj.copy_to(dest.get_container(),
                        dest.get_object(default=src.get_object()))

    def complete_copy(self, text, line, begidx, endidx):
        return self.complete_last(line)

    @command
    @requires_login
    @interpret_exceptions
    @command_syntax(num_args=1)
    def do_remove(self, path):
        remote = RemotePath(path, object_optional=False)

        container = self.get_container(remote.get_container())
        if container is None:
            print "Remote does not exist"
            return False

        try:
            container.delete_object(remote.get_object())
        except cloudfiles.errors.ResponseError, e:
            print "CloudFiles error: %s" % (str(e),)

    def complete_remove(self, text, line, begidx, endidx):
        return self.complete_last(line)

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
                remote = RemotePath(path, object_optional=False)
                container = self.get_container(remote.get_container())
                name = remote.get_object()
            else:
                name = path

            if container is not None:
                return container.get_object(name)
        except Exception, e:
            pass

        return None

    def complete_last(self, line):
        if self.conn is None:
            return []
        segs = line.split()
        prefix = ""
        if len(segs) > 1:
            prefix = segs[-1]

        return self.get_remote_completions(prefix)

    def get_remote_completions(self, prefix):
        if prefix.find('/') == -1:
            cs = self.conn.list_containers()
            return [c for c in cs if c.find(prefix) == 0]
        else:
            remote = RemotePath(prefix)
            container = self.get_container(remote.get_container())
            if container is None:
                return []
            objs = container.list_objects()
            return [o for o in objs
                    if o.find(remote.get_object(default="")) == 0]


def main(args):
    console = CloudFilesConsole()
    console.cmdloop(banner)

    print ""
    print "Done"

if __name__ == "__main__":
    main(sys.argv[1:])

