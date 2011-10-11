#!/usr/bin/env python

import hashlib
import os
import sys
from Tkinter import *
from tkMessageBox import *
from tkFileDialog import askopenfilename, asksaveasfilename

class CopyDialog(Frame):
    CHUNK_SIZE = 1024

    def __init__(self, master = None):
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets()

        self.appendConsole("Select the source and destination.  Press \"Start copy\" to begin.")
        self.appendSeparator()

    def createWidgets(self):
        title = Label(self)
        title["text"] = "Unbreakable Copy"
        title["font"] = ("Arial", 24)
        title.grid({"row": 0, "column": 0, "columnspan": 2})

        sourceB = Button(self)
        sourceB["text"] = "Browse"
        sourceB["command"] = self.chooseOrigin
        sourceB.grid({"row": 1, "column": 0})

        sourceE = Entry(self)
        self.sourceV = StringVar()
        self.sourceV.set("Choose source")
        sourceE["textvariable"] = self.sourceV
        sourceE["width"] = 60
        sourceE.grid({"row": 1, "column": 1})

        destB = Button(self)
        destB["text"] = "Browse"
        destB["command"] = self.chooseDest
        destB.grid({"row": 2, "column": 0})

        destE = Entry(self)
        self.destV = StringVar()
        self.destV.set("Choose destination")
        destE["textvariable"] = self.destV
        destE["width"] = 60
        destE.grid({"row": 2, "column": 1})

        self.console = Text(self)
        self.console["height"] = 20
        self.console.grid({"row": 3, "column": 0, "columnspan": 2})

        startB = Button(self)
        startB["text"] = "Start copy"
        startB["command"] = self.doCopy
        startB.grid({"row": 4, "column": 0, "columnspan": 2})

    def appendConsole(self, s):
        self.console.insert("end", s + "\n")

    def appendSeparator(self):
        self.console.insert("end", (int(self.console["width"]) - 1) * "-" + "\n")

    def chooseOrigin(self):
        self.sourceV.set(askopenfilename())

    def chooseDest(self):
        self.destV.set(asksaveasfilename())

    def doCopy(self):
        src = self.sourceV.get()
        dest = self.destV.get()

        self.appendConsole("Copying from \"%s\" to \"%s\"" % (src, dest))

        try:
            fi = open(src, "rb")
            fo = open(dest, "wb")

            i = 0
            n = os.stat(src).st_size / 1024 + 1
            ok = True
            for i in range(0, n):
                data = None
                try:
                    data = fi.read(self.CHUNK_SIZE)
                except Exception, e:
                    self.appendConsole("Failed chunk at %d: %s" % (i,str(e)))
                    ok = False
                    fi = open(src, "rb")
                    fo.write("\0" * self.CHUNK_SIZE)
                    fi.seek(self.CHUNK_SIZE * i)
                    continue
                if not data:
                    break
                fo.write(data)
                self.appendConsole("Copied 1Kb chunk at %d (%d remaining)" % (i, n - i - 1))

            fo.close()
            fi.close()

            if ok:
                self.appendSeparator()
                self.appendConsole("Comparing source and destination files")
                m1 = self.md5(src)
                m2 = self.md5(dest)
                if m1 == m2:
                    self.appendConsole("Source and destination are binary equal")
                else:
                    self.appendConsole("Warning: Source and destination differ")
            self.appendSeparator()
            self.appendConsole("Done.")
        except Exception, e:
            self.appendConsole("Failed irrecoverably:\n" + str(e))

    def md5(self, fn):
        m = hashlib.md5()
        fi = open(fn, "rb")
        while True:
            data = fi.read(self.CHUNK_SIZE)
            if not data:
                break
            m.update(data)
        fi.close()
        return m.digest()

if __name__ == "__main__":
    root = Tk()
    app = CopyDialog(master = root)
    app.master.title("Copy from/to")
    if len(sys.argv) > 1:
        app.sourceV.set(sys.argv[1])
    if len(sys.argv) > 2:
        app.destV.set(sys.argv[2])
    app.mainloop()
