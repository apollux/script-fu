#!/usr/bin/python

from __future__ import print_function

import cairo
import gobject
import gtk
import pygtk

class Screen(gtk.DrawingArea):
    __gsignals__ = { "expose-event": "override" }

    def do_expose_event(self, event):
        cr = self.window.cairo_create()

        cr.rectangle(event.area.x, event.area.y,
                     event.area.width, event.area.height)
        cr.clip()

        self.draw(cr, *self.window.get_size())

    def draw(self, cr, width, height):
        cr.set_source_rgb(1.0, 1.0, 1.0)
        cr.paint()

def main():
    def on_key_press(widget, event):
        keyname = gtk.gdk.keyval_name(event.keyval)
        if keyname == "Escape":
            gtk.main_quit()
        else:
            print("Unhandled key '%s' pressed" % (keyname, ))

    window = gtk.Window()
    window.connect("delete-event", gtk.main_quit)
    window.connect("key_press_event", on_key_press)
    widget = Screen()
    window.add(widget)
    widget.show()
    window.present()
    gtk.main()

if __name__ == "__main__":
    main()
