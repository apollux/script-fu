#!/usr/bin/python

from __future__ import print_function

import cairo
import gobject
import gtk
import string
import pygtk

class Screen(gtk.DrawingArea):
    __gsignals__ = { "expose-event": "override" }
    printable_chars = string.digits + string.letters + string.punctuation

    def __init__(self):
        super(Screen, self).__init__()

        self.text = [[]]

    def do_expose_event(self, event):
        cr = self.window.cairo_create()

        cr.rectangle(event.area.x, event.area.y,
                     event.area.width, event.area.height)
        cr.clip()

        self.draw(cr, *self.window.get_size())

    def on_key_press(self, event):
        def is_printable_char(key):
            return key in self.printable_chars

        keyname = gtk.gdk.keyval_name(event.keyval)
        if keyname == "Escape":
            gtk.main_quit()
        elif keyname == "space":
            self.text.append([])
        elif keyname == "Return":
            self.text = [[]]
        elif keyname == "BackSpace":
            last = self.text[-1]
            if len(last) > 0:
                self.text[-1] = last[:-1]
            elif len(self.text) > 1:
                del self.text[-1]
        elif event.keyval < 256 and is_printable_char(chr(event.keyval)):
            self.text[-1].append(chr(event.keyval))
        else:
            print("Unhandled key '%s' pressed" % (keyname, ))

        self.queue_draw()

    def draw(self, cr, width, height):
        cr.set_source_rgb(1.0, 1.0, 1.0)
        cr.paint()

        cr.set_source_rgb(0.0, 0.0, 0.0)
        cr.select_font_face("sans-serif")

        self.show_texts(cr, width, height, 40, 200)

    def show_texts(self, cr, width, height, min_text_size, max_text_size):
        text_size = min_text_size
        text = " ".join(("".join(cs) for cs in self.text))

        results = []
        for num_rows in xrange(1, len(self.text) + 1):
            badness = num_rows * 10
            print("Trying %d rows; badness = %f" % (num_rows, badness))
            results.append((num_rows, badness))

        size_increment = int(max_text_size - min_text_size)
        while size_increment > 0:
            cr.set_font_size(text_size + size_increment)
            (_, _, text_width, text_height, _, _) = cr.text_extents(text)
            if text_width < width * 0.9:
                text_size += size_increment
            size_increment = int(size_increment / 2)

        cr.set_font_size(text_size)
        (_, _, text_width, text_height, _, _) = cr.text_extents(text)
        cr.move_to(width / 2 - text_width / 2, height / 2 + text_height / 2)
        cr.show_text(text)

def main():
    window = gtk.Window()
    window.connect("delete-event", gtk.main_quit)
    widget = Screen()
    window.connect("key_press_event", lambda w, ev: widget.on_key_press(ev))
    window.add(widget)
    widget.show()
    window.present()
    gtk.main()

if __name__ == "__main__":
    main()
