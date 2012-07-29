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
        line_spacing = 1.6

        def text_dims(text):
            (_, _, text_width, text_height, _, _) = cr.text_extents(text)
            return (text_width, text_height)

        def pack_words(ws, font_size, twidth):
            rows = []
            i = 0
            badness = 0.0
            cr.set_font_size(font_size)
            (a_width, a_height) = text_dims("a")
            l = len(ws)
            sum_h = 0.0
            while i < l:
                (cwidth, _) = text_dims(ws[i])
                row = [ws[i]]
                i += 1
                while i < l:
                    (nwidth, h) = text_dims(ws[i])
                    if cwidth + nwidth + a_width <= twidth:
                        cwidth += nwidth + a_width
                        row.append(ws[i])
                        i += 1
                    else:
                        break
                # print("twidth = %f; cwidth = %f" % (twidth, cwidth))
                badness += max(0.0, (abs(twidth - cwidth) - twidth * 1.0 / 6.0))
                sum_h += a_height * line_spacing
                rows.append(row)
            if sum_h > height * 5.0 / 6.0:
                badness = float("inf")
            return (badness, rows)

        words = ["".join(cs) for cs in self.text]

        twidth = 2.0 / 3.0 * width

        text_size = min_text_size
        # print("initial text_size = %d" % (text_size,))
        size_increment = int(max_text_size - min_text_size)
        (best_badness, best_rows) = pack_words(words, text_size, twidth)
        best_badness += 1.6 ** (max_text_size - text_size)
        while size_increment > 0:
            # print("computing for size_increment = %d" % (size_increment, ))
            (badness, rows) = pack_words(words, text_size + size_increment, twidth)
            badness += 1.6 ** (max_text_size - (text_size + size_increment))
            # print("badness = %f" % (badness,))
            if badness < best_badness:
                text_size += size_increment
                # print("text_size = %d" % (text_size, ))
                best_rows = rows
                best_badness = badness
            size_increment = int(size_increment / 2)

        # print("displaying text: %d" % (text_size, ))

        cr.set_font_size(text_size)
        (a_width, a_height) = text_dims("a")
        y = a_height * line_spacing
        # print("best_rows = %s" % (str(best_rows), ))
        for row in best_rows:
            x = width * 1.0 / 6.0
            for w in row:
                cr.move_to(x, y)
                cr.show_text(w)
                (w, _) = text_dims(w)
                x += w + a_width
            y += a_height * line_spacing

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
