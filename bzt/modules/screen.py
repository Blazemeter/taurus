# coding=utf-8
"""
Copyright 2015 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
from Tkinter import Tk, Text
import Tkinter
import logging
import re

from urwid import BaseScreen


class DummyScreen(BaseScreen):
    """
    Null-object for Screen on non-tty output
    """

    def __init__(self, cols, rows):
        super(DummyScreen, self).__init__()
        self.size = (cols, rows)
        self.ansi_escape = re.compile(r'\x1b[^m]*m')

    def get_cols_rows(self):
        """
        Dummy cols and rows

        :return:
        """
        return self.size

    def draw_screen(self, size, canvas):
        """

        :param size:
        :type canvas: urwid.Canvas
        """
        data = ""
        for char in canvas.content():
            line = ""
            for part in char:
                if isinstance(part[2], str):
                    line += part[2]
                else:
                    line += part[2].decode()
            data += "%s│\n" % line
        data = self.ansi_escape.sub('', data)
        logging.info("Screen %sx%s chars:\n%s", size[0], size[1], data)


class GUIScreen(BaseScreen):
    """
    :type root: Tk
    """

    def __init__(self):
        super(GUIScreen, self).__init__()
        self.root = None
        self.size = (0, 0)

    def get_cols_rows(self):
        """
        Dummy cols and rows

        :return:
        """
        return self.size

    def _start(self):
        super(GUIScreen, self)._start()
        self.root = Tk()
        self.root.geometry("800x600")
        self.root.bind("<Configure>", self.resize)
        self.root.protocol("WM_DELETE_WINDOW", self.closed_window)
        self.text = Text(self.root)

    def _stop(self):
        if self.root:
            self.root.destroy()
        super(GUIScreen, self)._stop()

    def resize(self, event):
        logging.debug("Resized %s x %s", event.width, event.height)
        self.size = (int(event.width / 10), int(event.height / 10))

    def closed_window(self):
        self.root.destroy()
        self.root = None

    def draw_screen(self, size, canvas):
        """

        :param size:
        :type canvas: urwid.Canvas
        """
        data = ""
        for char in canvas.content():
            line = ""
            for part in char:
                if isinstance(part[2], str):
                    line += part[2]
                else:
                    line += part[2].decode()
            data += "%s\n" % line
        if self.root:
            self.text.delete("1.0", Tkinter.END)
            self.text.insert(Tkinter.END, data)
            self.text.pack()
            self.root.update()

