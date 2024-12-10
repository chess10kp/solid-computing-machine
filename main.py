# pyright: reportMissingTypeStubs=false
# pyright: reportUnknownMemberType=false
# pyright: reportUntypedBaseClass=false
# pyright: reportAttributeAccessIssue=false
# type: ignore

import sys
import os
import subprocess
from pprint import pprint
from exceptions import EmacsUnavailableException

import gi

gi.require_version("Gtk", "4.0")

from gi.repository import GLib, Gtk  # noqa: E402


TIME_PATH = os.path.expanduser("~/.time")


def read_time() -> str:
    return open(TIME_PATH).read().strip()


def is_running(process_name: str) -> bool:
    try:
        if not os.name == "nt":
            output = subprocess.check_output(["pgrep", "emacs"])
            return output.lower()
    except subprocess.CalledProcessError:
        return False

def get_agenda() -> str:
    """Gets the agenda for today, then closes the agenda buffer"""
    # get_agenda_command =  ("(progn  (setq org-agenda-custom-commands  '((\"d\" \"Daily agenda and all TODOs\"  ((agenda \"\" ((org-agenda-span 1)))))))  (org-batch-agenda \"d\"))"
    #                        )

    emacs_agenda = '(progn \
    (require \'org-agenda) \
    (let ((org-agenda-span \'day)) \
    (org-batch-agenda "a")))'

    output = subprocess.run(
        ["emacs", "-batch", "-l" ,"~/.emacs.d/init.el", "-eval", emacs_agenda], text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    ).stdout 

    return output


def parse_agenda() -> str:
    if not is_running("emacs"):
        raise Exception("AgendaOffline")

    try:
        agenda = get_agenda()
    except EmacsUnavailableException as e:
        print(e)
        sys.exit(-1)

    agenda = agenda.splitlines()
    todos = filter(lambda x: "todo" in x, agenda)
    todos = list(map(lambda x: x[x.find(":")+1:].strip(), todos))
    return todos


class Dashboard(Gtk.ApplicationWindow):
    def __init__(self, **kargs):
        super().__init__(**kargs, title="dashbaord")
        self.set_default_size(400, 300)
        box = Gtk.Box(spacing=6)
        self.set_child(box)

        self.timeButton = Gtk.Button(label=read_time())
        self.timeButton.connect("clicked", self.on_time_button_clicked)
        box.append(self.timeButton)

        button2 = Gtk.Button(label="by", hexpand=True)
        button2.connect("clicked", self.on_button_clicked)
        box.append(button2)

    def on_time_button_clicked(self, _w):
        # access the button object's label
        new_label = self.timeButton.get_label()
        self.timeButton.set_label(str(int(new_label) + 1))
        with open(TIME_PATH, "w") as f:
            f.write(new_label)

    def on_button_clicked(self, _w):
        print("dashboard_exit")
        self.close()


def on_activate(app):
    win = Dashboard(application=app)
    win.present()


app = Gtk.Application(application_id="com.example")
app.connect("activate", on_activate)

app.run(None)

sys.exit(0)
