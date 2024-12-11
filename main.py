# pyright: reportMissingTypeStubs=false
# pyright: reportUnknownMemberType=false
# pyright: reportUntypedBaseClass=false
# pyright: reportAttributeAccessIssue=false
# pyright: reportUnusedCallResult=false
# type: ignore

import sys
import os
import subprocess
from pprint import pprint
from typing_extensions import final
from exceptions import (
    EmacsUnavailableException,
    NotLinuxException,
    NoValueFoundException,
)

import gi

gi.require_version("Gtk", "4.0")

from gi.repository import GLib, Gtk  # noqa: E402


TIME_PATH = os.path.expanduser("~/.time")


def read_time() -> str:
    return open(TIME_PATH).read().strip()


def is_running(process_name: str) -> bool:
    try:
        if not os.name == "nt":
            output = subprocess.check_output(["pgrep", process_name])
            return output.lower() != b""
        else:
            raise NotLinuxException()
    except subprocess.CalledProcessError:
        return False


def get_agenda() -> str:
    """Gets the agenda for today, then closes the agenda buffer"""
    # get_agenda_command =  ("(progn  (setq org-agenda-custom-commands  '((\"d\" \"Daily agenda and all TODOs\"  ((agenda \"\" ((org-agenda-span 1)))))))  (org-batch-agenda \"d\"))"
    #                        )

    emacs_agenda = "(progn \
    (require 'org-agenda) \
    (let ((org-agenda-span 'day)) \
    (org-batch-agenda \"a\")))"

    output = subprocess.run(
        ["emacs", "-batch", "-l", "~/.emacs.d/init.el", "-eval", emacs_agenda],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).stdout

    return output


def parse_agenda() -> list[str]:
    if not is_running("emacs"):
        raise Exception("AgendaOffline")

    try:
        agenda = get_agenda()
    except EmacsUnavailableException as e:
        print(e)
        sys.exit(-1)

    agenda = agenda.splitlines()
    todos = list(map(
        lambda x: x[x.find(":") + 1 :].strip(), filter(lambda x: "todo" in x, agenda)
    ))
    return todos


def VBox() -> Gtk.Box:
    return Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)


def HBox() -> Gtk.Box:
    return Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=6)


@final
class Dashboard(Gtk.ApplicationWindow):
    def __init__(self, **kwargs):
        super().__init__(
            **kwargs,
            title="dashbaord",
            show_menubar=False,
            child=None,
            fullscreened=False,
            default_width=400,
            default_height=300,
            destroy_with_parent=True,
            hide_on_close=False,
            resizable=False,
            visible=True,
        )

        self.grid = Gtk.Grid()
        # half the grid shows the weather
        # the other half shows the agenda
        self.set_child(self.grid)

        self.weather = HBox()
        self.grid.attach(self.weather, 0, 0, 1, 1)

        agenda_box = Gtk.ListBox()
        self.grid.attach(agenda_box, 1, 0, 1, 1)

        self.agenda = parse_agenda()
        for i in range(len(self.agenda)):
            label = Gtk.Label(label=self.agenda[i])
            agenda_box.append(label)

        self.timeBox = Gtk.Box()
        self.timeButton = Gtk.Button(label=read_time())
        self.timeButton.connect("clicked", self.on_time_button_clicked)
        self.timeBox.attach(self.timeButton)
        self.grid.attach(self.timeBox, 0, 0, 1, 1)

        button2 = Gtk.Button(label="by", hexpand=True)
        button2.connect("clicked", self.on_button_clicked)
        agenda_box.append(button2)

    def on_time_button_clicked(self, _w: Gtk.Button) -> None:
        new_label = self.timeButton.get_label()
        if new_label is not None:
            with open(TIME_PATH, "w") as f:
                f.write(new_label)
        else:
            raise NoValueFoundException("timeButton does not hold a label value")

        self.timeButton.set_label(str(int(new_label) + 1))

    def on_button_clicked(self, _w: Gtk.Button):
        print("dashboard_exit")
        self.close()


def on_activate(app: Gtk.Application):
    win = Dashboard(application=app)
    win.present()


app = Gtk.Application(application_id="com.example")
app.connect("activate", on_activate)

app.run(None)

sys.exit(0)
