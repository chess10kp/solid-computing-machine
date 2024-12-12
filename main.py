# pyright: reportMissingTypeStubs=false
# pyright: reportUnknownMemberType=false
# pyright: reportUntypedBaseClass=false
# pyright: reportAttributeAccessIssue=false
# pyright: reportUnusedCallResult=false
# pyright: reportUnknownVariableType=false
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
from weather import get_weather
import style
from datetime import datetime as dt
import calendar

import asyncio
import gi


# For GTK4 Layer Shell to get linked before libwayland-client we must explicitly load it before importing with gi
from ctypes import CDLL
CDLL('libgtk4-layer-shell.so')


gi.require_version("Gtk", "4.0")
gi.require_version("Gtk4LayerShell", "1.0")

from gi.repository import GLib, Gtk, Pango,  Gtk4LayerShell as GtkLayerShell  # noqa: E402


TIME_PATH = os.path.expanduser("~/.time")


def apply_styles(widget: Gtk.Box | Gtk.Widget, css: str):
    provider = Gtk.CssProvider()
    provider.load_from_data(css.encode())
    context = widget.get_style_context()
    context.add_provider(provider, Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)


def read_time() -> str:
    return open(TIME_PATH).read().strip()


async def is_running(process_name: str) -> bool:
    try:
        if not os.name == "nt":
            output = subprocess.check_output(["pgrep", process_name])
            return output.lower() != b""
        else:
            raise NotLinuxException()
    except subprocess.CalledProcessError:
        return False


async def get_agenda() -> str:
    """Gets the agenda for today, then closes the agenda buffer"""

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


def get_default_styling() -> str: 
    return "  margin: %spx; margin-top: %spx; padding: %spx; border: %spx solid; border-radius: %spx; "  % (
                style.WIDGET_MARGINS[0],
                style.WIDGET_MARGINS[0],
                style.PADDING,
                style.BORDER,
                style.BORDER_ROUNDING,
            ),

async def parse_agenda() -> list[str]:
    # FIXME: development fix
    # return "TODO: some tasks and stuf\n 1:20 - 2:20 more stuff".splitlines()
    if not await is_running("emacs"):
        raise Exception("AgendaOffline")

    try:
        agenda = await get_agenda()
    except EmacsUnavailableException as e:
        print(e)
        sys.exit(-1)

    agenda = agenda.splitlines()
    todos = list(
        map(
            lambda x: x[x.find(":") + 1 :].strip(),
            filter(lambda x: "todo" in x, agenda),
        )
    )
    return todos


def VBox(spacing: int = 6) -> Gtk.Box:
    return Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=spacing)


def HBox(spacing: int = 6) -> Gtk.Box:
    return Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=spacing)


def timeBox() -> Gtk.Box:
    timeBox = Gtk.Box()
    timeButton = Gtk.Button(label=read_time())

    def on_time_button_clicked(_w: Gtk.Button) -> None:
        new_label = timeButton.get_label()
        if new_label is not None:
            with open(TIME_PATH, "w") as f:
                f.write(new_label)
        else:
            raise NoValueFoundException("timeButton does not hold a label value")

        timeButton.set_label(str(int(new_label) + 1))

    timeButton.connect("clicked", on_time_button_clicked)
    timeBox.append(timeButton)

    return timeBox


def weatherBox() -> Gtk.Box:
    weather = Gtk.Box()
    weather_box = VBox(10)
    apply_styles(weather, "box {%s}" % get_default_styling())
    weather.append(weather_box)
    weather_box.append(Gtk.Label(label="Weather"))
    # FIXME: this is a temp fix, change on deploy
    weather_label = Gtk.Label(label=str(asyncio.run(get_weather())) + "°F")
    # weather_label = Gtk.Label(label="70°F")
    weather_box.append(weather_label)
    apply_styles(weather_label, "label { font-size: 120px; }")
    return weather

def Timer() -> Gtk.Box:
    timer_box = VBox(10)

    # replace with svg 
    # ./alarm.svg
    timer_image = Gtk.Label(label="⏰")
    ibox = HBox()
    ibox.append(timer_image)
    ibox.append(Gtk.Entry())
    timer_box.append(ibox)
    timer_button = Gtk.Button(label="Start")
    ibox.append(timer_button)

    return timer_box

def Time() -> Gtk.Box: 
    """Returns time widget with time, and day"""
    timeBox = VBox(20)

    time_widget = Gtk.Label(label=dt.now().strftime("%I:%M %p"))
    day_widget = Gtk.Label(label=dt.now().strftime("%A"))
    # poll to refresh the time every minute
    GLib.timeout_add_seconds(60, lambda: time_widget.set_label(dt.now().strftime("%I:%M %p")))
    GLib.timeout_add_seconds(3600, lambda: day_widget.set_label(dt.now().strftime("%A")))
    apply_styles(timeBox, "box {%s}" % get_default_styling())

    timeBox.append(time_widget)
    timeBox.append(day_widget)

    return timeBox

def Calendar() -> Gtk.Box:
    todays_date = dt.now()
    calendar_str = calendar.month(todays_date.year, todays_date.month)

    calendar_box : Gtk.Box = Gtk.Box()
    calendar_label = Gtk.TextView()
    calendar_label.set_editable(False)
    calendar_label.set_cursor_visible(False)
    calendar_label.set_monospace(True)
    calendar_label.set_hexpand(True)
    apply_styles(calendar_label, "textview { font-size: %spx; }" % style.CALENDAR_FONT_SIZE) 
    apply_styles(calendar_box, "box {%s}" % get_default_styling())
    buffer = calendar_label.get_buffer()
    if buffer:
        buffer.set_text(calendar_str)

        # Create a text tag to bolden today's date
        tag: Gtk.TextTag = buffer.create_tag("bold", weight=Pango.Weight.ULTRABOLD)

        # Find and bold the specific part of the text (today's date)
        start_pos = buffer.get_iter_at_offset(calendar_str.find(str( todays_date.day )))
        end_pos = buffer.get_iter_at_offset(calendar_str.find(str( todays_date.day )) + len(str(todays_date.day))
) 
        # Apply the bold tag to today's date
        buffer.apply_tag(tag, start_pos, end_pos)
    else:
        raise Exception("Unable to create Calendar Buffer")
    calendar_box.append(calendar_label)

    return calendar_box
    


@final
class Dashboard(Gtk.ApplicationWindow):
    def __init__(self, **kwargs):
        super().__init__(
            **kwargs,
            title="dashboard",
            show_menubar=False,
            child=None,
            fullscreened=False,
            default_width=1200,
            default_height=500,
            destroy_with_parent=True,
            hide_on_close=False,
            resizable=False,
            visible=True,
        )

        self.main_box = HBox(10)
        # half the grid shows the weather
        # the other half shows the agenda
        self.set_child(self.main_box)

        self.weatherBox = VBox(20)
        self.weather = weatherBox()
        self.weatherBox.append(self.weather)
        self.main_box.append(self.weatherBox)


        self.calendarBox = VBox(20)
        self.calendarBox.append(Time())
        self.calendarBox.append(Calendar())
        self.calendarBox.append(Timer())
        self.main_box.append(self.calendarBox)
        
        self.agenda_box = VBox(20)
        agenda_box: Gtk.Box = VBox(20)
        self.agenda_box.append(agenda_box)
        self.main_box.append(self.agenda_box)
        apply_styles(
            agenda_box,
            "box {%s}" % get_default_styling()
        )

        self.agenda_title = Gtk.Box()
        apply_styles(self.agenda_title, "padding: 10px; background-color: #f0f0f0;")
        self.agenda_title_label = Gtk.Label(label="Agenda")
        apply_styles(self.agenda_title_label, "label { font-size: 30px; }")
        self.agenda_title.append(self.agenda_title_label)
        agenda_box.append(self.agenda_title)

        self.agenda = asyncio.run(parse_agenda())
        for i in range(len(self.agenda)):
            label = Gtk.Label(label=self.agenda[i])
            agenda_box.append(label)

        self.hourBox = timeBox()
        self.agenda_box.append(self.hourBox)

    def on_button_clicked(self, _w: Gtk.Button):
        print("dashboard_exit")
        self.close()


def on_activate(app: Gtk.Application):
    # set to layer shell
    win = Dashboard(application=app)

    GtkLayerShell.init_for_window(win)
    GtkLayerShell.set_layer(win, GtkLayerShell.Layer.TOP)
    GtkLayerShell.set_anchor(win, GtkLayerShell.Edge.BOTTOM, True)
    GtkLayerShell.set_margin(win, GtkLayerShell.Edge.BOTTOM, 20)
    GtkLayerShell.set_margin(win, GtkLayerShell.Edge.TOP, 20)
    GtkLayerShell.auto_exclusive_zone_enable(win)

    win.present()



app = Gtk.Application(application_id="com.example")
app.connect("activate", on_activate)

app.run(None)

sys.exit(0)
