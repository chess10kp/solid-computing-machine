#!/usr/bin/env python3
# pyright: reportMissingTypeStubs=false
# pyright: reportUnknownMemberType=false
# pyright: reportUntypedBaseClass=false
# pyright: reportAttributeAccessIssue=false
# pyright: reportUnusedCallResult=false
# pyright: reportUnknownVariableType=false
# pyright: basic
# ruff: ignore

import sys
import os

from itertools import starmap

import subprocess
from typing_extensions import Iterable, final
from exceptions import (
    EmacsUnavailableException,
    NotLinuxException,
    NoValueFoundException,
)
from habits import HabitManager

import style
from datetime import datetime as dt
import calendar

import asyncio

try:
    import gi
    from weather import get_weather
except ModuleNotFoundError:
    script_dir = os.path.dirname(os.path.abspath(__file__))
    venv_python = os.path.join(script_dir, "venv", "bin", "python")

    if not os.path.exists(venv_python):
        sys.stderr.write("Virtual environment not found in 'venv' directory.\n")
        sys.exit(1)

    os.execv(venv_python, [venv_python] + sys.argv)


# For GTK4 Layer Shell to get linked before libwayland-client we must explicitly load it before importing with gi
from ctypes import CDLL

CDLL("libgtk4-layer-shell.so")


gi.require_version("Gtk", "4.0")
gi.require_version("Gtk4LayerShell", "1.0")

from gi.repository import GLib, Gdk, Gtk, Pango, Gtk4LayerShell as GtkLayerShell  # noqa: E402


TIME_PATH = os.path.expanduser("~/.time")


def apply_styles(widget: Gtk.Box | Gtk.Widget | Gtk.Label , css: str):
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
    return str(
        "  margin: %spx; margin-top: %spx; padding: %spx; border: %spx solid; border-radius: %spx; "
        % (
            style.WIDGET_MARGINS[0],
            style.WIDGET_MARGINS[0],
            style.PADDING,
            style.BORDER,
            style.BORDER_ROUNDING,
        ),
    )


async def parse_agenda() -> list[str]:
    # FIXME: development fix
    # return "TODO: some tasks and stuf\n 1:20 - 2:20 more stuff".splitlines()
    try:
        agenda = await get_agenda()
    except EmacsUnavailableException as e:
        print(e)
        sys.exit(-1)

    agenda = agenda.splitlines()
    todos = list(
        map(
            lambda x: x[x.find(":") + 1 :].strip(),
            filter(lambda x: "todo" in x and "closed" not in x.lower(), agenda),
        )
    )
    return todos


def VBox(spacing: int = 6, hexpand: bool = False, vexpand: bool = False) -> Gtk.Box:
    return Gtk.Box(
        orientation=Gtk.Orientation.VERTICAL,
        spacing=spacing,
        hexpand=hexpand,
        vexpand=vexpand,
    )


def HBox(spacing: int = 6, hexpand: bool = False, vexpand: bool = False) -> Gtk.Box:
    return Gtk.Box(
        orientation=Gtk.Orientation.HORIZONTAL,
        spacing=spacing,
        hexpand=hexpand,
        vexpand=vexpand,
    )


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


def weather_widget() -> Gtk.Box:
    weather = Gtk.Box()
    weather_box = VBox(10)
    apply_styles(weather, "box {%s}" % get_default_styling())
    weather.append(weather_box)
    weather_label = Gtk.Label(label=str(asyncio.run(get_weather())) + "°F")
    weather_box.append(weather_label)
    apply_styles(weather_label, "label { font-size: 120px; }")
    return weather


def Time() -> Gtk.Box:
    """Returns time widget with time, and day"""
    timeBox = VBox(20)

    time_widget = Gtk.Label(label=dt.now().strftime("%I:%M %p"))

    passingOfTimeBox = HBox(20)

    def read_day_date():
        return dt.now().strftime("%A, %B %d")
    
    dayDataButton = Gtk.Label(label=read_day_date())

    def days_passed_this_year(): 
        today = dt.now()
        year_start = dt(today.year, 1, 1)
        return (today - year_start).days + 1

    numDaysInThisYearPassedButton = Gtk.Label(label=str(days_passed_this_year() ) + " d")
    numHoursPassedThisYearButton = Gtk.Label(label=str(days_passed_this_year() * 24) + " hr")
    numMinutesPassedThisYearButton = Gtk.Label(label=str(days_passed_this_year() * 24 * 60) + " min")

    def update_time():
        return dt.now().strftime("%I:%M %p")

    GLib.timeout_add_seconds(60, (lambda: time_widget.set_label(update_time()) or True))
    GLib.timeout_add_seconds(600, (lambda: dayDataButton.set_label(read_day_date()) or True))

    GLib.timeout_add_seconds(7200, (lambda: numDaysInThisYearPassedButton.set_label(str(days_passed_this_year())) or True))
    GLib.timeout_add_seconds(600, (lambda: numHoursPassedThisYearButton.set_label(str(days_passed_this_year() * 24)) or True))
    GLib.timeout_add_seconds(300, (lambda: numMinutesPassedThisYearButton.set_label(str(days_passed_this_year() * 24 * 60)) or True))

    apply_styles(time_widget, "label {font-size: 60px;}")
    apply_styles(dayDataButton, "button {font-size: 30px;}")
    apply_styles(passingOfTimeBox, "box {%s}" % get_default_styling())

    apply_styles(numDaysInThisYearPassedButton, "button {font-size: 30px;}")
    apply_styles(numHoursPassedThisYearButton, "button {font-size: 30px;}")
    apply_styles(numMinutesPassedThisYearButton, "button {font-size: 30px;}")

    passingOfTimeBox.append(numDaysInThisYearPassedButton)
    passingOfTimeBox.append(numHoursPassedThisYearButton)
    passingOfTimeBox.append(numMinutesPassedThisYearButton)

    timeBox.append(time_widget)
    timeBox.append(dayDataButton)
    timeBox.append(passingOfTimeBox)

    return timeBox


def Calendar() -> Gtk.Box:
    calendar_box: Gtk.Box = VBox(vexpand=True)
    todays_date = dt.now()
    calendar_str = calendar.month(todays_date.year, todays_date.month).splitlines()
    calendar_str = list(map(str.split, calendar_str))

    if len(calendar_str[2]) < 7:
        for i in range(7 - len(calendar_str[2])):
            calendar_str[2].insert(0 , " ")

    calendar_label = Gtk.Grid(
        hexpand=False, vexpand=False
    )

  # Create a Grid to hold the calendar
    calendar_label.set_column_spacing(5)
    calendar_label.set_row_spacing(5)
    calendar_label.set_halign(Gtk.Align.CENTER)
    calendar_label.set_valign(Gtk.Align.CENTER)
    calendar_label.set_hexpand(False)
    calendar_label.set_vexpand(False)

    # Add the days of the month
    for row, week in enumerate(calendar_str[1:]):
        for col, day in enumerate(week):
            label = Gtk.Label(label=str(day))
            label.set_xalign(0.5)
            label.set_yalign(0.5)
            apply_styles(label, "label { font-size: 20px; }")

            if day == str(todays_date.day):
                apply_styles(
                    label,
                    "label { font-weight: bold; background-color: #FFD700; color: black; padding: 2px; border-radius: 4px; }",
                )

            calendar_label.attach(label, col, row, 1,1)


    calendar_box.append(calendar_label)

    apply_styles(calendar_box, "box {%s}" % get_default_styling())

    return calendar_box


def Agenda() -> Gtk.Box:
    """Returns Agenda Widget"""
    agenda_box = VBox(20)
    agenda_ibox = VBox(20)

    def update(): 
        agenda = asyncio.run(parse_agenda())

        child = agenda_ibox.get_first_child()

        lines = []
        while child : 
            lines.append(child.get_label())


        while child : 
            prev = child
            child = child.get_next_sibling()
            agenda_ibox.remove(prev)
            
        for i in range(len(agenda)):
            label = Gtk.Label(label=agenda[i])
            GLib.idle_add(agenda_ibox.append, label)

    update()
    agenda_box.append(agenda_ibox)

    buttons = HBox() 
    agenda_box.append(buttons)

    GLib.timeout_add_seconds(120, (lambda: agenda_ibox.set_label(update()) or True))

    apply_styles(agenda_ibox, "box {%s}" % get_default_styling())

    return agenda_box


@final
class Dashboard(Gtk.ApplicationWindow):
    def __init__(self, **kwargs):
        super().__init__(
            **kwargs,
            title="dashboard",
            show_menubar=False,
            child=None,
            fullscreened=False,
            default_height=500,
            destroy_with_parent=True,
            hide_on_close=False,
            resizable=False,
            visible=True,
        )

        self.main_box = Gtk.Box(
            orientation=Gtk.Orientation.HORIZONTAL, spacing=20, homogeneous=False
        )

        self.set_child(self.main_box)

        self.weatherBox = VBox(20)
        self.weather = weather_widget()
        self.weatherBox.append(Time())
        self.weatherBox.append(self.weather)
        self.weatherBox.append(Calendar())
        self.main_box.append(self.weatherBox)

        self.calendarDiv = VBox(20, hexpand=True, vexpand=True)
        self.calendarBox = HBox(20, hexpand=True, vexpand=False)
        self.calendarBox.set_halign(Gtk.Align.START)
        self.calendarDiv.append(self.calendarBox)
        self.main_box.append(self.calendarDiv)

        self.agenda_box = VBox(20)
        self.agenda_box.append(Agenda())
        self.main_box.append(self.agenda_box)

        apply_styles(self.main_box, "box { bg: #282828; }")


def on_activate(app: Gtk.Application):
    # set to layer shell
    win = Dashboard(application=app)

    GtkLayerShell.init_for_window(win)
    GtkLayerShell.set_layer(win, GtkLayerShell.Layer.BOTTOM)
    GtkLayerShell.set_margin(win, GtkLayerShell.Edge.BOTTOM, 20)
    GtkLayerShell.set_margin(win, GtkLayerShell.Edge.TOP, 20)

    win.present()


app = Gtk.Application(application_id="com.example")
app.connect("activate", on_activate)

display = Gdk.Display.get_default()
if not display:
    sys.exit()

# get all the monitors, then create a window on each monitor
monitors = display.get_monitors()

app.run(None)

app.connect("shutdown", lambda *_: sys.exit(0))
