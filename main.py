#!/usr/bin/env python3
# pyright: reportMissingTypeStubs=false
# pyright: reportUnknownMemberType=false
# pyright: reportUntypedBaseClass=false
# pyright: reportAttributeAccessIssue=false
# pyright: reportUnusedCallResult=false
# pyright: reportUnknownVariableType=false
# pyright: basic
# ruff: ignore

# TODO:refresh button for agenda
# TODO: Use icons instead of words
# TODO: Multimonitor support

import sys
import os

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

import cairo

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


def Habits() -> Gtk.Box:
    habit_widget = VBox()
    habits_box = VBox(10)
    habit_widget.append(habits_box)

    # Paths and setup
    # Today's date
    hm = HabitManager()

    # UI for each habit
    def generate_habits() -> list[Gtk.Box]:
        habit_widgets = []
        habits = hm.get_habits()
        today = dt.today()
        for habit in habits:
            habit_box = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=10)

            # Habit name
            habit_label = Gtk.Label(label=habit["name"])
            habit_label.set_xalign(0)

            # Streak information
            streak_label = Gtk.Label(
                label=f"{habit['current_streak']} | {habit['best_streak']}"
            )
            streak_label.set_xalign(1)

            # Check today's habit completion
            done_today = False
            if habit["latest_date"] == today.isoformat():
                done_today = True

            def on_check_toggled(button: Gtk.Button, habit):
                nonlocal done_today
                if button.get_active() and not done_today:
                    # Mark habit as done today
                    done_today = True
                    habit["current_streak"] += 1
                    habit["best_streak"] = max(
                        habit["best_streak"], habit["current_streak"]
                    )
                    habit["latest_date"] = today.isoformat()
                    habit["week"].append(0)
                    habit["week"] = habit["week"][-7:]  # Keep only last 7 days

                    # Update streak label
                    streak_label.set_text(
                        f" {habit['current_streak']} | {habit['best_streak']}"
                    )

                    # Save updated habit to file
                    hm.update_habits(habits)

            check_button = Gtk.CheckButton(label="")
            check_button.set_active(done_today)
            check_button.connect("toggled", on_check_toggled, habit)
            remove_habit_button = Gtk.Button(label="-")
            remove_habit_button.connect(
                "clicked", lambda _: remove_habit(habit["name"], habits)
            )

            habit_box.append(habit_label)
            habit_box.append(streak_label)
            habit_box.append(check_button)
            habit_box.append(remove_habit_button)

            # Append habit box to the main habits box
            habit_widgets.append(habit_box)
        return habit_widgets

    def remove_habit(habit: str, habits: list[dict[str, str]]) -> bool:
        nonlocal habits_box
        habits = hm.get_habits()
        new_habits = ""
        for h in habits:
            if h["name"] != habit:
                week_str = ",".join(map(str, h["week"]))
                new_habits += f"{h['name']} {h['best_streak']} {h['current_streak']} {h['latest_date']} {week_str}\n"
        hm.update_habits(new_habits)
        f_child = habits_box.get_first_child()
        while f_child is not None:
            if isinstance(f_child, Gtk.Box):
                label = f_child.get_first_child()
                while label:
                    if isinstance(label, Gtk.Label) and label.get_text() == habit:
                        habits_box.remove(f_child)
                        return True
                    label = label.get_next_sibling()
            f_child = f_child.get_next_sibling()
        return True

    def add_habit(habit: str):
        if not habit:
            return
        habits = hm.get_habits()
        for h in habits:
            if h["name"] == habit:
                return

        today = dt.today()
        nonlocal habits_box, habit_widget

        hm.add_habit(f"{habit} 0 0 {today.isoformat()} 0,0,0,0,0,0,0\n")
        new_habit = HBox(10)
        new_habit_label = Gtk.Label(label=habit)
        new_habit_streak = Gtk.Label(label="0 | 0")
        new_habit_check = Gtk.CheckButton(label="")
        new_habit_remove = Gtk.Button(label="-")
        new_habit_remove.connect(
            "clicked", lambda _: remove_habit(habit, hm.get_habits())
        )
        new_habit.append(new_habit_label)
        new_habit.append(new_habit_streak)
        new_habit.append(new_habit_check)
        new_habit.append(new_habit_remove)
        habits_box.append(new_habit)

    def make_habit_widgets() -> None:
        nonlocal habits_box, habit_widget
        habit_widget.remove(habits_box)
        habits_box = VBox()
        habit_widget.append(habits_box)
        habits = generate_habits()
        for h in habits:
            habits_box.append(h)

    make_habit_widgets()

    new_habit_box = HBox(10)
    new_habit_entry = Gtk.Entry()
    new_habit_entry.set_placeholder_text("")
    new_habit_button = Gtk.Button(label="+")
    new_habit_button.connect("clicked", lambda _: add_habit(new_habit_entry.get_text()))
    new_habit_button.connect("clicked", lambda _: new_habit_entry.set_text(""))
    new_habit_box.append(new_habit_entry)
    new_habit_box.append(new_habit_button)

    # region = cairo.Region(cairo.RectangleInt(50, 50, 100, 100))
    # new_habit_box.input_shape_combine_region(region)

    # Add widgets to the habit box
    habit_widget.append(new_habit_box)

    return habit_widget


def Time() -> Gtk.Box:
    """Returns time widget with time, and day"""
    timeBox = VBox(20)

    time_widget = Gtk.Label(label=dt.now().strftime("%I:%M %p"))

    def update_time():
        return dt.now().strftime("%I:%M %p")

    GLib.timeout_add_seconds(60, (lambda: time_widget.set_label(update_time()) or True))

    apply_styles(time_widget, "label {font-size: 60px;}")

    timeBox.append(time_widget)

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
    agenda_ibox: Gtk.Box = VBox(20)

    agenda = asyncio.run(parse_agenda())
    for i in range(len(agenda)):
        label = Gtk.Label(label=agenda[i])
        agenda_ibox.append(label)

    agenda_box.append(agenda_ibox)

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


def get_all_monitors(monitor, window): 
    geometry = monitor.get_geometry()
    scale_factor =  monitor.get_scale_factor()

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

# Create a dashboard for each monitor
monitors = list(display.get_monitors() )
print(monitors)

for i in range(len(monitors)):
    print(monitors[i])

    # create_dashboard_for_monitor(monitor, display)

app.run(None)

app.connect("shutdown", lambda *_: sys.exit(0))
