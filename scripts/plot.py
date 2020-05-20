#!/usr/bin/env python3

import datetime
import os
import pathlib

from matplotlib.dates import DateFormatter, date2num
from matplotlib.ticker import FixedLocator
import matplotlib.pyplot as plt

os.chdir(pathlib.Path(__file__).parent)

points = []


def read(num):
    if num:
        return int(num)
    else:
        # thanks awk
        return 0


with open("report.log") as f:
    for line in f:
        (
            haskell_loc,
            kalyn_loc,
            haskell_loc_added,
            haskell_loc_removed,
            kalyn_loc_added,
            kalyn_loc_removed,
            haskell_files,
            kalyn_files,
            timestamp,
            sha,
        ) = line.split(",")
        points.append(
            {
                "haskell_loc": read(haskell_loc),
                "kalyn_loc": read(kalyn_loc),
                "haskell_loc_added": read(haskell_loc_added),
                "haskell_loc_removed": read(haskell_loc_removed),
                "kalyn_loc_added": read(kalyn_loc_added),
                "kalyn_loc_removed": read(kalyn_loc_removed),
                "haskell_files": read(haskell_files),
                "kalyn_files": read(kalyn_files),
                "timestamp": datetime.datetime.strptime(
                    timestamp, "%Y-%m-%d %H:%M:%S %z"
                ),
                "sha": sha.strip(),
            }
        )


def combine(k1, k2, k3):
    for point in points:
        point[k3] = point[k1] + point[k2]


def accumulate(key):
    total = 0
    for point in points:
        total += point[key]
        point[key + "_total"] = total


combine("haskell_loc_added", "kalyn_loc_added", "loc_added")
combine("haskell_loc_removed", "kalyn_loc_removed", "loc_removed")


accumulate("loc_added")
accumulate("loc_removed")


months = [datetime.date(year=2020, month=n, day=1) for n in range(3, 6)]
midmonths = [datetime.date(year=2020, month=n, day=16) for n in range(3, 6)]


def plot(series, title, png):
    # https://matplotlib.org/3.1.1/gallery/text_labels_and_annotations/date.html
    fig, ax = plt.subplots(figsize=(24, 18))
    t = [p["timestamp"] for p in points]
    for key, label, color in series:
        y = [p[key] for p in points]
        (line,) = ax.plot(t, y, label=label, linewidth=10)
        if color:
            line.set_color(color)
    ax.tick_params(labelsize=40)
    ax.xaxis.set_major_formatter(DateFormatter("%b %-d"))
    ax.xaxis.set_minor_locator(FixedLocator(list(map(date2num, midmonths))))
    plt.xticks(months)
    plt.rc("font", size=40)
    plt.legend()
    plt.title(title)
    plt.grid(True, "both")
    plt.savefig(png, transparent=True)


plot(
    [("haskell_loc", "Haskell", "orange"), ("kalyn_loc", "Kalyn", "purple")],
    "Total lines of code",
    "loc.png",
)
plot(
    [("haskell_files", "Haskell", "orange"), ("kalyn_files", "Kalyn", "purple")],
    "Number of modules",
    "files.png",
)
plot(
    [("loc_added_total", "Added", "green"), ("loc_removed_total", "Removed", "red")],
    "Added and removed lines of code",
    "codefreq.png",
)
