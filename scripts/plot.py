#!/usr/bin/env python3

import datetime
import os
import pathlib

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

print(points)
