#!/usr/bin/env zsh

set -e
set -o pipefail

function sum_col {
    awk "{SUM += \$$1} END {print SUM}"
}

function sgrep {
    grep "$@" || true
}

find . -name '*.hs' | xargs cat | wc -l | read haskell_loc
find . -name '*.kalyn' | xargs cat | wc -l | read kalyn_loc

stat="$(git show --numstat --format=)"

printf '%s\n' $stat | sgrep '\.hs$' | sum_col 1 | read haskell_loc_added
printf '%s\n' $stat | sgrep '\.hs$' | sum_col 2 | read haskell_loc_removed
printf '%s\n' $stat | sgrep '\.kalyn$' | sum_col 1 | read kalyn_loc_added
printf '%s\n' $stat | sgrep '\.kalyn$' | sum_col 2 | read kalyn_loc_removed

find . -name '*.hs' | wc -l | read haskell_files
find . -name '*.kalyn' | wc -l | read kalyn_files

git log -1 -s --format=%ci | read timestamp

git rev-parse HEAD | read sha

echo "${haskell_loc},${kalyn_loc},${haskell_loc_added},${haskell_loc_removed},${kalyn_loc_added},${kalyn_loc_removed},${haskell_files},${kalyn_files},${timestamp},${sha}"
