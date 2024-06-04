#! /usr/bin/env bash

set -e

for koan in $(ls koans/*/*/*/Koan.hs)
do
    mkdir -p diffs/$(dirname $koan)
    diff --unchanged-line-format="" --old-line-format="< %L" --new-line-format="> %L" $koan $(dirname $koan)/solution/$(basename $koan) > diffs/$(dirname $koan)/diff.txt || true
done

# Check that we didn't forget to check in diffs
test -z "$(git ls-files --others --exclude-standard -- diffs)"

# Check that the diffs haven't changed
git diff --exit-code
