#!/bin/sh
# Part of Scheme 48 1.9.  See file COPYING for notices and license.
#
# Authors: Mike Sperber
#

# Build file list for the Windows installer WiX script
# Mike doesn't know how to do this using only Windows batch files.
# Run this in the source directory, under Cygwin or something.

runnable=$1

(
echo ',batch';
echo ',open (subset srfi-1 (partition delete-duplicates filter concatenate filter-map))';
echo ',open (subset srfi-13 (string-tokenize))';
echo ',open (subset srfi-14 (char-set char-set-complement))';
echo ',load build/windows-installer.scm';
# generate enough uuids and quote them
echo '(define uuids (list ';
uuidgen -n100 | sed 's/^.*$/"&"/';
echo '))';
echo '(define files (list';
for f in scheme/*.scm \
      scheme/env/*.scm \
      scheme/big/*.scm \
      scheme/misc/*.scm \
      scheme/net/*.scm \
      scheme/r6rs/*.scm \
      scheme/cml/*.scm \
      scheme/srfi/*.scm \
      scheme/rts/*.scm \
      scheme/sort/*.scm \
      scheme/posix/*.scm \
      doc/*.ps doc/*.pdf doc/*.txt doc/html/*; do
  echo "\"$f\"";
done;
echo '))';
echo "(write-file-elements-include-file files uuids \"build/scheme48-files.wxi\")"
) > scheme48-files.input

# For some reason, directly piping into $runnable doesn't work under Mike's Cygwin
# installation---the second 4k block gets read a second time.

$runnable < scheme48-files.input
