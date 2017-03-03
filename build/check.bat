@echo off

rem Part of Scheme 48 1.9.  See file COPYING for notices and license.
rem
rem Authors: Mike Sperber, Marcus Crestani
rem

set runnable="%~1"

echo ,batch > check.input
echo ,exec ,load =scheme48/debug/check.scm >> check.input
echo ,config ,load =scheme48/test-packages.scm >> check.input
echo ,open base-test >> check.input
echo ,open big-test >> check.input
echo ,open sockets-test >> check.input
echo ,open tconc-queue-test >> check.input
echo ,open transport-link-cell-test >> check.input
echo ,open tlc-table-test >> check.input
echo ,open env-test >> check.input
echo ,config ,load =scheme48/sort/test-packages.scm >> check.input
echo ,open sort-test >> check.input
echo ,config ,load =scheme48/misc/packages.scm >> check.input
echo ,config ,load =scheme48/misc/test-packages.scm >> check.input
echo ,open misc-test >> check.input
echo ,config ,load =scheme48/r6rs/test-packages.scm >> check.input
echo ,open r6rs-test >> check.input
echo ,config ,load =scheme48/cml/test-packages.scm >> check.input
echo ,open cml-test >> check.input
echo ,config ,load =scheme48/srfi/test-packages.scm >> check.input
echo ,open portable-srfi-test >> check.input
echo ,config ,load =scheme48/ffi-test/test-packages.scm	>> check.input
echo ,open ffi-test >> check.input
echo ,open test-suites >> check.input
echo (define-test-suite all-tests (compiler-tests base-tests big-tests misc-tests tcp-sockets-tests tconc-queue-tests transport-link-cell-tests tlc-table-tests tlc-table-weak-tests sort-tests env-tests r6rs-tests cml-tests portable-srfi-tests ffi-tests)) >> check.input
echo (run-test-suite all-tests) >> check.input

%runnable% -h 8000000 < check.input
