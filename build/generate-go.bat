@echo off

rem Part of Scheme 48 1.9.  See file COPYING for notices and license.
rem
rem Authors: Mike Sperber
rem

set GO=%1
set VM=%2
set IMAGE=%3
echo @%VM% -i %IMAGE% %%* > %GO%
