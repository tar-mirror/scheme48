@echo off

rem Part of Scheme 48 1.9.  See file COPYING for notices and license.
rem
rem Authors: Richard Kelsey, Mike Sperber, Marcus Crestani
rem

set runnable="%~1"

cd ps-compiler
echo ,batch > compile-vm.input
echo ,config ,load ../scheme/platform-interfaces.scm >> compile-vm.input
echo ,config ,load ../scheme/rts-packages-32.scm >> compile-vm.input
echo ,config ,load ../scheme/prescheme/interface.scm >> compile-vm.input
echo ,config ,load ../scheme/prescheme/package-defs.scm >> compile-vm.input
echo ,config ,load ../scheme/prescheme/interface.scm >> compile-vm.input
echo ,config ,load ../scheme/prescheme/package-defs.scm >> compile-vm.input
echo ,exec ,load load-ps-compiler.scm >> compile-vm.input
echo ,exec ,load compile-vm-no-gc-32.scm >> compile-vm.input
echo ,exit >> compile-vm.input

call %runnable% -h 12000000 < compile-vm.input
cd ..
move /Y scheme\vm\scheme48vm-32.c c
