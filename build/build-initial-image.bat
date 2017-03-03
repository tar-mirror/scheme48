@echo off

set runnable="%~1"

call build\filenames.bat

echo ,batch > build\build-initial-image.input
echo ,bench on >> build\build-initial-image.input
echo ,open signals handle features code-quote >> build\build-initial-image.input
echo ,open bitwise ascii code-vectors >> build\build-initial-image.input
echo ,open (subset i/o (write-byte)) (subset ports (set-port-crlf?!)) >> build\build-initial-image.input
echo ,config ,load scheme/platform-interfaces.scm >> build\build-initial-image.input
echo ,config ,load scheme/rts-packages-32.scm >> build\build-initial-image.input
echo ,open platform  >> build\build-initial-image.input
echo ,open cells record-types >> build\build-initial-image.input
echo ,load scheme/alt/low-exception.scm >> build\build-initial-image.input
echo ,load %linker-files% >> build\build-initial-image.input
echo ,load scheme/alt/init-defpackage.scm >> build\build-initial-image.input
echo (load-configuration "scheme/platform-interfaces.scm") >> build\build-initial-image.input
echo (load-configuration "scheme/rts-packages-32.scm") >> build\build-initial-image.input
echo (load-configuration "scheme/interfaces.scm") >> build\build-initial-image.input
echo (load-configuration "scheme/vm/shared-interfaces.scm") >> build\build-initial-image.input
echo (load-configuration "scheme/packages.scm") >> build\build-initial-image.input
echo (flatload initial-structures) >> build\build-initial-image.input
echo (load "build/initial.scm") >> build\build-initial-image.input
echo (link-initial-system) >> build\build-initial-image.input

call %runnable% -h 8000000 < build\build-initial-image.input
cd build
if exist initial.image-32 del initial.image-32
ren initial.image initial.image-32
cd ..

