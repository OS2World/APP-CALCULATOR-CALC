rem Batch file to compile CALC files.
if %2==DEBUG goto foo
rem c:\fortran\bin\fl   /Os /FPi /c   %1.for
c:\fortran\bin\fl   /Os /FPi /c   %1.for
goto hoo
:foo
c:\fortran\bin\fl   /Od /4Yb /FPi /c   %1.for
rem c:\fortran\bin\fl   /Od  /4Yb /FPi /c   %1.for
:hoo
