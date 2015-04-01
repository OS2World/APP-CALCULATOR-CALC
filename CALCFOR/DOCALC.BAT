rem use this to compile and link CALC.  Note that I is a directory containing
rem the FORTRAN compiler, linker AND necessary libraries (we use a ram disk)
rem compcalc is a batch file that invokes the compiler.
rem calclink will invoke the linker on these
rem BMASM is a Microsoft supplied object file that supports the getchasm
rem function (poll keyboard, return results w/o waiting for ENTER key)
if %1==LINK goto linkit
call compcalc calc
call compcalc strglib
call compcalc calclib1
call compcalc calclib2
call compcalc calcsimp
call compcalc calcdebg
call compcalc calcsim2
subst e: c:\fortran
:linkit
e:\bin\LINK calc+strglib+calclib1+calclib2+calcsimp+calcsim2+calcdebg+e:\bin\bmasm,,,e:\lib\ e:\lib\graphics,,
