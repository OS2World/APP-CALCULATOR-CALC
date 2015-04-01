if %1*==* goto doit
c:\FORTRAN\BIN\LINK %1 +strglib+calclib1+calclib2+calcsimp+c:\fortran\bin\bmasm,,,F:\FORTRAN\LIB\ c:\fortran\lib\graphics,,
goto done
:doit
H:\bin\LINK calc+strglib+calclib1+calclib2+calcsimp+calcsim2+calcdebg+h:\bin\bmasm,,,h:\lib\ h:\lib\graphics,,
rem LINK calc+strglib+calclib1+calclib2+calcsimp+calcdebg+bmasm,,,graphics,,
rem F:\FORTRAN\BIN\LINK calc+strglib+calclib1+calclib2+calcsimp+f:\fortran\bin\bmasm,,,F:\FORTRAN\LIB\ f:\fortran\lib\graphics,,
:done
