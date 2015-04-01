rem use this to compile and link CALCM.  Note that I is a directory containing
rem the FORTRAN compiler, linker AND necessary libraries (we use a ram disk)
rem compcalc is a batch file that invokes the compiler.
rem BMASM is a Microsoft supplied object file that supports the  getchasm
rem function (poll keyboard, return results w/o waiting for ENTER key)
call compcalc calcm
call compcalc calcm2
I:LINK calcM+CALCM2+STRGLIB+I:bmasm,,,I: I:graphics,,
