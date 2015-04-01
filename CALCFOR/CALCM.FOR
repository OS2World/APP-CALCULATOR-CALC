c CALCULATOR PROGRAM -- Regular Version
c @c for the public domain. D. Hellerstein, ERS/RTD, 1/92, 202-19-0444
c              NOTE TO PROGRAMMERS
C
C  This library contains modules for analyzing equations entered
c interactively by a user; as would be done when createing a calculator.
c   An  actual "stand alone" calculator (CALC.FOR) consists of
c     a few routines that determine what kind of action
c      is desired,  and some  calls to appropriate drivers.
c
c       Following is a list of the most important drivers and some very
c               useful utilities:
c
c       1)      SUBROUTINE calc_EQUATION(ANSWER,PSTRING,NERR)
c               calc_Equation will take a  string (contained in
c               PSTRING), analyze it and return the value in ANSWER.
c               If any errors are encountered in the analysis,
c               NERR will equal 1, otherwise NERR = 0.
c               ANSWER  should be REAL *8
c
c        4) SUBROUTINE calc_VARIABLE(ANSWER,VNAME,PSTRING,NERR,QSAVE)
c           If QSAVE is false, then calc_Variable will analyze an
c           equation (in PSTRING, see description in calc_Equation).
c           The value will be saved into the variable location
c           (of the variable named VNAME (Character *12)),
c           and the value will be returned in ANSWER. Nerr is as above.
c          IF qsave is  TRUE, calc_variable will take the value in ANSWER
c               and save into variable VNAME (pstring is NOT analyzed,
c               ANSWER is NOT changed).
c
c        5) Subroutine calc_LOADFunction(FNAME,ANAME,PSTRING,nerr)
C
c               calc_LOADFunction will take the string (in PSTRING) and
c                  save it under the name FNAME.  The variable ANAME
c                  will be treated as a function argument variable, as
c                  opposed to an external variable (set by calc_variable).
c                  NERR is as above.
c
c        6) Subroutine calc_function(Fname,ArgValue,Answer,Nerr)
c               Evaluate user function Fname, with argument of ArgValue,
c                  returns value in Real*8 answer; nerr > 0 if any fatal error.
c
c       CALC_SHOWVARIS(IOUT,NDEC,QEXP,VARNAME)  --
C                       displays names and values of all current variables.
C                       (IF VARNAME= ' ')
c       CALC_DEFINEVars(nvars)  --
c                    Display, and allow user to change, value of all
c                       current variables. Nvars is returned, is the
c                       number of currently active user variables
c                       (does not include PI and E)
c       CALC_SHOWFUNKS(IOUT,NDEC,QEXP,AFUNK)  -- Displays the Name,Argument,
c           and equation of all AFUNK, or all current functions if AFUNK=' '
c           (IOUT IS IO CHANNEL #
c       CALC_ERROR -- The error handler.  You might want to change this
c                    if you do not want display of error messages.
C       CALC_WRITENUM(VAL,CharVAL,NDEC,Qe,Nlen)   --
c                       Converts VAL (Real *8) into a string  (CHARVAL).
c                       Ndec and Qe control format of number (in string)
c                       Ndec=# of digits to right of decimal,
c                       If Ndec=0 (and qe=false), use integer format.
c                       QE=true if use exponential notation.
c                       Nlen is returned, it is # of significant charcters, if
c                         Nlen is <=0, error.
c
c  Note on character variables::
c       Character variables will be capitalized by Calc_cleanup_strung
c       before being paresed, etc.
c
c A note on user defined functions and variables.
c
c   Variables, once defined, can be entered into future equations
c     (during the same run of the program).  The current values
c     will be used in place of the variable.  Note that if a variable
c     that has not been defined (by calc_variable) is referenced in
c     an equation, an error will result.
c
c   User functions, once defined, can be invoked much like any
c    other functions (such as sin). This include invocations of
c    self in argument list:
c       eg (user function UF): UF(UF(5*W)+UF(3.2)) is a legal equation.
c                 (NOTE:: the above is NOT a legal function DEFINITION).
c
c    When one defines user functions; one can reference other, previously
c       defined user functions. Eg: UF1(I)=UF(I)*2-UF(I-1).
c
c    In addition, one can reference undefined variables when functions
c     are being defined.  The undefined variable will be automatically
c     created and assigned the value of 0.0.  Note that in normal
c     equation analysis, referencing undefined variables is an error.
c
C       NOTE:: At definition time, be sure that no INFINITE loops are
c         being constructed.  EG ; if
c            F(I)=H(I)-2     and
c            H(I)=I*F(I)        an infinite loop will result.
c        If this should occur, and you ask to analyze an equation containing
c           either of these 2 functions, the error message:
c       % OUT OF MEMORY (PERHAPS A SELF REFERENCING USER FUNCTION)
c          will appear.
c
c-----------------------------------------------------
c  action type modules
c=========================================================
c analyze an equation and return value in answer
C  This ROUTINE will recieve a string containing numeric characters,
c  operators, parentheses, and function names and will return a value.
c
c Numeric characters are the digits 0 .. 9, ".", "+", "-" and  "E" (eg 1.0E01)
c
c   Operators understood are  +  -  *  /  and ^ (or **) (exponentiation).
c       Operator precedence is as listed (+ is lowest, ^ is highest).
c
c   Parentheses understood are ([{ and )]}.  There is no distinction
c   between types of parentheses (they are used interchangeably).
c
c  21 Intrinsic Functions (eg.SIN, COS, and EXP) are understood.
c      Up to 35 user defined functions (with names of up to 12 characters)
c      can be specified.
c
c
C
c---------------------------------------------------------------------


        include 'fgraph.fi'

C====================================================================
c main program:: STAND ALONE CALCULATOR

        program calc_prog
        parameter (len_history=20)
        integer exit,setvari,setfunk,showvari,showfunk,help,calc,FMTSET
        INTEGER OUTSET,Idefine,trigradians,trigdegrees,doderiv,ninset
        integer simplify,remark
        character *160 get_commandline,ascird_2a,ascird_2b
        CHARACTER *160 Pstrung,strung,get_from_file

        character *4 a4B

        LOGICAL QE

        logical qout

        character *24 charans
        character *1 cvtucase,alevel
        character *12 Argument          !used if setfunk
        character *12 Name              !used if setfunk or setvari
        character *12 smp,simpname          !for simplification
        Real *8 answer
        character *12 afun,dafun,dafunarg

        logical qsave
        INTEGER TODO

        character *1 a2,next_char,apost

        character *80 A_history(len_History)
        common /cm_a_history/nat,ngot,a_history

        character *60 outname,openwrite,openread,inname
        common /cm_outname/outname,inname

c several of these actions are not supported.
        common /actions/
     1 exit,setvari,setfunk,showvari,showfunk,help,calc,FMTSET,OUTSET,
     1 INIT,Idefine,trigradians,trigdegrees,doderiv,ninset,SUMUP,
     1  remark,simplify

        data exit,setvari,setfunk,showvari,showfunk,help,calc,FMTSET,
     1                  OUTSET,INIT,Idefine,trigradians,trigdegrees,
     1                  doderiv,ninset,SUMUP,remark,simplify
     1          /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18/

        data iout/21/
        DATA NDEC/6/
        DATA QE/.FALSE./
        data infunk_string/40/

        data outname/' '/               !io defaults
        data in_unit/5/
        data qout/.false./

        data nat,ngot/1,0/

        a4b(1:1)=char(27)
           a4b(2:2)='['
           A4B(3:3)='7'
           a4b(4:4)='m'
        apost=char(39)

C BEGIN calc PROGRAM

        CALL calc_INIt          !initialize calculator

c Kommand= 0-user input, 1-from command line, 2-Esc, 3=help
 4      strung=GET_COMMANDLINE(' F1 for Help ',kommand)
        if (kommand.eq.2) stop ' bye.'
        if (kommand.eq.3) strung='HELP'    ! a hack, but it works
        Call PROCESS_strung(strung,in_unit)
        if (strung.eq.'?') strung='HELP'   ! see above
        if (strung.eq.' ') goto 200       ! nil string
        call push_ahistory(strung)       !save command

 10     continue


c strung is the user entry strung.
c     TODO is one of the /ACTIONS/ above.
c     Pstrung is the strung that should be processed by appropriate
c                    calc_ module
c     Name is either  user function name or variable name
c     Argument is user function argument
c     Ncode is VSAVE if /S option in variable definition.

        Call ACTION_TYPE(strung,                !sent
     1          Todo,Pstrung,Name,Argument,Qsave)  !recieved


c Case ToDO ::  exit,setvari,setfunk,showvari,showfunk,help,calc,ETC.

        if (todo.eq.exit) then
           stop ' '
        ELSE IF (TODO.EQ.INIT) THEN
            print *,' ReInitializing variables and functions !!'
            CALL DO_calc_INIT
        else if (todo.eq.IDEFINE) then
            call calc_definevars(nvar)
            if (nvar.le.0) print *,' No user variables.'
        else if (todo.eq.trigradians) then
            call calc_radians(.true.)
            print *,' Default trig arguments :: RADIANS '
        else if (todo.eq.trigdegrees) then
            call calc_radians(.false.)
            print *,' Default trig arguments :: DEGREES '
        else if (todo.eq.help) then
            call Calc_Help
        ELSE IF (TODO.EQ.FMTSET) THEN
           if (cvtucase(ascird_2a(
     1     ' Enter E to use exponential notation (eg 0.12E-02)?',kk))
     2                                             .EQ.'E') then
              qe=.true.
           else
              qe=.false.
           end if

           ndec=valimit(
     1              ' Enter # of digits to right of decimal point:',
     1              0.,5.,15.)

            infunk_string=valimit(
     1       ' Enter # of characters to use when displaying function:',
     1         1.,float(infunk_string),1000.)

       elseIF (TODO.EQ.SHOWVARI) THEN
           CALL CALC_SHOWVARIS(5,ndec,qe,pstrung)
           IF (QOUT)call CALC_shoWVARIS(IOUT,ndec,qe,pstrung)
           GOTO 200

        else if (todo.eq.showfunk) then
           call CALC_showfunks(6,ndec,qe,pstrung,infunk_string)
           IF (QOUT)call CALC_showfunks(IOUT,ndec,qe,pstrung,1000)


        ELSE IF (TODO.EQ.ninset) THEN
          if (pstrung.eq.' ') then
            if (in_unit.ne.5) then
                print *,' User Input of equations.'
                in_unit=5
            else
                in_unit=25
                close(unit=25)
                inname=openread(' Enter name for input file ?',25,nerr)
                if (nerr.ne.0) then
                   in_unit=5
                   print *,' Results will NOT be read from input file'
                endif
            endif
          else                          !got an input name
              CLOSE(UNIT=25)
              inname=pstrung
              OPEN(UNIT=25,FILE=inname,
     1              ACCESS='SEQUENTIAL',err=9002,
     1              status='OLD',mode='READ')
              in_unit=25
              print *,' Input from:',inname
          endif

          goto 200

 9002     print *,' Can not open input file:',inname
          in_unit=5


        ELSE IF (TODO.EQ.OUTSET) THEN
          if (pstrung.eq.' ') then
            IF (QOUT) THEN
               CLOSE(UNIT=21)
               QOUT=.FALSE.
               print *,' Closing output file :',outname
            else
                outname=openwrite(
     1            ' Enter name for output file ?',21,'.LST',nerr)
                if (nerr.eq.0) then
                   qout=.true.
                else
                   print *,' Results will NOT be saved to output file'
                endif
            end if
          else                          !got an output name
              IF (QOUT) THEN            ! close any open file
                 CLOSE(UNIT=21)
                 print *,' Closing output file: ',outname
              endif
              outname=pstrung
              OPEN(UNIT=21,FILE=outname,
     1              ACCESS='SEQUENTIAL',err=900,
     1              status='UNKNOWN')
              qout=.true.
              print *,' Saving results to: ',outname
          endif
          goto 200

 900      print *,' Can not open output file:',outname
          qout=.false.

        END IF             ! switches

c  else ,here to figure out equation OR to define a function OR to define a variable

c Case TODO == CALC,SETVARI, or SETFUNK (or SUMUP after call to SETUP_SUMUP)

        IF (ToDo.eq.Calc) then          !analyze an equation, type a value
          CALL calc_equation(Answer,Pstrung,nerr)
           if (nerr.gt.0) goto 200
           nn=messlen(Pstrung)
           Call calc_WRITENUM(ANSwer,CharAns,NDEC,QE,NLEN)
           if (nlen.eq.0) then
              nlen=24
              charans=' Cant print number.'
           end if
           write(6,77)Pstrung(1:nn),charans(1:nlen)
           IF (QOUT)write(IOUT,77)Pstrung(1:nn),charans(1:nlen)
 77        format(1x,a,' == ',a)


        else if (todo.eq.setfunk) then          !DO NOT ANALYZE
           Call calc_loadfunction(Name,ARGUMENT,Pstrung,Nerr)
           if (nerr.gt.0 )goto 200
           nn=messlen(strung)
           IF (QOUT)write(IOUT,99)strung(1:nn)
           write(6,99)strung(1:nn)
 99        format(' Function :: ',a)

        else if (todo.eq.setvari) THEN
           Call calc_variable(Answer,NAME,Pstrung,nerr,Qsave)   !Qsave=TRUE if /S option
           if (nerr.gt.0) goto 200
           nn=messlen(name)
           Call calc_WRITENUM(ANSwer,CharAns,NDEC,QE,NLEN)
           if (nlen.eq.0) then
              nlen=24
              charans=' Cant print number.'
           end if
           write(6,66)name(1:nn),charans(1:nlen)
           IF (QOUT)write(IOUT,66)name(1:nn),charans(1:nlen)
 66        format(1x,a,' :== ',a)


        end if


c here to get next command from user.

 200    CONTINUE                        !here to get new strung

c if from command line, exit to dos
        if (kommand.eq.1) then
             print *,
     1         ' Note: To compute several equations,',
     1                ' run CALC without an argument.'
              stop ' '
        endif

 2001   strung=' '
 2002   if (in_unit.eq.5) then
           pstrung=strung
           ISHIST=0
           IF (PSTRUNG.NE.' ') IShIST=1
           iarg=1
 2003      continue
           if (.not.qout) then
                strung=ascird_2b(' ',iarg,pstrung)
           else
                strung=ascird_2b(a4b//' ',iarg,pstrung)
           endif

           if (iarg.eq.18432) then          !up arrow
                call pop_ahistory(strung,-1)
                goto 2002
           elseif (iarg.eq.18688) then  !pgup
                call down_line(1)
                call pop_ahistory(strung,-1)
                goto 2002
           elseif (iarg.eq.20480) then    ! down arrow
                call pop_ahistory(strung,1)
                goto 2002
           elseif (iarg.eq.20736) then  !pgdn
                call down_line(1)
                call pop_ahistory(strung,1)
                goto 2002

           elseif (iarg.lt.255) then
               call push_ahistory(strung)
               call down_line(0)
           endif
        else
           strung=get_from_file(in_unit)
           if (in_unit.eq.5) then
                goto 2001
           else
               call push_ahistory(strung)
           endif
        endif

        if (iarg.eq.15104 .or. iarg.eq. 8960) then
          call calc_help
          goto 200

        elseif (iarg.eq.27) then
            if (ISHIST.EQ.1) then               !ERASE HISTORY LINE
                 call down_line(0)
                 call del_to_end
                 goto 2001
             else
                 stop ' Exiting CALC '
             endif
        elseif (iarg.gt.255)  then     !un used cursor command
                goto 200
        END IF
        call PROCESS_strung(strung,in_unit)
        if (strung.eq.' ') goto 200             !ignore blank lines
        if (strung.eq.'?') strung='HELP'

        GOTO 10

        END

c
C----------------------------------------------------------
C GET CONTINUATION (IF requested)  AND CLEAN strung UP

        SUBROUTINE Process_strung(strung,in_unit)

        CHARACTER *(*) strung
        character *100 ascird_2b,atmp,get_from_file,atmp2

        CALL CALC_CLEANUP_strung(strung,0)        !CLEAN UP strung

        MLEN=LEN(strung)                        !MAX LENGTH


 10     NExc=INDEX(strung,'!')          !LOOK FOR FIRST "!"

 1002   IF (NExc.GT.0)  THEN    !CONTINUATION MARK
 1003     continue
          if (in_unit.eq.5) then
              atmp=' '
              call down_line(1)
 1004         atmp2=atmp
              iarg=1
 1005         atmp=ascird_2b(' :More!:',iarg,atmp2)

              if (iarg.eq.18432) then          !up arrow
                 call pop_ahistory(atmp,-1)
                 goto 1004
              elseif (iarg.eq.20480) then    ! down arrow
                  call pop_ahistory(atmp,1)
                  goto 1004
              elseif (iarg.lt.255) then
                 call push_ahistory(atmp)
                 call down_line(0)
              endif
          else
             atmp=get_from_file(in_unit)
             iarg=10
             if (in_Unit.eq.5) then
                goto 1003
             else
                 call push_ahistory(atmp)
             endif
          endif

          if (iarg.gt.255) then
              call calc_help
              goto 10
           elseif (iarg.eq.27) then
               stop ' Exiting CALC '
           endif

           CALL CALC_CLEANUP_strung(atmp,0)        !CLEAN UP atmp
           strung(nexc:mlen)=atmp              ! add to equation string

           CALL CALC_CLEANUP_strung(strung,0)        !CLEAN UP strung
           goto 10                              !look for another continuation mark
        END IF

        if (in_unit.eq.5) call del_to_end

c got all continuations.
C Note, calc_cleanup_strung is called by "action modules", and is necessary to
c remove clutter,etc.

        return
        end

C----------------------------------------------------------
c See what kind of action to under take, and return some values
c  depending on action.
c  At this point we expect a "processed" strung.

        Subroutine ACTION_TYPE(strung,          !from caller
     1          Todo,Pstrung,Name,Argument,Qsave)  !returned to caller

        PARAMETER (NPERMFUNK=24)

        Character *(*) strung,pstrung,name,argument

        Integer Todo
        Logical Qsave
        character *1 aa,next_char

        integer exit,setvari,setfunk,showvari,showfunk,help,calc,fmtset
        INTEGER OUTSET,Idefine,trigradians,trigdegrees,doderiv,ninset
        integer remark,simplify
        common /actions/
     1 exit,setvari,setfunk,showvari,showfunk,help,calc,fmtset,OUTSET,
     1   INIT,Idefine,trigradians,trigdegrees,doderiv,ninset,SUMUP,
     1   remark,simplify


c set some pointers
        ngrab=0
        nlen=messlen(strung)
        neq=index(strung,'=')

        if (neq.gt.1) then
           nparen=index(strung(1:neq),'(')
           IF (NPAREN.GT.0) THEN
             NPAREN2=INDEX(strung(1:NEQ),')')
             if (nparen2.le.nparen+1) then
               call Calc_Error(' Bad Function definition ')
               return
             end if
           END IF
        else
           nparen=0
        end if

c now,see what kind of action


        if (strung.eq.'HELP' )then
           Todo=help
        ELSE IF (strung.EQ.'/INIT') THEN
           TODO=INIT
        else if (strung.eq.'/DEFINE') then
           todo=Idefine
        else if (strung.eq.'/RADIANS') then
            todo=trigradians
        else if (strung.eq.'/DEGREES') then
            todo=trigdegrees
        else if (strung(1:4).eq.'/OUT') then
           TODO=OUTSET
           ngrab=4
        else if (strung(1:3).eq.'/IN') then
           TODO=ninset
           ngrab=3
        else if (strung(1:4).eq.'/VAR') then
           Todo=showvari
           ngrab=4
        else if (strung(1:4).eq.'/FUN') then
           Todo=showfunk
           ngrab=4
        else if (strung(1:4).eq.'/FMT') then
           todo=fmtset
        else if (strung.eq.'EXIT'.or.strung.eq.'QUIT') then
           Todo=exit
        else if (neq.le.0) then
           Todo=calc
           Pstrung=strung
        else if (nparen.lt.1)  then                     ! here to set vari
           Name=strung(1:neq-1)         !the variable
           Pstrung=strung(NEQ+1:LEN(strung))    !strung SHOULD BE THE EQUATION ONLY
           IF (strung(Neq+1:Neq+2).EQ.'/S') THEN        !SAVE VALUE OF MOST RECENT EQUATONS
              Qsave=.TRUE.
           ELSE                                 !SAVE FOLLOWING EQUATION
             Qsave=.False.
           END IF
           Todo=SetVari
        else if (nparen.gt.0) then                      !function set
           Argument=strung(nparen+1:nparen2-1)
           Name=strung(1:NPAREN-1)
           ToDo=SETFUNK
           Pstrung=strung(NEQ+1:NLEN)   !PROCESS PART BEYOND = SIGN
        end if

        if (ngrab.gt.0) then                    !ancillary info
           ngrab2=NEXT_NON_CHAR(ngrab,STRUNG,LEN_trim(strung))  !next ' '
           aa=next_char(ngrab2,i6,strung)
c        print *, ' ngrab ',ngrab,ngrab2,i6
           if (aa.eq.' ') then      !check for function name
                pstrung=' '
           else
                pstrung=strung(i6:len_trim(strung))
           endif
        endif

        return
        end



cc========================================
C HELP FOR CALC PROGRAM

        SUBROUTINE  CALC_HELP

        character *60 outname,inname
        common /cm_outname/outname,inname

        character *76 amess
        character *4 a4,a4a,A4B,a5
        a4(1:1)=char(27)
           a4(2:2)='['
           a4(3:3)='1'
           a4(4:4)='m'
           a4a=a4
           A4B=A4
        a4a(3:3)='0'
        A4B(3:3)='7'
        a5=a4a
        a5(3:3)='5'

 557        FORMAT(/1x,a,
     1 ' Enter Equation to Calculate, or ESC to exit CALC ...',a/)

        ireturn=1
 2      continue
        if (ireturn.eq.0.or.ireturn.gt.6) then                  ! exited
            write(6,557)a4,a4a
            return
        else

           if (ireturn.ne.6) then
             IF (IRETURN.NE.1) THEN
                  write(amess,331)ireturn
 331             format(' CALC HELP.  Page = ',i3,' of 6')
             ELSE
                  write(amess,332)ireturn
 332       format(' CALC. FreeWare from DMH @ USDA\ERS\RTD (1/92).',
     1             ' Page = ',i3,' of 6')
             endif
           else
               amess=' CALC HELP. Last Page '
           endif
           call print_at(1,1,14,2,amess,1)
           IF (IRETURN.EQ.1) THEN
              CALL PRINT_AT(3,20,15,6,
     1              'CALCULATOR PROGRAM: Introduction',0)
               print *,a4a
           elseIF (IRETURN.EQ.2) THEN
              CALL PRINT_AT(3,30,15,2,' Synopsis ',0)
           elseIF (IRETURN.EQ.3) THEN
              CALL PRINT_AT(3,30,15,2,' Functions ',0)
           elseIF (IRETURN.EQ.4) THEN
              CALL PRINT_AT(3,30,15,2,' Hints. ',0)
           elseIF (IRETURN.EQ.5) THEN
              CALL PRINT_AT(3,30,15,2,' Switches  ',0)
           elseIF (IRETURN.EQ.6) THEN
              CALL PRINT_AT(3,30,15,2,' Switches ..  ',0)
           ENDIF
           GO TO (110,120,130,140,150,160),ireturn
        endif

 110   WRITE(6,    20)a4b,a4a
   20  FORMAT(1x,a,
     1 '  Features:',a/
     1 '          Up to 100 variables may be saved.'/
     1 '          Variables can be freely inserted into equations.'/
     1 '          25 intrinsic functions, such as SIN and LOG.'/
     1 '          Up to 34 User Definable functions allowed.'/
     1 '          Free Style input of numbers,',
     1               ' with PI and E built in.'/
     1 '          Control of output format.'/
     1   )
       WRITE(6,30)A4B,A4A,A4B,A4A,A4B,A4A,a4b,a4a,a4b,a4a,a5,a4b,a4a
   30  FORMAT(
     1  1X,A,
     1 '  Simple Usage:'/
     1 1x,a,t10,
     1     ' Simply enter the equation you want to analyze; then'
     2 ' hit ENTER.'/
     1 T10,
     1     ' The equation will be echoed, followed by the answer.'/
     1  /,1X,a,
     1 ' Example:: ',A/
     1  T6,a,
     1 '? ',a,' 9,000-(8.9E01*-2)/3.',T65,
     1 '(user input)'/
     1 7x,a,
     1 '9,000-(8.9E01*-2)/3. == 9059.33333 ',A,T60,
     1 '(',a,'output ',a,' from computer)'//
     1 t20,a,a,' Note: To exit program, hit the ESC key. ',a/
     1   )

        CALL calc_PAUSEHELP(IRETURN)
        goto 2


 120   WRITE(6,    40)a4b,a4a,a4a,a4b,a4a
   40  FORMAT(
     1  /1x,a,
     1 ' Number Entry:'/1x,a,5x,a,
     1 ' Numbers can be entered as real, integer or exponenti'
     2 'al notation'/,t5,
     1 ' (integers will be converted to real). Commas may be inserted'
     2 ' (if desired)'/,t5,
     1 ' without having any effect on the number. Eg: The following h'
     2 'ave'/,t5,
     1 ' the same value:  9000.0  : 9000  :  9.0E+03   : 9,000.00 : 9'
     2 ',000'/
     1  /1x,a,
     1 'Math Operators',a,
     1 ' (listed in order of increasing precedence):'/
     1 t5,'+ and -    ::   *, / and %    :: ** or ^ (exponentiation) ',
     2 ' ::    Functions'/
     1 t30,' Note: % is the MOD operator, e.g. 15 % 6 = 3. '
     1   )
       WRITE(6,    50)a4b,a4a
   50  FORMAT(1x,a,
     1  ' Variables:'/1x,a,t10,
     1 'Names: Can',
     1 ' be 12 characters long. Alphabetics, digits, the apostrophe,'
     2   /t11,
     2  ' ( '' ), and the underscore ( _ ) are allowed',
     2 ' characters. Eg: VAR_1'''
     1  /t6,
     1 'Assigning a value:'/t16,
     1 'VARNAME=EQUATION        eg: X1=98 - 2(SIN(45) - .2)'
     1  /t6,
     1 'Saving most recent value to a variable:'/t16,
     1 'VARNAME=/S              Eg:   V1=/S'/
     1   )

        CALL calc_PAUSEHELP(IRETURN)
        goto 2


 130   WRITE(6,    60)a4b,a4a,A4B,A4A
   60  FORMAT(/1x,a,
     1 ' Permanent Functions:',a/
     1 '         SIND  COSD  TAND     SINR  COSR  TANR',
     1       '   (xxxR functions for radians)'/
     1 '         ARCSIND     ARCCOSD   ARCTAND  (xxxxxxR for radians)'/
     1 '         ABS   EXP   EXP10     LOG (or LN)    LOG10'/
     1 '         INT   SQRT  RADTODEG  DEGTORAD'/
     1 '         SINH  COSH  TANH'/
     1 '         FACT (factorial: works with integers from 0 to 169) '/
     1 '    Note: trig functions can be called without "type" '
     1               'specifier: eg; SIN'/
     1 '          Initial default is DEGREES',
     1            ' (this can be changed, see below).'/
     1  /1x,a,
     1 ' User Function Assignation:',a/
     1 t6,' Up to 34 user functions can be cr'
     2 'eated and saved.'/t6,
     1 ' The user function name and the argument name follow the same'
     2 ' rules as'/t6,
     1 ' variable names (see above).'/
     1   )

        CALL calc_PAUSEHELP(IRETURN)
        goto 2


 140   WRITE(6,    80)a4b,a4a,a4b,a4a,a4b,a4a
   80  FORMAT(1x,a,
     1 ' Continuation Line::',a/t6,
     1 'If your equation will not fit on one line, put a ! at th'
     2 'e end of the line.'/t6,
     1 'You will then be prompted for the "continuation" of  the'
     2 ' equation'/
     1  1x,a,
     1 '  Parentheses ::',a/t6,
     1 'All 3 types of  parentheses  ( "(","{",and "[" ) can be'
     2 ' freely mixed.'/t6,
     1 '{ and [ will be converted to  ( , ditto for ] and }.'
     1  /1x,a,' Cursor Keys::',a/t6,
     1  ' <-, ->, Home, End, Del, and Ins keys can be used to help',
     2        ' edit equation.'/
     1      t6,'  Up and Down arrow keys to recall (up to 10) prior',
     1                    ' equations.'/  )

        write(6,83)a4b,a4a,a4b,a4a
 83     format(1x,a,' Implicit Multiplication :: ',a/t6,
     1  ' If you do not put an "operator" between 2 items, then ',
     1          ' multiplication'/
     1  ' is assumed.  Example: ',
     1           'the following are equivalent expressions --'/t9,
     1  '3*PI == PI*3 == 3 PI  == 3 (PI)  == 3PI  == PI 3 == (PI)3 '/
     1  ' However, PI3 is interpted as "variable with name of PI3",',
     1  ' and NOT as PI*3.'/
     1  t6,a,'CAUTION:',a,' Be careful in how you use the letter E,',
     1     ' since E can be used'/
     1  t14,' as a variable (2.71) or to denote',
     1      ' scientific notation (0.31E05).')
        write(6,831)a4b,a4a
 831    format(/
     1  1x,a,' Overflow::',a,
     1   ' CALC uses uses double precision, with a maximum value',
     1         ' of 1.0E+300'/
     1   ' permitted.  For functions, max args are:',
     1      ' SIN,etc=1.0e10, Factorial=170, Exp=750.')

        CALL calc_PAUSEHELP(IRETURN)
        goto 2

 150   WRITE(6,    90)a4b,a4a,a4b,a4a


   90  FORMAT(/t10,
     1 ' Certain commands are "switches" that set defaults, etc.:'/
     1  t10,'  ( [xx] means that xx is optional, "'
     1 //
     1  1x,a,'    SWITCH   ',a,23x,a,' Description',a/
     1 4x,' /INIT            :: Initialize variables, functions, &',
     1                          ' simplification'/
     1 4x,' /DEFINE          :: ReDefine ALL user-defined variables '/
     1 4x,' /VAR   [varname] :: List a  variable and its value.'/
     1 4x,' /FUN  [function] :: List a function (name and equation)'/
     1 4x,' /FMT             :: Set "format" used  '
     2 ,' for displaying numbers & functions.'/
     1 4x,' /OUT  [file.ext] :: Toggle -- send output',
     2         ' to terminal and a ".LST" file. ')
        if (outname.ne.' ') then
           WRITE(6,911)outname(1:35)
 911    FORMAT(18X,' Current output file: ',a)
        else
           WRITE(6,913)
 913       format(18X,' Currently: No Output File ')
        endif
       WRITE(6,  901)
  901  FORMAT(
     1 4x,' /IN  [file.ext]  ::',
     1              ' Read equations from an input file'/
     1 4x,' /RADIANS and  /DEGREES   ::',
     1      ' Set default trig argument type'/)

        CALL calc_PAUSEHELP(IRETURN)
        goto 2

 160    write(6,955)
 955    format(
     1  5x,'For /FUN and /VAR, if no argument is given,',
     1          ' ALL functions (or variables)'/
     1  ' are displayed.  For /IN and  /OUT, ',
     1            'if no argument is given,'/
     1  ' you will be prompted appropriately.')

       WRITE(6,    81)a4b,a4a,a4b,a4a
   81  FORMAT(/1x,a,
     1 ' Reading from File::',a/t6,
     1 ' To analyze several equations, you can read equations',
     1       ' from a file, '/
     1  t8,' using the /IN switch:  '/
     1  t32,' X=(45-6)/(PI^2) '/
     1  t14,a,' Example:',a,3x,t40,' F(Z)= (Z/10) X '/
     1  t32,' F(200) '/)


        CALL calc_PAUSEHELP(IRETURN)
        goto 2

       RETURN
       END



        SUBROUTINE calc_PAUSEHELP(IRETURN)
        integer *2 dummy2,getchasm
        character *4 a4,a4a
        a4(1:1)=char(27)
           a4(2:2)='['
           a4(3:3)='1'
           a4(4:4)='m'
           a4a=a4
        a4a(3:3)='0'

        if (ireturn.eq.1) then
            print *,a4,
     1  '   ESC to exit HELP, PGDN for Next Page ',a4a

        elseif (ireturn.eq.6) then
            print *,a4,
     1  '   ESC to exit HELP, PGUP for Prior Page ',a4a

        else
            print *,a4,
     1  '  ESC to exit HELP, PgDn for Next Page, PgUp for Prior Page ',
     1  a4a
        endif

        dummy2=0
        do while(dummy2.eq.0)
          dummy2=getchasm()
        enddo

        if (dummy2.eq.27) then
            ireturn=0                           !0=done
        elseif (dummy2.eq.18688) then   ! pgup
          ireturn=ireturn-1
        elseif(dummy2.eq.20736)then
          ireturn=ireturn+1
        elseif (dummy2-48.gt.0.and.dummy2-48.lt.10) then
           ireturn=dummy2-48
        else
          ireturn=min(ireturn+1,10)
        END IF
        return
        end





c===========================
c modify pstrung so that CALC reads it as a series of + .. + .. +

        subroutine SETUP_Sumup(PSTRuNG)

        character *(*)pstrung
        CHARACTER *1 ACHAR,NEXT_CHAR

        ilen=len_trim(pstrung)

c Skip throuhg pstrung finding words.  Between each word, with a word
c defined by a delimiter (space, : or ;) insert a +

        nc=0

        do ij=1,ILEN
           achar=next_char(nc,nc1,pstrung)
           if (achar.eq.' ') THEN
              return                !pstrung has been changed
           else
             IF (NC.GT.1) pstrung(nc1-1:nc1-1)='+'
           endif
           NC=NEXT_NON_CHAR(NC1,PSTRUNG,ILEN)
        enddo

        RETURN          !SHOULD NEVER GET HERE, BUT ...
        end

c======================================
c Initialize various stuff

        subroutine do_calc_init

        parameter(NPERMFUNK=25)
        PARAMETER (MAXFUNK=59)
        parameter(ntmpfunk=maxfunk-npermfunk)
        PARAMETER (MAXVARI=100)

        CHARACTER *12 VARINAMES(MAXVARI)
        REAL *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        CHARACTER *12 FUNKargs(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        integer *2 i22,getchasm
        character *12 ans,ascird_2a,atmp
        character *4 a4,a4a,A4B,a5
        character *3 yno(2)


        yno(1)='NO'
        yno(2)='YES'
        a4(1:1)=char(27)
           a4(2:2)='['
           a4(3:3)='1'
           a4(4:4)='m'
           a4a=a4
           A4B=A4
        a4a(3:3)='0'
        A4B(3:3)='7'
        a5=a4a
        a5(3:3)='5'

 10     ans=ascird_2a(
     1   ' Select option: ALL, FUNC, VAR, ESC to Exit: ',
     1                  io)

       if (io.gt.255.or.ans.eq.' ') ans='?'
       if (io.eq.15104.or.io.eq.8960) then
                         ans='?'
       elseif (io.gt.255.or. io.eq.27) then
                return
       endif
       call captaliz(ans)
       if (ans.eq.'?') then
         write(6,8981)a4b,a4a
 8981     format(3x,a,
     1  ' Initialization options: ',a/
     1  ' ALL      == Clear all variables and function '/
     1  ' FUNC     == Clear a user-selected function '/
     1  ' VAR      == Clear a user-selected variable '/
     1  ' ESC key  == Return to ? prompt '/)
            goto 10
        endif

C ALL
        if (ans(1:3).eq.'ALL') then
            call zero_anstack
            call calc_init
            goto 10
        endif

c FUNC
        if (ans(1:3).eq.'FUN') then
           if (nfunk.le.npermfunk) then
                print *,' No user functions to remove '
                goto 10
           endif

           do ij=npermfunk+1,nfunk
                if (funkaddress(ij-npermfunk).eq.0) cycle
                atmp=funknames(ij)
                write(6,578)atmp
 578             format(10x,' Function: ',a,' : Y to remove ? '\)
                i22=0
                do while(i22.le.0)
                   i22=getchasm()
                enddo
                if (i22.eq.27.or.i22.gt.255) goto 10
                if (.not.(i22.eq.89.or.i22.eq.121)) THEN
                   call down_line(1)
                   cycle                  !else, get next one
               ENDIF

                call remove_funk(atmp,istat)
                if (istat.eq.1) then
                    print *,' Impossible function name:',atmp
                    stop
                elseif (istat.eq.2) then
                   print *,'   Trying to Remove permanent function: ',
     1                         atmp
                   stop
                else
                     call calc_garbageman    !make em immediately available
                     print *,' << REMOVED >> '
                endif
            enddo
            call zero_anstack
            call calc_garbageman    !make em immediately available
        endif

        if (ans(1:3).eq.'VAR') then
           if (nvari.le.3) then
                 print *,' No user variables '
                goto 10
          endif
          do ij=4,nvari

                atmp=varinames(ij)
                write(6,5781)atmp
 5781             format(10x,' Variable: ',a,' : Y to remove ? '\)
                i22=0
                do while (i22.le.0)
                   i22=getchasm()
                enddo
                if (i22.eq.27.or.i22.gt.255) goto 10
                if (.not.(i22.eq.89.or.i22.eq.121)) THEN
                   call down_line(1)
                   cycle                  !else, get next one
               ENDIF

              call remove_variname(atmp,istat)
              if (istat.eq.1) then
                print *,'       No such variable: ',atmp
                goto 10
             elseif (istat.eq.2) then
                 print *,'     Can not remove permanent variable:',atmp
                 goto 10
             else
                 print *,'  << REMOVED >> '
             endif
          enddo
        endif


        goto 10                 !no such option

        end


c======================================================================

        SUBROUTINE calc_EQUATION(Answer,pSTRING,NERR2)


        CHARACTER *(*)pSTRING

        CHARACTER *160 PSTRING1
        COMMON /CALC_STRINGDUMP/PSTRING1

        common /calc_CMERROR/NERR


        real *8 answer,analyze_tree
        common /calc_CALCDEBUG/NODE

        integer nil2                    !needed by calc_routines
        common /calc_cmnil/nil2

        data nil2/0/

        call calc_createvar(.false.)
c                               !if false, undefined variables are fatal errors.

        NERR=0                  !RESET ERROR COUNTER
        NERR2=1                         !ASSUME ERROR

        PSTRING1=PSTRING
        call calc_cleanup_strung(pstring1,0)

        CALL calc_readequation(pstring1,' ')     !' ' signals NO "function" argument.
        IF (NERR.GT.0) return

        CALL BUILD_TREE(node)           !NODE IS RETURNED, IS  BASE OF EQUATION.
        IF (NERR.GT.0) GOTO 200

        answer=analyze_tree(node,nil2)          !Node is returned from BUILD_TREE.
        IF (Nerr.gt.0) GOTO 200

 100    continue
        NERR2=0                 !NO ERROR
        RETURN

 200    CONTINUE                        !ERROR RETURN
        RETURN

        end




c======================================================
c Analyze equation and set value equal to variable. Also return
c  value to user (in answer)

        Subroutine calc_variable(answer,Vname,pstring,nerr2,qsave)

        Character *(*)Vname,pString
        logical qsave

        common /calc_CMERROR/NERR
        common /calc_CALCDEBUG/NODE

        real *8 answer,analyze_Tree

        CHARACTER *160 PSTRING1
        COMMON /CALC_STRINGDUMP/PSTRING1
        CHARACTER *12 VNAME1

        integer nil2                    !needed by calc_routines
        common /calc_cmnil/nil2
        data nil2/0/


        PSTRING1=PSTRING
        call calc_cleanup_strung(pstring1,0)
        VNAME1=VNAME
        CALL CALC_CLEANUP_STRuNG(VNAME1,1)

        call calc_createvar(.false.)

        NERR=0                  !RESET ERROR COUNTER

        NERR2=1                 !ASSUME ERROR

           IF (vname1.eq.' '.or.VNAME1.EQ.'E'.OR.VNAME1.EQ.'PI') THEN
           CALL Calc_Error(
     1       '  ! ATTEMPT TO REDEFINE PERMANENT VARIABLE')
              RETURN
           endif


        if (qsave) then
           call SET_VARINAME(Vname1,PointVar)  !AVAL WILL POINT TO VARIABLE TABLE
           mvari=PointVar                    !(prior location or create a new one)
           call CALC_putvari(mvari,vname1,answer)  !answer contains prior answer from
           NERR2=0
           return                                !calc_equation or from calc_variable
        endif

c else, we have equation to analyze

        CALL calc_readequation(pstring1,' ')     !' ' signals NO "function" argument.
        IF (NERR.GT.0) return

        CALL BUILD_TREE(node)           !NODE IS RETURNED, IS  BASE OF EQUATION.
        IF (NERR.GT.0) GOTO 200

        answer=analyze_tree(node,nil2)          !NOTE:: THIS IS ONLY SPOT WHERE FF IS
        IF (NERR.GT.0) GOTO 200         ! GIVEN A VALUE

        call SET_VARINAME(Vname1,PointVar)    !AVAL WILL POINT TO VARIABLE TABLE
        mvari=PointVar                   !(prior location or create a new one)

        call CALC_putvari(mvari,vname1,answer)  !perhaps redundant naming. so what...

        NERR2=0                         !NO ERROR
        return

 200    CONTINUE                        !ERROR RETURN
        RETURN

        end



C=================================================
c analyze user function, given an argument value

        Subroutine calc_function(Fname,argvalue,answer,nerr2)

        implicit none

        integer npermfunk
        paramEter (NPERMFUNK=25)

        Character *(*)Fname
        Character *12 fname2

        real *8 argvalue,analyze_tree
        real *8 answer
        real fnum               !function number

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER NERR,NERR2
        INTEGER MVNODE,MAKE_VALNODE,FNODE
        REAL CALC_GET_FUNKADDRESS

        NERR=0                  !RESET ERROR COUNTER
        NERR2=1                         !ASSUME ERROR

         FNAME2=FNAME
          CALL CALC_CLEANUP_STRuNG(FNAME2,1)
         call find_func(Fname2,fnum)                !FIND FUNCTION ID
         if (fnum.EQ.0 ) THEN
             call Calc_Error('  ! No such User function ')
             return
         elseif (FNUM.LE.NPERMFUNK) THEN
             CALL CALC_ERROR(' ! Not a USER function ')
             return
         endif

        FNode=calc_Get_FunkAddress(Fnum)        !the tree address of user function

        mvnode=make_valnode(ArgValue,VALU)      !MAKE VALUE NODE

        IF (NERR.GT.0) RETURN

        answer=analyze_tree(FNODE,mvnode)               !now analyze tree

                                                        !node anymore
        if (nerr.gt.0) return

        nerr2=0                                 !no errors

        return
        end




c========================================================
c  Create a user function by saving an "equation tree" in a location
c  that future "equation analysis" requests can access.

        Subroutine calc_LOADFunction
     1            (FUNCName,UserArg,PSTRING,nerr2)


        paramEter (NPERMFUNK=25)

        character *(*) FUNCname,userarg,PSTRING      !PSTRING IS THE EQUATION

        common /calc_cmerror/nerr
        common /calc_CALCDEBUG/NODE


        CHARACTER *160 PSTRING1
        COMMON /CALC_STRINGDUMP/PSTRING1
        CHARACTER *12 FUNCNAME1,USERARG1

        pstring1=pstring
        call calc_cleanup_strung(pstring1,0)

        FUNCNAME1=FUNCNAME
        CALL CALC_CLEANUP_STRuNG(FUNCNAME1,1)

        USERARG1=USERARG
        CALL CALC_CLEANUP_STRuNG(USERARG1,1)

        call calc_createvar(.true.)
                                !if True, undefined variables are created = 0

        NERR=0                  !RESET ERROR COUNTER

        NERR2=1                 !ASSUME ERROR

        IF (userarg1.EQ.'E'.OR.userarg1.EQ.'PI'.or.
     1       userarg1.eq.' ')THEN
           CALL Calc_Error(
     1     ' ! Can Not Use Permanent Variable as Function Argument')
            RETURN
         endif

         call find_func(FUNCname1,fnum)
         if (fnum.lE.npermfunk.AND.FNUM.GT.0) then     ! 0 SIGNIFIES NEW FUNK
             call Calc_Error(
     1           ' ! Attempt to redefine permanent function')
             return
         endif

        CALL calc_readequation(pstring1,userarg1)     !userarg is "function" argument.
        IF (NERR.GT.0) return

        CALL BUILD_TREE(node)           !NODE IS RETURNED, IS  BASE OF EQUATION.
        IF (NERR.GT.0) GOTO 200

c  if here we have a valid user function name and argumentname, and a valid
c    function equation.

         mvari=fnum                     !mvari will be used as index.
           if (mvari.eq.0) then         !first time for this name
              call set_funk(FUNCname1,node,userarg1)
           else                         !function name being replaced
              call fill_funk(mvari,FUNCname1,userarg1,node)
           endif

        NERR2=0                 !NO ERROR
        return

 200    CONTINUE                !ERROR RETURN
        RETURN

        end

C=================================================
c==========================================================
c routine to display functions as entered.

        Subroutine CALC_ShowFunks(IOUT,ndec,qe,afunk,maxlen0)

        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)
        character *(*) afunk
        logical qe
        character * 1000 fstring

        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK),ffn
        character *12 funkargs(ntmpfunk),atmp
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        character *4 a4,a4a,A4B,a5

        common /calc_cmerror/nerr

        NERR=0                  !RESET ERROR COUNTER
        maxlen=maxlen0
        if (afunk.NE.' ') maxlen=1000       !force complete display of
                                        ! single functions
        a4(1:1)=char(27)
           a4(2:2)='['
           a4(3:3)='1'
           a4(4:4)='m'
           a4a=a4
           A4B=A4
        a4a(3:3)='0'
        A4B(3:3)='7'
        a5=a4a
        a5(3:3)='5'
        I1=NFUNK-NPERMFUNK

        if (I1.LE.0) then
           WRITE(IOUT,22)
 22        FORMAT(' No user functions')
           return
        endif

c else, there are some user functions

         if (afunk.ne.' ') then                 !show only 1
              atmp=afunk
              call find_func(atmp,fid)                !FIND FUNCTION ID
              iat=calc_Get_FunkAddress(Fid)
        else
              iat=0
              if (iout.eq.6) then
                 print *,' User Functions, ',maxlen0, ' characters ',
     1         ' displayed (use /FMT to change this):'
              endif
        endif

        DO IJ=1,I1
              II=FUNKADDRESS(IJ)
              if (ii.eq.0) cycle                !was deleted

              if (iat.ne.0 .and. ii.ne.iat) cycle  ! skip if only display 1
                                                ! and this aint the one.
              ffn=funkargs(ij)
              call load_funkargname(ffn)
              call zero_anstack
              CALL Tree_string(fstring,II,ndec,qe)
              nn=min(len_trim(fstring),maxlen)
              iadd=200
              if (iout.eq.6) iadd=40
              if (nn.gt.iadd) then   ! wrap it around
                  i1=1
                  i2=iadd
                  write(IOUT,66)
     1               funknames(npermfunk+ij),ffn,fstring(i1:i2)
                  do while (i2.lt.nn)
                      i1=i1+iadd
                      i2=min(i2+iadd,nn)
                      if (i2.eq.nn .and. i2.lt.len_trim(fstring) ) then
                          write(IOUT,6612)fstring(i1:i2),a5,a4a
                      else
                          write(IOUT,661)fstring(i1:i2)
                      endif
                  enddo
              else                      !fit on one line
                  if (len_trim(fstring).le.nn) then
                    write(IOUT,66)
     1                funknames(npermfunk+ij),ffn,fstring(1:nn)
                  else                     !too long
                    write(IOUT,662)
     1                funknames(npermfunk+ij),ffn,fstring(1:nn),a5,a4a
                  endif
              endif
        end do

 662        FORMAT(t2,': ',a,'(',a,') = ',a,a,' ¯',a)
 66        FORMAT(t2,': ',a,'(',a,') = ',a)
 661       format(t29,'...',t34,a)
 6612      format(t29,'...',t34,a,a,' ¯',a)
        return
        end

c==========================================================

C ROUTINE TO DISPLAY ALL CURRENT VARIABLES

        SUBROUTINE CALC_SHOWVARIS(IOUT,ndec,qe,ANAME)

        PARAMETER(MAXVARI=100)

        logical qe
        character *25 charans
        CHARACTER *(*)ANAME
        CHARACTER *12 VARINAMES(MAXVARI),ANAME1,ANAME2
        real *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues
        common /calc_cmerror/nerr

        NERR=0                  !RESET ERROR COUNTER

        print *,' Variables: '
        ANAME1=ANAME(1:LEN_TRIM(ANAME))
        DO IJ=2,nvari           !POSITION 1 RESERVED FOR FUNKARGUMENT INDEX
           if (varinames(ij).eq.' ') cycle   !erased spot
           IF (ANAME.EQ.' ') THEN
                call calc_writenum(varivalues(ij),charans,ndec,qe,nlen)
                WRITE(IOUT,55)VARINAMES(IJ),charans(1:Nlen)
           ELSE
              ANAME2=VARINAMES(IJ)
              ANAME2=ANAME2(1:LEN_TRIM(ANAME2))
              IF (ANAME2.EQ.ANAME1) THEN
                 call calc_writenum(varivalues(ij),charans,ndec,qe,nlen)
                 WRITE(IOUT,55)VARINAMES(IJ),charans(1:Nlen)
              ENDIF
           ENDIF
 55        FORMAT(1x,a,' :==',a)
        end do
        return
        end

C=================================================
C=================================================
C ROUTINE TO DISPLAY AND REDEFINE ALL USER VARIABLES

        SUBROUTINE CALC_DEFINEVars(nvars1)

        implicit none

        integer calc_numbvars
        character *12 get_variname,aname

        real aj
        integer ij,nvars,nvars1

        REAL *8 calc_GET_VariValue,avalue,valdef8

        nvars=calc_numbvars()
        nvars1=nvars-3
        if (nvars.le.3) then
          return
        endif

c else, display and prompt for value

        print *,' Please assign numeric values to current variables:'

        do ij=4,nvars           !first 3 are permanent variables
           aj=ij
           aname=get_variname(aj)       !var number aj
           avalue=calc_GET_VariValue(Aj)

           avalue=valdef8(' Variable :'//aname,avalue)

           call CALC_putvari(ij,aname,avalue)
        end do

        return
        end




c
c Structure of calculation module.
c       1)The program is divided into 3 main sections.
c               a) The string parser.
c               b) The tree builder.
c               c) The equation analyzer.
c
c         The string parser is passed a string, as described above,
c               and returns a list containing containing real values,
c               and a variety of flags (to depict operators, functions, and
c               parentheses).
c
c         The tree builder takes this list and constructs a tree containing
c               values and operators-parens-functions.  This tree is
c               organized according to precedence of operators and according
c               to locations of parentheses.
c
c         The equation analyzer takes this tree and analyzes it in such a
c               manner as to ensure the reflection of higher precedence that
c               was built into the tree by the tree builder, and to correctly
c               deal with user defined functions.
c
c       Recursive techniques are used in this program, which means
c               stack creation and general ugliness (good old Fortran still
c               needs some work).
C=================================================
C=================================================

        subroutine build_tree(mainroot)

        Implicit none

        INTEGER EQuEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQuEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        INTEGER OLDNODE,TMPNODE,n1,n2
        INTEGER VALNODE, Atype,mainroot,tmpopr,iprec,prioropr
        integer make_oprnode,make_valnode,make_funknode
        integer calc_buildmnode,calc_find_Prec

        real *8 aval

        real flagval,abs4_calc
        integer iflagval

        integer fflag

        integer nerr
        common /calc_cmerror/nerr

        integer nil2                    !needed by calc_routines
        common /calc_cmnil/nil2

        data fflag/8/

C Clean up old trees.
        call zero_anstack
        CALL CALC_GARBAGEMAN

c First thing we do is create a "main root", which is the start of
c the entire equation-tree


        mainroot=make_oprnode(1.)
        call  calc_pushd(mainroot,iopr,0,0)

 100    continue
        if (nerr.gt.0) goto 9000                        !test for error

        call calc_get_element(aval,atype)
        if (atype.ne.valu) then
           flagval=aval
        else
            flagval=0
        endif
        iflagval=abs4_calc(aval)      !special abs, handles overflow

        if (atype.eq.PAREN.and.iflagval.eq.BP) then
            atype=oper
            flagval=sign(float(iopr),flagval)
            iflagval=abs(flagval)
        endif

        if (atype.eq.valu.or.atype.eq.vari) then
            valnode=make_valnode(aval,atype)
            call calc_get_element(aval,atype)
            if (atype.ne.valu) then
               flagval=aval
            else
               flagval=0
            endif
            iflagval=abs4_calc(aval)
            if (atype.eq.PAREN.and.iflagval.eq.BP) then
               atype=oper
               flagval=sign(float(iopr),flagval)
               iflagval=abs(flagval)
            endif

            if (atype.eq.valu.or.atype.eq.vari) then
                goto 9000
             endif

            if (AType.eq.PAREN) then       !end of subtree?
               if(iflagval.eq.EP) then
                  goto 500                      !so construct it.
               endif

            elseif (atype.eq.EQUEND) then      !end of equation
                goto 600                        !construct subtree, and
                                                !done.
            elseif (atype.ne.OPER) then        !error
                goto 9000

            else                        !oper
               if (iflagval.eq.iopr) then               ! 4 ( illegal
                  goto 9000
                else
                   goto 200                                     !get next element
                endif
            endif

        elseif (atype.eq.EQUEND.or.
     1        (atype.eq.PAREN.and.iflagval.eq.EP)) Then
             goto 9000

        elseif (atype.eq.OPER) then            !note:atype=oper if BP
           if (iflagval.ne.iopr) then
                goto 9000
           else
                tmpnode=make_oprnode(flagval)
                call calc_pushd(tmpnode,iopr,0,0)
                goto 100
           endif

        elseif (atype.eq.FUNK) then
                tmpnode=Make_funknode(flagval)
                call calc_pushd(tmpnode,fflag,0,0)
                tmpnode=make_oprnode(1.)
                call calc_pushd(tmpnode,iopr,0,0)
                goto 100
        endif                          !


c Here on VAL OPER situation
 200    continue
        if (nerr.gt.0) goto 9000                        !error
        call calc_popd(oldnode,prioropr,n1,n2)  !get prior operator
        iprec=calc_find_prec(prioropr,iflagval)
        if (iprec.eq.2) then                    !eg; 3-4* ,or (3-4
            call calc_pushd(oldnode,prioropr,0,0)       !no shuffling needed
            call calc_pushd(valnode,iflagval,0,0)
            goto 100                                    !continue
        endif

c else, Process subtree until lower precedent operator.
        valnode=calc_buildmnode(prioropr,oldnode,valnode)
        goto 200


c here on EP. Similar to 200, but do something special upon hitting IOPR.
 500    continue
        if (nerr.gt.0) goto 9000                        !error
        call calc_popd(oldnode,prioropr,n1,n2)  !get prior operator
        if (prioropr.ne.iopr) then
            valnode=calc_buildmnode(prioropr,oldnode,valnode)
            goto 500
        endif

c else, we have complete subtree.
 520    continue
        call right_assign(oldnode,valnode)      !attach to RS of subtree
        valnode=oldnode                         !this becomes currentnode
        call calc_popd(oldnode,tmpopr,n1,n2)            !pop back till non funk optype
        if (tmpopr.eq.fflag) then
           goto 520                             !continue back
        endif

c else, resume reading in equation.
        call calc_pushd(oldnode,tmpopr,0,0)
        call calc_get_element(aval,atype)
        if (atype.ne.valu) then
            flagval=aval
        else
            flagval=0
        endif
        iflagval=abs4_calc(aval)
            if (atype.eq.PAREN.and.iflagval.eq.BP) then
               atype=oper
               flagval=sign(float(iopr),flagval)
               iflagval=abs(flagval)       !flagval = bp
            endif

        if (atype.eq.OPER) then                 !either +-*/^ or )
            if (iflagval.ne.iopr) then
              goto 200
            else                                ! NODE ( is illegal
              goto 9000
            endif
        elseif (atype.eq.PAREN.and.Iflagval.eq.EP) then
            goto 500
        elseif (atype.eq.equend) then
           goto 600
        else
           goto 9000
        endif

c Here at EQUEND. SImilar to EP.
 600    continue
        if (nerr.gt.0) goto 9000                        !error
        call calc_popd(oldnode,prioropr,n1,n2)  !get prior operator
        if (prioropr.eq.iopr) then
           if (oldnode.eq.mainroot) Then        !successful tree construction
                mainroot=valnode
                return
           else
                goto 9000                       !premature end
           endif
        endif
        valnode=calc_buildmnode(prioropr,oldnode,valnode)
        goto 600


 9000   continue                !error
           call calc_error(' ! Bad equation specification ')
           return

        end

c----------------------
c get abs value of *8 expression.  Return 0 if TOO BIG (> 1.e9)
        function abs4_calc(aval)
        real *8 aval
        if (dabs(aval).gt. 1.0e9) then
           abs4_calc=0
        else
           abs4_calc=dabs(aval)
        endif
        return
        end

C=================================================
c return node (both of which should by OPER type nodes) with the highest
c  precedent operator.

        integer FUNCTION  calc_FIND_PREC(opa0,opb0)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=1400)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        Integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI


        integer IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP
        INTEGER OPA,OPB,op1,op2,OPA0,OPB0


        OPA=IABS(OPA0)
        OPB=IABS(OPB0)

C COMPARE OPERATORS

          IF (OPA.EQ.ADD.OR.OPA.EQ.MINUS) then
               OP1=1
          elseif (OPA.EQ.IOPR) THEN
               OP1=0
          elseif (OPA.EQ.MULT.OR.OPA.EQ.DIV.or.opa.eq.modop)  then
              OP1=2
          elseif (OPA.EQ.EXP) then
              OP1=3
          endif

           IF (opb.EQ.ADD.OR.opb.EQ.MINUS) then
                op2=1
           elseif (opb.EQ.MULT.OR.opb.EQ.DIV.or.opb.eq.modop)  then
              op2=2
           elseif (opb.EQ.EXP) then
               op2=3
           endif

        IF (OP1.LT.OP2) THEN            !2nd arg is higher precedent
            calc_FIND_PREC=2
        ELSE
            calc_FIND_PREC=1
        endif

        RETURN
        END

C=================================================
c build a math OPER node
        integer function calc_buildmnode(anoper,ls,rs)

        implicit none
        integer tmpnode,make_oprnode,anoper,ls,rs

        tmpnode=make_oprnode(FLOAT(anoper))
        call left_assign(tmpnode,ls)
        call right_assign(tmpnode,rs)

        calc_buildmnode=tmpnode

        return
        end

C===========================================================
C
C ROUTINE TO GET NEXT ELEMENT (VALUE AND OPERATOR) FROM EQUATION LIST

        SUBROUTINE calc_GET_ELEMENT(AVAL,ATYPE)         !check the next element,

        PARAMETER (MAXELEM=210)

        real *8 aval
        INTEGER ATYPE

        Integer *2 elemopr(MAXELEM)
        REAL *8 ELEMVALS(MAXELEM)
        INTEGER ELEMCOUNT

        common /calc_ELEMENTS/ELEMCOUNT,LASTELEM,ELEMOPR,ELEMVALS

        DATA ELEMCOUNT/0/

C WE COULD CHECK FOR OVERFLOW, BUT IT IS SOOO UNLIKELY AS TO BE IMPOSSIBLE
C (160 CHARACTER MAX ON STRING LENGTH)

        ELEMCOUNT=ELEMCOUNT+1
        AVAL=ELEMVALS(ELEMCOUNT)
        ATYPE=ELEMOPR(ELEMCOUNT)

        RETURN
        END







C==============================================
C ROUTINE TO READ AN EQUATION FROM TERMINAL, and create an "equation list"

        SUBROUTINE calc_readequation(string,userarg)

        PARAMETER (MAXELEM=210)
        PARAMETER (maxnode=1400)

        character *(*) userarg,string

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        Integer *2 elemopr(MAXELEM)
        REAL *8 ELEMVALS(MAXELEM)
        INTEGER ELEMCOUNT

        common /calc_ELEMENTS/ELEMCOUNT,LASTELEM,ELEMOPR,ELEMVALS

c  load the value of the "function argumentname", contained in USERARG.
c  If we are analyzing a regular equation (Not a function definition)
c  the userarg will be ' ' (blank).
c This value is needed by parse_equation; it is used to distinguish
c  the "function argument variable" from "external, real valued" variables.

        call load_funkargname(userarg)

        call parse_equation(string)

        ELEMOPR(LASTELEM+1)=EQEND       !PUT IN BRAKE
        ELEMVALS(LASTELEM+1)=NIL        !tells build_Tree to stop.
        LASTELEM=LASTELEM+1

        RETURN
        END



C=========================================================
C
C Routine to return element list given equation string.
c This should have been written with a chart (finite state system)
c  However, it works, so I'll leave it be.

        SUBROUTINE PARSE_EQUATION(STRING)

C BASIC DATA STRUCTURE
        PARAMETER (MAXELEM=210)
        PARAMETER (maxnode=1400)

C    POINTERS   To Left , Right,
C Note a son is attached to either the either the left or right
C  node of its parent
        INTEGER LS,RS
        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER NIL                     !NIL POINTER

        common /calc_cmerror/nerr

        common /calc_NODES/NODE,LS,RS,NIL
        REAL *8 VALUE(MAXNODE)
        Integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        Integer *2 elemopr(MAXELEM)
        REAL *8 ELEMVALS(MAXELEM)
        INTEGER ELEMCOUNT

        common /calc_ELEMENTS/ELEMCOUNT,LASTELEM,ELEMOPR,ELEMVALS


        CHARACTER *(*) STRING

        CHARACTER *1 ACHAR,next_char,apost
        LOGICAL GOTSIGN
        INTEGER OLDELEM,OPTYPE,CALC_KVT_OPER
        REAL *8 AVAL
        INTEGER OLDVAL


c zero equation list
        apost=char(39)                    ! the ' character
        ELEMCOUNT=0
        do ij=1,maxelem
           elemvalS(ij)=0
           elemopr(ij)=0
        end do


        NLEN=LEN_TRIM(STRING)            !CHARACTERS IN STRING


C we expect a PARTIALLY cleaned up STRING (no lower case)
        GOTSIGN=.FALSE.
        ASIGN=1.
        NCHAR=0
        NELEM=0

 100    CONTINUE                        !HERE TO GET NEXT CHARACTER


        NELEM=NELEM+1
        IF (NELEM.GT.MAXELEM-5) GOTO 9000               !TOO BIG (LEAVE ROOM AT END )

110     continue                                !here if asign set (no new elem)(

        if (nerr.gt.0 ) return  !error abort

 112    NCHAR0=NCHAR
          achar=next_char(nchar,nchar,string)      !skip weirdo characters
        IF (aCHAR.eq.' ') THEN
           GOTO 2000            !AT END OF EQUATION
        ENDIF

        IF (ACHAR.EQ.'(') THEN
            ELEMOPR(NELEM)=PAREN
            ELEMVALS(NELEM)=BP*ASIGN            !eg. n*"-(xxx)" situation
            ASIGN=1.
            GOTSIGN=.FALSE.
            GOTO 100
        elseif (ACHAR.EQ.')') THEN
            IF (GOTSIGN) GOTO 9000              !BAD PLACE FOR A SIGN
            ELEMOPR(NELEM)=PAREN
            ELEMVALS(NELEM)=EP

            if (calc_do_mult(nchar,string).eq.1.0) then         !implied mult?
                   nelem=nelem+1
                   ELEMOPR(NELEM)=oper
                   ELEMVALS(NELEM)=mult
            endif
            GOTO 100                    !get more characters
        endif

c -------- operators section
        IF (ACHAR.EQ.'*'.OR.ACHAR.EQ.'/'.OR.ACHAR.EQ.'^'.OR.
     1          ACHAR.EQ.'+'.OR.ACHAR.EQ.'-'.or.achar.eq.'%') THEN

            IF (GOTSIGN)  THEN          !WE ALREADY HAVE A SIGN, SO WE DONT
                GOTO 9000               !WANT AN OPER; CALL ERROR
            endif

            OPTYPE=calc_KVT_OPER(ACHAR)

            if (optype.eq.mult) then            !check for ** exponentiation
               if (nchar.eq.nlen) then          !equation should not end with *
                 call Calc_Error(' !Bad Equation ')
                 return
               else
                  if (string(nchar+1:nchar+1).eq.'*') then   !exp
                     nchar=nchar+1                      !move ahead 1
                     optype=exp                 !and set optype to exp
                   endif               !found a ** operator
               endif                   !not at end of string
            endif                      !optype not mult

            IF (OPTYPE.EQ.MINUS.OR.OPTYPE.EQ.ADD) THEN
                CONTINUE                                !SEE BELOW
            ELSE
               ELEMOPR(NELEM)=OPER              !GOT SIGN CHECKED ABOVE
               ELEMVALS(NELEM)=OPTYPE           !SO ASIGN remains 1.,GOTSIGN=FALSE
               GOTO 100
            endif

C We have a + or -, it might be sign, or it might be operator.
            IF (NELEM.LE.1) THEN                !FIRST POSITION IS SPECIAL
                GOTSIGN=.TRUE.
                IF (OPTYPE.EQ.MINUS) THEN
                   ASIGN=-1.                    !- FLAGS NEGATIVE OPR.
                endif
                GOTO 110
            endif

            ntmp=ELEMOPR(NELEM-1)               !avoid Integer *2 problems
            OLDELEM=iabs(ntmp)
            OLDVAL=ABS4_calc(ELEMVALS(NELEM-1))  !ELEMVALS 8 IS *8,OLDVAL =int


            IF (OLDELEM.EQ.OPER                 !IF PRIOR IS (, FUNK(, OR */+-^
     1           .OR. (OLDELEM.EQ.PAREN.AND.OLDVAL.EQ.BP)
     1           .OR.  OLDELEM.EQ.FUNK)  THEN
                 GOTSIGN=.TRUE.
                 IF (OPTYPE.EQ.MINUS) THEN              !IF +, is default
                     ASIGN=-1.
                 endif
                 GOTO 110
            ELSE                        !EITHER ) OR VALUE/VARIABLE PRECEDES, SO IT IS
               ELEMOPR(NELEM)=OPER      !AN OPER (GOTSIGN AND ASIGN REMAIN NUL)
               ELEMVALS(NELEM)=OPTYPE
               GOTO 100
            endif

        endif                  !FOUND AN OPERATOR
c ------------ end of operators section
C IS IT ALPHABETIC, IF SO, IT IS EITHER FUNCTION OR VARIABLE
        IF ((ACHAR.GE.'A'.AND.ACHAR.LE.'Z').or.
     1       (achar.eq.apost.or.achar.eq.'_') ) THEN
            CALL FUNK_OR_vari(OPTYPE,fVAL,NCHAR,STRING,NLEN)    !FUNK_or_vari
            ELEMOPR(NELEM)=OPTYPE                       !DETERMINES IF FUNK OR VARI
            ELEMVALS(NELEM)=fVAL*ASIGN
            GOTSIGN=.FALSE.
            ASIGN=1.
            if (optype.eq.vari) then               !if a variable ..
              if (calc_do_mult(nchar,string).eq.1) then         !implied mult?
                   nelem=nelem+1
                   ELEMOPR(NELEM)=oper
                   ELEMVALS(NELEM)=mult
              endif
            endif
            GOTO 100
        endif


C IS IT NUMERIC
        IF ((ACHAR.GE.'0'.AND.ACHAR.LE.'9').OR.ACHAR.EQ.'.') THEN
            CALL CALC_MAKE_NUMBER(AVAL,NCHAR,STRING,NLEN)
            ELEMOPR(NELEM)=VALU
            ELEMVALS(NELEM)=AVAL*ASIGN          !EG.  *-9
            ASIGN=1.
            GOTSIGN=.FALSE.
            if (calc_do_mult(nchar,string).eq.1) then         !implied mult?
                   nelem=nelem+1
                   ELEMOPR(NELEM)=oper
                   ELEMVALS(NELEM)=mult
            endif
            GOTO 100
        endif

C IF HERE ,ERROR
        GOTO 9000



C HERE AT END OF STRING
 2000   CONTINUE
        LASTELEM=NELEM-1
        RETURN

C HERE IF ERROR
 9000   CALL Calc_Error(' ! BAD EQUATION SPECIFICATION ')
        RETURN


        END


c------------------------------------------
c  Check to see if an implied multiply is present -- not
c  at end of string, not preceding ), not preceding an operator, hence
c  possibly before number, func, var or (


        function calc_do_mult(nchar,string)

        character *(*)string
        character *1 next_char,achar

        integer calc_kvt_oper

        ilen=len_trim(string)

        if (nchar.ge.ilen) then         !nothing after this
           calc_do_mult=0
        else
            achar=next_char(nchar,ifoo,string)
            if (achar.eq.' '.or.achar.eq.')') then
                  calc_do_mult=0
            else
                if (calc_kvt_oper(achar).eq.0) then
                     calc_do_mult=1
                else
                    calc_do_mult=0
                endif
            endif
         endif
        return
        end


C---------------------------
c extract a number from the string (return as a real *8,
c in AVAL) Rule: nnnE-nn or nnnE+nn is always treated as exponential notation
c and NOT as nnn*E-nn
        SUBROUTINE CALC_make_number(AVAL,NCHAR,STRING,NLEN)

        LOGICAL QERR,qexp
        CHARACTER *(*) STRING
        CHARACTER *24 HOLD
        CHARACTER *1 ACHAR
        REAL *8 AVAL,calc_cvt_number

        HOLD=' '
        HOLD(1:1)=STRING(NCHAR:NCHAR)
        qexp=.false.
        IAT=NCHAR

 100    do while (1.lt.2)               !exit w/EXIT

           IF (IAT.GT.NLEN) THEN
               iend=iat
               exit                     !END OF STRING, SO WE HAVE VALUE
           endif

           IAT=IAT+1

           ACHAR=STRING(IAT:IAT)

C CHECK FOR BREAK CHARACTERS
           IF (ACHAR.EQ.' '.OR.ACHAR.EQ.';'.OR.ACHAR.EQ.':') THEN
                IEND=IAT-1
                EXIT
           ENDIF

           if (achar.eq.'E') then
              qexp=.not.(qexp)
              if (qexp) then                !is E in exponential notation
                  IF (IAT.EQ.NLEN) THEN         !end of string
                        IEND=IAT-1              !so must TREAT e AS VAR
                        exit
                    else                !look for + or - character
                       ACHAR=STRING(IAT+1:IAT+1)
                       IF (ACHAR.EQ.'-'. or. achar.eq.'+') then  !legit e
                           iat=iat+1            !jump over - +
                           cycle
                        elseif (achar.gE.'0'.and.achar.lE.'9') then
                           cycle             !assume +
                        else     ! not valid following E, so E is part  of VAR
                           iend=iat-1
                           exit
                        endif
                     endif

              else               !2nd e, 2nd e is start of new variable
                 iend=iat-1
                 exit
              endif

           elseif (achar.eq.'-'.or.achar.eq.'+') then  !end of number
                iend=iat-1
                exit       !note: + and - for Exp notation already found

           elseIF ((ACHAR.GE.'0'.AND.ACHAR.LE.'9').OR.
     1              ACHAR.EQ.'.'.or.achar.eq.',') then
              cycle                   !read more characters

           else                 ! funk, paren, var, number, other operator
              iend=iat-1
              exit
           endif

c if here, either a digit, an "." , an "E" , or a + or - following an E.
        enddo                    !GET NEXT DIGIT


C HERE WHEN DIGITS ALL IN
        hold=string(nchar:iend)
        AVAL=calc_CVT_NUMBER(HOLD,QERR)         !AVAL WILL POINT TO VARIABLE TABLE
        IF (QERR) CALL Calc_Error(' ! BAD NUMBER SPECIFIED ')
        NCHAR=Iend                !IAT WAS EITHER LAST CHAR OR NON DIGIT BEYOND
        RETURN                          !VARI NAME.

        END


C---------------------------------------------
c convert string, with commas, into real *8

        REAL *8 FUNCTION calc_CVT_NUMBER(HOLD,QERR)


        character *(*) HOLD
        character *35 tmp
        LOGICAL QERR
        real *8 aval,val8

        tmp=' '
        ilen=len_trim(hold)
        iat=0
        do i1=1,ilen
          if (hold(i1:i1).ne.',') then
             iat=iat+1
             tmp(iat:iat)=hold(I1:i1)
           endif
        enddo

        aval=val8(tmp(1:len_trim(tmp)),ierr)
        if (ierr.gt.0) then
            calc_cvt_number=0.0
            qerr=.true.
        else
           calc_cvt_number=aval
           qerr=.false.
        endif

        return
        end


C---------------------------------------------
C ALPHABETIC SUBSTRING MANIPULATION. Determines if a function or
c  a variable, and the index to th function or variable.
C NB: Since aval is simply a flag, it is called with a *4 real.

        SUBROUTINE FUNK_or_VARI(OPTYPE,AVAL,NCHAR,STRING,NLEN)

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        logical qcreate
        common /calc_CmCreateVar/qcreate

        INTEGER OPTYPE
        CHARACTER *(*) STRING

        CHARACTER *12 HOLD
        CHARACTER *1 ACHAR,apost

        data qcreate/.false./

        apost=char(39)
        HOLD=' '
        HOLD(1:1)=STRING(NCHAR:NCHAR)
        IAT=NCHAR
        MMCHAR=1

 100    CONTINUE
           IAT=IAT+1
           IF (IAT.GT.NLEN) THEN
               GOTO 1000                !END OF STRING, SO WE HAVE VARI
           endif
           ACHAR=STRING(IAT:IAT)
           IF (ACHAR.EQ.'(') THEN
              GOTO 2000                         !WE HAVE FUNCTION NAME
           elseif ( (ACHAR.GE.'A'.and.ACHAR.LE.'Z') .OR.
     1              (ACHAR.EQ.'_'.or.achar.eq.apost).OR.
     1              (ACHAR.GE.'0'.AND.ACHAR.LE.'9')
     1                                               ) THEN !VALID NAME
             MMCHAR=MMCHAR+1
             IF (MMCHAR.LE.12) HOLD(MMCHAR:MMCHAR)=ACHAR        !12 CHAR MAX NAME
             GOTO 100
           ELSE                         !END OF NAME, WE HAVE VARI
                GOTO 1000
           endif

C HERE IF VARI
 1000   CONTINUE
        OPTYPE=VARI

        CALL FIND_VARINAME(HOLD,AVAL)
        IF (AVAL.EQ.0.AND.(.NOT.qcreate)) THEN          !UNDEFINED
            CALL Calc_Error(' ! REFERENCING UNDEFINED VARIABLE ')
            RETURN
        ELSE                            !DEFINED OR UNDEFINED.AND.qcreate
           CALL SET_VARINAME(HOLD,AVAL)
        endif

        NCHAR=IAT-1             !IAT WAS EITHER LAST CHAR OR OPER BEYOND
        RETURN                          !VARI NAME.

C HERE IF FUNCTION
 2000   CONTINUE
        NCHAR=IAT                       !IAT STOPPED AT ( CHARACTER
        OPTYPE=FUNK
        CALL FIND_FUNC(HOLD,AVAL)       !AVAL POINTS TO FUNCTION TABLE
        IF (AVAL.EQ.0) THEN
           CALL Calc_Error(' ! UNSPECIFIED FUNCTION ')
        endif
        RETURN
        END


c--------------------------------------------
c set default trig function type (either radians or degrees)

        subroutine calc_radians(qtmp)
        logical qradians                !radians or degrees flag
        common /trigtype/qradians
        logical qtmp

        qradians=qtmp                   !if true, default is radians
                                        !eg; sin is read as sinr

        return
        end

C---------------------------------------------
c routine to instruct "funk_or_vari" on how to deal with "undefined"
c variables. If Qdef=true, then create a new variable (=0) when
c undefined vars are encountered. If false, Error on undefined var.

        Subroutine Calc_createvar(qdef)

        logical qdef

        logical qcreate
        common /calc_CmCreateVar/qcreate

        qcreate=qdef
        return
        end


C=================================================


        INTEGER FUNCTION calc_KVT_OPER(ACHAR)

        CHARACTER *(*)ACHAR

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        IF (ACHAR.EQ.'+') THEN
            calc_kvt_oper=ADD
        elseif (ACHAR.EQ.'-') THEN
            calc_kvt_oper=MINUS
        elseif (ACHAR.EQ.'*') THEN
            calc_kvt_oper=MULT
        elseif (ACHAR.EQ.'/') THEN
            calc_kvt_oper=DIV
        elseif (ACHAR.EQ.'^') THEN
            calc_kvt_oper=EXP
        elseif (achar.eq.'%') then
            calc_kvt_oper=MODOP
        else
            calc_kvt_oper=0
        endif

        RETURN
        END

c=======================================================================
c return string corresponding to operator

        SUBROUTINE CALC_OPER_CVT(CHARANS,fop)

        CHARACTER *(*)CHARANS

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        IF (fop.eq.add) THEN
            Charans='+'
        elseif (fop.eq.minus) THEN
            Charans='-'
        elseif (fop.eq.mult) THEN
            Charans='*'
        elseif (fop.eq.div) THEN
            Charans='/'
        elseif (fop.eq.modop) then
            charans='%'
        elseif (fop.eq.exp) THEN
            Charans='^'
        endif

        RETURN
        END

c---------------------
c compare type and value of two nodes, get left, get right

        integer function GETLR(i1,i2)

        PARAMETER (maxnode=1400)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        integer get_left, get_right

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR


        entry get_left(i1)                      !get a node's ls

        if (i1.lt.0 .or.i1.gt.maxnode) goto 1000
        if (i1.eq.0) then
            get_left=0
        else
            get_left=node(i1,ls)
        endif
        return

        entry get_right(i1)                      !get a node's rs

        if (i1.lt.0 .or.i1.gt.maxnode) goto 1000
        if (i1.eq.0) then
            get_right=0
        else
            get_right=node(i1,rs)
        endif
        return



 1000   continue                        !here on fatal error
            print *,' Bad Node #  kompare_nodes:',i1,i2
           stop

        end


