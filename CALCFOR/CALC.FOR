c @for the public domain, written by DMH @ ERS, 5/92
c====================================================================
C  A CALCULATOR PROGRAM.
C
c 5 /92.
c Written by Daniel Hellerstein
c Economic Research Service, Resources and Technology Division
c 1301 New York Ave NW, rm 438
c Washington, DC 20005
c 202-219-0444
c
c  See also STRGLIB.FOR, CALCLIB1.FOR, CALCLIB2.FOR, CALCSIMP.FOR, CALCSIM2.FOR
c  and CALCDEBG.FOR
C
c  NOTE:: for a fairly complete "user level" description of this program,
c  see the subroutine "Calc_Help".
c
C CALC.EXE can be run in 2 ways
c
c 1) Normal method.
c   Simply type  CALC (from the DOS prompt) and wait for the prompt.
c
c  2) "Inline" method.
c
C
C      Usage::
c  1)    C:>CALC Equation.  The answer will then be typed,
c           and then you are returned to DOS.
c             eg;   C:>CALC 9-(4^2*2)
C               9-(4^2*2)=-23.00000         !output
c  2)    C:>CALC
c           ?                         !prompt from CALC
c
c---------------------------------------------------------------------
c  The following CALC_LIB modules are called by this program.
c       1) Subroutine CALC_INIT
c               Initializes some stacks and other data structures
c               needed by CALC_routines.  Must be done before any
c               other routine called. If called again, deletes all
c               variables, etc.
c
c       2)SUBROUTINE calc_cleanup_strung(strung,i_remove_spaces)
C           Processes the strung and converts it into a form more
c               agreeable to the other routines.
c
c       3)      SUBROUTINE calc_EQUATION(ANSWER,Pstrung,NERR)
c               calc_Equation will take a  "Cleaned up" strung (contained in
c               Pstrung), analyze it and return the value in ANSWER.
c               If any errors are encountered in the analysis,
c               NERR will equal 1, otherwise NERR = 0.
c               Pstrung will not be changed.
c
c        4) SUBROUTINE calc_VARIABLE(ANSWER,VNAME,Pstrung,NERR,QSAVE)
c           If QSAVE is false, then calc_Variable will analyze an
c           equation (in Pstrung, see description in calc_Equation).
c           The value will be saved into the variable location
c           (of the variable named VNAME (Character *12)),
c           and the value will be returned in ANSWER. Nerr is as above.
c          IF qsave is  TRUE, calc_variable will take the value in ANSWER
c               and save into variable VNAME (pstrung is NOT analyzed,
c               ANSWER is NOT changed).
c
c        5) Subroutine calc_loadFunction(FNAME,ANAME,Pstrung,nerr)
C
c               calc_loadFunction  is used to load a user function. It
c                 will take the strung (in Pstrung) and
c                  save it under the name FNAME.  The variable ANAME
c                  will be treated as a function argument variable, as
c                  opposed to an external variable (set by calc_variable).
c                  NERR is as above.
c
C
C       CALC_makederiv(AF,AFprime,Afprimevar,nerr2)
C               Create a new function, with name AFPRIME and argument name
c                  AfprimeVar, by determining the derivative of AF.
c
C      Calc_definevars(nvars) :: Nvars is returned, is # of active
c               user variables. This routine will display and ask for
c               values of all currently active user variables.
c
c       CALC_SHOWVARIS(IOUT,NDEC,QEXP)  -
C                       displays names and values of all current variables.
c
c       CALC_SHOWFUNKS(IOUT,NDEC,QEXP,AFUNK,ilen,iwp51,qdo_fmt) Displays the
c       Name,Argument,
c       and equation  of AFUNK, or all funks (afunk=' '). (IOUT IS IO CHANNEL
c       iwp51 flags "WP 5.1" style output, qdo_fmt true means
c       "use MATRIX statements in wp 5.1 style output.
c
c       CALC_ERROR -- The error handler.  You might want to change this
c                    if you do not want display of error messages.
c---------------------------------------------------------------------
c---------------------------------------------------------------------
C

c  Used  for color menus (print_at in strglib).  ANSI.SYS stuff used for bold,
c etc.
        include 'fgraph.fi'

C====================================================================
c main program:: STAND ALONE CALCULATOR

        program calc_prog
        parameter (len_history=20)
        integer exit,setvari,setfunk,showvari,showfunk,help,calc,FMTSET
        INTEGER OUTSET,Idefine,trigradians,trigdegrees,doderiv,ninset
        integer simplify,remark
        character *250 get_commandline,ascird_2a,ascird_2b
        CHARACTER *650 Pstrung,strung
        character *250 get_from_file

        logical qout

        character *24 charans
        character *1 alevel
        character *12 Argument          !used if setfunk
        character *62 Argument62          !used if setfunk
        character *12 Name              !used if setfunk or setvari
        character *12 smp,simpname          !for simplification
        Real *8 answer
        character *12 afun,dafun,dafunarg,CVT_FKEY,get_funk_argname

        logical qsave
        INTEGER TODO

        character *1 awp51
        integer iwp51

        character *1 a2,next_char,apost

        common /calc_cmerror/NERR

        character *80 A_history(len_History)
        common /cm_a_history/nat,ngot,a_history

        character *60 outname,openwrite,openread,inname
        common /cm_outname/outname,inname

        character *4 ablink,abold,ablock,anormal
        common /calc_screen/ablink,abold,ablock,anormal

        common /actions/
     1 exit,setvari,setfunk,showvari,showfunk,help,calc,FMTSET,OUTSET,
     1 INIT,Idefine,trigradians,trigdegrees,doderiv,ninset,SUMUP,
     1  remark,simplify

        logical qe,qdo_fmt
        integer ndec,infunk_string

        data exit,setvari,setfunk,showvari,showfunk,help,calc,FMTSET,
     1                  OUTSET,INIT,Idefine,trigradians,trigdegrees,
     1                  doderiv,ninset,SUMUP,remark,simplify
     1          /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18/
        data iout,ioutf/21,31/
        data qdo_fmt/.true./
        DATA QE/.FALSE./
        DATA NDEC/6/
        data infunk_string/40/
        data nerr/0/

        data outname/' '/               !io defaults
        data in_unit/5/
        data qout/.false./

        data nat,ngot/1,0/
        apost=char(39)

C BEGIN calc PROGRAM

        CALL calc_INIt          !initialize calculator internals

c Kommand= 0-user input, 1-from command line, 2-Esc, 3=help
 4      strung=GET_COMMANDLINE(' F1 for Help ',kommand)
        if (kommand.eq.2) stop ' bye.'
        if (kommand.eq.3) strung='HELP'    ! a hack, but it works
        Call PROCESS_strung(strung,in_unit)
        if (strung.eq.'?') strung='HELP'   ! see above
        if (strung.eq.' ') goto 200       ! nil string
        call push_ahistory(strung)       !save command

 10     continue

       nerr=0                   !reset global error flag

c strung is the user entry strung.
c     TODO is one of the /ACTIONS/ above.
c     Pstrung is the strung that should be processed by appropriate
c                    calc_ module
c     Name is either  user function name or variable name
c     Argument is user function argument
c     Ncode is VSAVE if /S option in variable definition.

        Call ACTION_TYPE(strung,                !sent
     1          Todo,Pstrung,Name,Argument62,Qsave,ilevel)  !recieved
        argument=argument62             !argument62 used in WP output


c Case ToDO ::  exit,setvari,setfunk,showvari,showfunk,help,calc,ETC.

        if (todo.eq.exit) then
           stop ' '
        ELSE IF (TODO.EQ.INIT) THEN
            CALL DO_calc_INIT(
     1          name,argument,qe,qdo_fmt,ndec,infunk_string)
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
       elseIF (TODO.EQ.SHOWVARI) THEN
           CALL CALC_SHOWVARIS(5,ndec,qe,pstrung)
           IF (QOUT)call CALC_shoWVARIS(IOUT,ndec,qe,pstrung)
           GOTO 200

        else if (todo.eq.showfunk) then
          if (.not.qsave) then
           call CALC_showfunks(6,ndec,qe,NAME,infunk_string,0,qdo_fmt)
              iwp51=0
          else
            iwp51=1
          endif

           IF (QOUT.and. (.not.qsave)) then     !generic output ?

               if (IN_UNIT.EQ.5.AND.name.ne.' ') then  !save 1 file,wp?
 902               awp51=ascird_2a(
     1        ' Select output style: W, C, or F1 for Help ',iarg)
                   call captaliz(awp51)

                   if (iarg.eq.27) then
                       iwp51=0
                   elseif (iarg.gt.255.or.awp51.eq.'?') then
                        call showfunk_help
                        goto 902
                   elseif (awp51.eq.'W') then
                        iwp51=1
                   else
                        iwp51=0
                   endif

               else                     !not single output
                   iwp51=0
               endif                    !display just 1 function
           endif                        !qout and qsave


c output to file
           if ((.not.qsave).and.qout) then
              call CALC_showfunks(IOUT,ndec,qe,NAME,4500,iwp51,qdo_fmt)
           elseif (qsave) then      !save function to its own file
                  OPEN(UNIT=ioutf,FILE=argument62,
     1                  ACCESS='SEQUENTIAL',err=9010,status='UNKNOWN')
                call CALC_showfunks(IOUTf,ndec,qe,NAME,4500,1,qdo_fmt)
                   CALL DOWN_LINE(0)
                   CALL DEL_TO_END
                   close(unit=ioutf)
           endif
           if (qsave) then
                PRINT *,' Function: ',name,
     1          ', written (for WP 5.1 equation editor) to ',argument
           elseif (qout.and.iwp51.eq.1) then
                PRINT *,' Function: ',name,
     1          ', written (for WP 5.1 equation editor) to ',outname
           elseif (qout.and.name.ne.' ') then
                   PRINT *,' Function: ',name,
     1               ', written as a CALC equation to ',outname
           endif
           goto 9020

 9010      print *,' Could not open file for WP output:',argument
 9020      qsave=.false.                !reset to false

        ELSEIF (TODO.EQ.SUMUP) THEN     !SUMMATE A LIST OF ITEMS
            CALL SETUP_Sumup(PSTRuNG)
            call down_line(0)
            if (in_unit.eq.5) call del_to_end
            if (pstrung.eq.' ') then
                call calc_error(' ! List of terms MUST follow /SUM ')
            else
                todo=calc
             endif

        elseif (todo.eq.remark) then    !do nothing
            call down_line(1)
            if (qout) then
                write(iout,8963)pstrung
 8963            format(1x,a)
            endif
            goto 200

        elseif (todo.eq.simplify) then  ! simplify, assume level=0
 8970       if (name.eq.' ') then
                   name=ascird_2a(
     1                ' Select function to simplify:',iarg)
                   if (iarg.gt.255.or.iarg.eq.27) then
                        goto 200
                   endif
                   call captaliz(name)
                   smp=name(1:min(len_trim(name),10))//'_S'
                   simpname=ascird_2a(
     1                 ' Name to give simplified function (ENTER='//
     1                 smp(1:len_trim(smp))//' :',iarg)
                   if (iarg.gt.255.or.iarg.eq.27) then
                        goto 200
                   elseif (simpname.eq.' ') then
                      simpname=smp
                   endif
                   call captaliz(simpname)
                   print *,
     1  ' Note: you can set simplification options with /INIT '
                   ilevel=0                     !use flags instead

            else

                simpname=argument

            endif               !else, set in action_

            call simplify_it(name,ilevel,simpname)

        ELSE IF (TODO.EQ.ninset) THEN
          if (pstrung.eq.' ') then
            if (in_unit.ne.5) then
                print *,' User Input of equations.'
                in_unit=5
            else
                in_unit=25
                close(unit=25)
                inname=openread(' Enter name for input file ?',25,nerr1)
                if (nerr1.ne.0) then
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
     1            ' Enter name for output file ?',21,'.LST',nerr1)
                if (nerr1.eq.0) then
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
          CALL calc_equation(Answer,Pstrung,nerr1)
           if (nerr1.gt.0) goto 200
           nn=messlen(Pstrung)
           Call calc_WRITENUM(ANSwer,CharAns,NDEC,QE,NLEN)
           if (nlen.eq.0) then
              nlen=24
              charans=' Cant print number.'
           end if
           if (kommand.eq.1) then
             call down_line(0)
             call del_to_end
           endif
           write(6,77)Pstrung(1:nn),charans(1:nlen)
           IF (QOUT)write(IOUT,77)Pstrung(1:nn),charans(1:nlen)
 77        format(1x,a,' == ',a)


        else if (todo.eq.setfunk) then          !DO NOT ANALYZE
           Call calc_loadfunction(Name,ARGUMENT,Pstrung,Nerr1)
           if (nerr1.gt.0 )goto 200
           nn=messlen(strung)
           IF (QOUT)write(IOUT,99)strung(1:nn)
           write(6,99)strung(1:nn)
 99        format(' Function :: ',a)

        else if (todo.eq.setvari) THEN
           Call calc_variable(Answer,NAME,Pstrung,nerr1,Qsave)
               if (nerr1.gt.0) goto 200
           nn=messlen(name)
           Call calc_WRITENUM(ANSwer,CharAns,NDEC,QE,NLEN)
           if (nlen.eq.0) then
              nlen=24
              charans=' Cant print number.'
           end if
           write(6,66)name(1:nn),charans(1:nlen)
           IF (QOUT)write(IOUT,66)name(1:nn),charans(1:nlen)
 66        format(1x,a,' :== ',a)

        else if (todo.eq.doderiv) then          !12/24/84 ADDITION:DERIVATIVE!!
           if (.not.qsave) then

              afun=ascird_2a(
     2          ' Enter NAME of function to take derivative of? ',kk)
               if (afun.eq. ' ') goto 200             ! cancel
               call captaliz(afun)
               dafun=ascird_2a(
     1    ' Enter NAME to give to DERIVATIVE (ENTER='
     2                    //afun(1:len_trim(afun))//apost//')?',kk)
               if (dafun.eq.' ') then
                 dafun=afun(1:len_trim(afun))//apost
               end if
               call find_func(afun,fnum)
               dafunarg=get_funk_argname(fnum)
               if (dafunarg.eq.' ') dafunarg='X'

               dafunarg=ascird_2a(
     1   ' Display using ARGRUMENT NAME (<ENTER>='//dafunarg//')?',kk)
               if (dafunarg.eq.' ')dafunarg='X'
           else
               afun=name
               dafun=argument
               call find_func(afun,fnum)
               dafunarg=get_funk_argname(fnum)
c              dafunarg='X'
           endif

c        print *,' afun=',afun
c        print *,' dafun=',dafun
c        print *,' dafunarg=',dafunarg
c        print *,nerr

           call calc_makederiv(afun,dafun,dafunarg,nerr1)  ! DO IT

c       print *,nerr
           if (nerr1.gt.0) goto 200
           if (.not.qsave) then
              print *,(' New function: '//dafun//' now defined.')
           else                         !display it
              print *,' d',afun, '/ d',dafunarg,' = ',dafun, ' ==> '
           call CALC_showfunks(6,ndec,qe,dafun,infunk_string,0,qdo_fmt)
              IF (QOUT)call CALC_showfunks(
     1                      IOUT,ndec,qe,dafun,4500,0,qdo_fmt)
           endif

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
           if (qout) then
                strung=ascird_2b(ablock//' ',iarg,pstrung)
           else
                strung=ascird_2b(' ',iarg,pstrung)
           endif

           pSTRUNG=CVT_FKEY(IARG)   !CHECK FOR /SWITCH proxy FKey
           IF (pstrung.ne.' ') then
               iarg=-1
               GOTO 2003
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
        elseif (strung.eq.'/DeBug') THEN   ! included as a hook.
           CALL CALC_DEBUG(qout,iout)
           GOTO 200
        elseif (iarg.gt.255)  then     !un used cursor command
                goto 200
        END IF
        call PROCESS_strung(strung,in_unit)
        if (strung.eq.' ') goto 200             !ignore blank lines
        if (strung.eq.'?') strung='HELP'

        GOTO 10

        END



c--------
c a help message
        subroutine showfunk_Help
                        WRITE(6,  901)
  901 FORMAT(  /
     1 ' There are 2 styles that CALC can use to display a function: '/
     1 '    1) C  -- Similar to how equations are entered in CALC ',
     1          ' (the default) '/
     1 '    2) W  -- Output is useable by the equation editor of',
     1                     ' WordPerfect 5.1'/
     1 '    The WP 5.1 style is used when you want to spruce up the d',
     1                     'isplay of your'/
     1 ' equation, such as by:                        1  '/
     1 '       using superscripts for powers, using  ÄÄÄ   for ',
     1                     'division,'/
     1 '                                              x '/
     1 '       and appropriately sizing parenthesis. '/)

                        WRITE(6,  903)
  903 FORMAT(
     1 '    To do this (examples are in parenthesis): '/
     1 '   i) Open an output file  (/OUT FX.LST). '/
     1 '  ii) Select a function using /FUN (/FUN FX). '/
     1 ' iii) Select W as the desired style. '/
     1 '     (Alternate method: instead of steps i-iii, enter',
     1       ' /FUN FX WP FX.LST)'/
     1 '  iv) Exit CALC and run WordPerfect, '/
     1 '         and Retrieve the output file  (WP FX.LST). '/
     1 '   v) Immediately Save it under a new name (FX.EQN). '/
     1 '  vi) Create an equation, and select the new file (FX.EQN) ',
     1                     'as the Filename.'//)



        return
        end

C===========================
C CONVERT FUNCTION KEY TO STRING
        CHARACTER *(*) function CVT_FKEY(IKEY)
        IF (IKEY.EQ.16128) THEN     !f5
           CVT_FKEY='/OUT '
        ELSEIF (IKEY.EQ.15360) THEN     !f2
           CVT_FKEY='/FUN '
        ELSEIF (IKEY.EQ.16384) THEN     !f6
           CVT_FKEY='/IN '
        ELSEIF (IKEY.EQ.16640.OR.IKEY.EQ.8192) THEN      !f7 ALT-D
           CVT_FKEY='/DERIV '
        ELSEIF (IKEY.EQ.16896.OR.IKEY.EQ.12800) THEN      !f8 ALT-S
           CVT_FKEY='/SUM '
        ELSEIF (IKEY.EQ.17152.OR.IKEY.EQ.7936) THEN      !f9 ALT-M
           CVT_FKEY='/SIMPLIFY '
        ELSEIF (IKEY.EQ.11520 .OR. IKEY.EQ.15616) THEN
           CVT_FKEY='EXIT'
        ELSE
           CVT_FKEY=' '
        ENDIF

        RETURN
        END

c
C----------------------------------------------------------
C GET CONTINUATION (IF requested)  AND CLEAN strung UP

        SUBROUTINE Process_strung(strung,in_unit)

        CHARACTER *(*) strung
        character *100 ascird_2b,atmp,get_from_file,atmp2
        character *12 cvt_fkey

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

              atmp2=CVT_FKEY(IARG)   !CHECK FOR /SWITCH proxy FKey
              if (atmp2.ne.' ') then
                 iarg=-1
                 goto 1005
              endif


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
     1          Todo,Pstrung,Name,Argument,Qsave,ilevel)  !returned to caller

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
        nlen=len_trim(strung)
        neq=index(strung,'=')
        NAME= ' '
        ARGUMENT=NAME

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


        if (strung(1:4).eq.'HELP' )then
           Todo=help

        ELSE IF (strung(1:5).EQ.'/INIT') THEN
           TODO=INIT
           call next_word(2,strung,name,iat)
           CALL CAPTALIZ(NAME)
           if (iat.ne.0) then
               call next_word(iat,strung,argument,ifoo)
               CALL CAPTALIZ(ARGUMENT)
           endif


        else if (strung(1:7).eq.'/DEFINE') then
           todo=Idefine

        else if (strung(1:6).eq.'/DERIV' ) THEN
           TODO=DODERIV
           qsave=.false.
           call next_word(2,strung,name,iat)
           CALL CAPTALIZ(NAME)
           if (iat.ne.0) then
               qsave=.true.
               call next_word(iat,strung,argument,ifoo)
               if (argument.eq.' ') then        !use f'
                   argument=name(1:min(len_trim(name),len(name)-1))
     1                                  //char(39)
               endif
               CALL CAPTALIZ(ARGUMENT)
           endif

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
           QSAVE=.FALSE.                        !AUTO SAVE OF FILE AS WP
           call next_word(2,strung,name,iat)
           CALL CAPTALIZ(NAME)
           if (iat.ne.0) then           !NAME OF FUNCTION
               call next_word(iat,strung,argument,ifoo)
               IF (IFOO.NE.0) THEN              !CHECK FOR WP
                  CALL CAPTALIZ(ARGUMENT)
                  IF (ARGUMENT(1:2).EQ.'WP') THEN     !SAVE AS WP
                     QSAVE=.TRUE.
                     call next_word(iFOO,strung,argument,iAT)  !WP FILE NAME
                     IF (IAT.NE.0) then
                         CALL CAPTALIZ(ARGUMENT)
                     else
                        argument='CALCWP51.LST'
                     endif
                  ENDIF                 !SAVE AS WP FILE
              ENDIF                     !CHECK FOR WP
           else
                name=' '
           ENDIF                         !NAME OF FUNCTION


        elseif (strung(1:4).eq.'/SUM') then
           todo=sumup
           ngrab=4
        elseif (strung(1:4).eq.'/REM') then
           todo=remark
           ngrab=4
        elseif (strung(1:5).eq.'/SIMP') then
           todo=simplify
           call next_word(2,strung,name,iat)   !get number or name
           argument=' '
           ilevel=0
           iat0=iat
           if (name.ne.' ') then                !else, user input
               ii1=ichar(name(1:1))-48
               if (ii1.ge.0.and.ii1.le.9) then  !level
                  ilevel=min(ii1,3)
                  call next_word(iat0,strung,name,iat)   !get name
                  iat0=iat
                  call next_word(iat0,strung,argument,iat)   !get arg
              else
                  ilevel=0
                  call next_word(iat0,strung,argument,iat)   !get arg
              endif
           endif

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
           ngrab2=NEXT_NON_CHAR(ngrab,STRUNG,nlen)  !next ' '
           aa=next_char(ngrab2,i6,strung)
           if (aa.eq.' ') then      !check for function name
                pstrung=' '
           else
                pstrung=strung(i6:nlen)
           endif
        endif

        return
        end


c--------------------------
c grab next word in a list. Istart is start location -- we look for
c first non-character after this location, then first character !
c in other words, we find next word after the word (or blank space)
c pointed to by istart
c Iat is location of last character of agot.
        subroutine next_word(istart,astring,agot,iat)

        implicit none
        integer istart,iat
        character *(*)astring,agot
        character *1 aa,next_char
        integer ngrab2,ngrab3,nlen,next_non_char

        nlen=len_trim(astring)
        agot=' '
        iat=0

        ngrab2=NEXT_NON_CHAR(istart,astring,nlen)
        if (ngrab2.gt.nlen) then
            return
        endif
        aa=next_char(ngrab2,ngrab3,astring)
        if (aa.eq.' ') then
           return
        endif
        iat=next_non_char(ngrab3,astring,nlen)-1
        agot=astring(ngrab3:iat)

        return
        end

cc========================================
C HELP FOR CALC PROGRAM

        SUBROUTINE  CALC_HELP

        character *60 outname,inname
        common /cm_outname/outname,inname

        character *4 ablink,abold,ablock,anormal
        common /calc_screen/ablink,abold,ablock,anormal

        character *76 amess


 557        FORMAT(/1x,a,
     1 ' Enter Equation to Calculate, or ESC to exit CALC ...',a/)

        ireturn=1
 2      continue
        if (ireturn.eq.0.or.ireturn.gt.7) then                  ! exited
            write(6,557)abold,anormal
            return
        else

           if (ireturn.ne.7) then
             IF (IRETURN.NE.1) THEN
                  write(amess,331)ireturn
 331             format(' CALC HELP.  Page = ',i3,' of 7')
             ELSE
                  write(amess,332)ireturn
 332       format(' CALC. FreeWare from DMH @ USDA\ERS\RTD (1992).',
     1             ' Page = ',i3,' of 7')
             endif
           else
               amess=' CALC HELP. Last Page '
           endif
           call print_at(1,1,14,2,amess,1)
           IF (IRETURN.EQ.1) THEN
              CALL PRINT_AT(3,20,15,6,
     1              'CALCULATOR PROGRAM: Introduction',0)
               print *,anormal
           elseIF (IRETURN.EQ.2) THEN
              CALL PRINT_AT(3,30,15,2,' Basics.',0)
           elseIF (IRETURN.EQ.3) THEN
              CALL PRINT_AT(3,30,15,2,' Functions.',0)
           elseIF (IRETURN.EQ.4) THEN
              CALL PRINT_AT(3,30,15,2,' Hints.',0)
           elseIF (IRETURN.EQ.5) THEN
              CALL PRINT_AT(3,30,15,2,' Switches. ',0)
           elseIF (IRETURN.EQ.6) THEN
              CALL PRINT_AT(3,30,15,2,' Switches ..  ',0)
           elseIF (IRETURN.EQ.7) THEN
              CALL PRINT_AT(3,30,15,2,' WP Equation Output  ',0)
           ENDIF
           GO TO (110,120,130,140,150,160,170),ireturn
        endif

 110   WRITE(6,    20)ablock,anormal
   20  FORMAT(1x,a,
     1 '  Features:',a/
     1 '          Up to 100 variables may be saved.'/
     1 '          Variables can be freely inserted into equations.'/
     1 '          25 intrinsic functions, such as SIN and LOG.'/
     1 '          Up to 34 User Definable functions allowed.'/
     1 '          Free Style input of numbers,',
     1               ' with PI and E built in.'/
     1 '          Output can be used by WP 5.1 Equation Editor '/
     1 '          Compute derivative of a user function, and simplify.'
     1 /  )
       WRITE(6,30)ablock,anormal,ablock,anormal,ablock,
     1   anormal,ablock,anormal,ablock,anormal,ablink,ablock,anormal
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


 120   WRITE(6,    40)ablock,anormal,ablock,anormal
   40  FORMAT(
     1  /1x,a,
     1 ' Number Entry:',a/5x,
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
     1 t5,'+ and -  ::  *, / and %  ::  ** or ^  ::  Functions ',
     2 ' ::  negative sign '/
     1 t15,' Notes: -3^2 = 9, % is the MOD operator (15 % 6 = 3 )')
       WRITE(6,    50)ablock,anormal
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


 130   WRITE(6,    60)ablock,anormal,ablock,anormal
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
     1 ' variable names (see above).'
     1   )
       WRITE(6,    70)
   70  FORMAT(t6,
     1 ' FUNCTIONNAME(ARGUMENTNAME)=EQUATION  eg: F1(X)=ABS(X*33-100)'
     2 '+2'/
     1 '         Note: The "function equation" can reference previous'
     2 'ly defined user'/t6,
     1 '         functions. When you use a user function, the argument'
     2 ' can'/t6,
     1 '         consist of any valid equation, including user functi'
     2 'ons.'/
     1   )

        CALL calc_PAUSEHELP(IRETURN)
        goto 2


 140   WRITE(6,    80)ablock,anormal,ablock,anormal,ablock,anormal
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
     1      t6,'  Up and Down arrow keys to recall (up to 20) prior',
     1                    ' equations.'/  )

        write(6,83)ablock,anormal,ablock,anormal
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
        write(6,831)ablock,anormal
 831    format(/
     1  1x,a,' Overflow::',a,
     1   ' CALC uses uses double precision, with a maximum value',
     1         ' of 1.0E+300'/
     1   ' permitted.  For functions, max args are:',
     1      ' SIN,etc=1.0e10, Factorial=170, Exp=750.')

        CALL calc_PAUSEHELP(IRETURN)
        goto 2

 150   WRITE(6,    90)ablock,anormal,ablock,anormal,ablock,anormal,
     1              ablock,anormal,ablock,anormal


   90  FORMAT(/t10,
     1 ' Certain commands are "switches" that set defaults, etc.:'/
     1  t10,'  ( [xx] means that xx is optional, "',a,
     2          'Fn',a,'" has same effect as "/switch" )'
     1 //
     1  1x,a,'    SWITCH   ',a,23x,a,' Description',a/
     1 4x,' /INIT            :: Initialize, and set various',
     1         ' CALC options'/
     1 4x,' /DEFINE          :: ReDefine ALL user-defined variables '/
     1 4x,' /VAR   [varname] :: List a  variable and its value.'/
     1 1x,a,'F2',a,1x,
     1  ' /FUN [function] :: Display a function (CALC or WP mode)'/
     1 1x,a,'F5',a,1x,' /OUT  [file.ext] :: Toggle -- send output',
     2         ' to terminal and a ".LST" file. ')
        if (outname.ne.' ') then
           WRITE(6,911)outname(1:35)
 911    FORMAT(18X,' Current output file: ',a)
        else
           WRITE(6,913)
 913       format(18X,' Currently: No Output File ')
        endif
       WRITE(6,  901)
     1     ablock,anormal,ablock,anormal,ablock,anormal,ablock,anormal
  901  FORMAT(
     1 1x,a,'F6',a,1x,' /IN  [file.ext]  ::',
     1              ' Read equations from an input file'/
     1 5x,' /RADIANS and  /DEGREES   :: Set default trig argument type'/
     1 1x,a,'F7',a,1x,' /DERIV [function] :: Compute derivative of a',
     1               '  user function.'/
     1 1x,a,'F8',a,1x,' /SUM  n1 .. nN   ::',
     1            ' Sum a list of numbers & terms '/
     1   25x, ' Example:  /SUM 4 5 EXP(2*PI) 20/SIND(45) 9.5 '/
     1 1x,a,'F9',a,1x,' /SIMPLIFY [function] :: Simplify a function'/)

        CALL calc_PAUSEHELP(IRETURN)
        goto 2

 160    write(6,93) ablock,anormal,ablock,anormal
 93       format(/t3,a,'/DERIV',a,
     1         ' will take a previously defined function and compute',
     1         ' its derivative;'
     1  /' saving the result under a seperate function.   This new ',
     1       ' function can'
     1  /' be accessed just like an ordinary user defined function. ',
     1 ' To view the'/
     1 ' derivative, use the /FUN switch (eg: /FUN F'').'/
     1 /1x,'NOTE: Simplification of the derivative (using the '
     1      a,'/SIMP',a,' switch) is recommended.'/)

        write(6,955)ablock,anormal,abold,anormal
 955    format(
     1  5x,'For /FUN and /VAR, if no argument is given,',
     1          ' all functions (or variables)'/
     1  ' are displayed;   For /IN, /OUT, /SIMPLIFY and /DERIV, ',
     1            'if no argument given,'/
     1  ' you will be prompted appropriately.'/
     1  t5,'When /OUT is active, the CALC prompt is ',a,' ?',a,
     1   ' (instead of ',a,' ?',a,')' )

       WRITE(6,    81)ablock,anormal,ablock,anormal
   81  FORMAT(/1x,a,
     1 ' Reading from File:',a,4x,
     1 ' To analyze several equations, you can read equations '/
     1  ' from a file, using the /IN switch:'/
     1  t35,' X=(45-6)/(PI^2) '/
     1  t14,a,' Example:',a,3x,t43,' F(Z)= (Z/10) X '/
     1  t35,' F(200) '/)


        CALL calc_PAUSEHELP(IRETURN)
        goto 2

 170    continue
       write(6,931) ablock,anormal,ablock,anormal
 931       format(/3x,
     1 'CALC can create output files that can be ',
     1                  'used by the WP 5.1 equation editor.'/
     1 ' The easiest way to do this is with the /FUN switch.'/
     1 ' For example, suppose you have a function ',a,'G1',a,':'//
     1 '   Step 1) From the CALC prompt enter: ',
     1     a,' /FUN G1 WP \MYDIR\G1.LST ',a/
     1 10x,'(a file named G1.LST is created on the \MYDIR directory)')

        write(6,941)ablock,anormal,ablock,anormal
 941     format(
     1 '   Step 2) Run WordPerfect 5.1, and invoke the ',
     1        a,'CVTCALC',a,' macro'/
     1 '   Step 3) Enter ',a,' \MYDIR\G1.LST ',a,
     1                    ' as the CALC file name.'/)
        write(6,951)
 951     format(
     1 ' Notes: '/
     1 '   i) The CVTCALC macro file (CVTCALC.WPM) must be installed'/
     1 '       (in your WP 5.1 macros subdirectory). '/
     1 '  ii) When the macro asks for a file, if you do not specify ',
     1      'a directory'/
     1 '      WordPerfect will look in it''s (default) documents',
     1           ' directory.'/)


        CALL calc_PAUSEHELP(IRETURN)
        goto 2



       RETURN
       END



        SUBROUTINE calc_PAUSEHELP(IRETURN)
        integer *2 dummy2,getchasm
        character *4 ablink,abold,ablock,anormal
        common /calc_screen/ablink,abold,ablock,anormal

        if (ireturn.eq.1) then
            print *,abold,
     1  '   ESC to exit HELP, PGDN for Next Page ',anormal

        elseif (ireturn.eq.6) then
            print *,abold,
     1  '   ESC to exit HELP, PGUP for Prior Page ',anormal

        else
            print *,abold,
     1  '  ESC to exit HELP, PgDn for Next Page, PgUp for Prior Page ',
     1  anormal
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

        subroutine DO_calc_INIT(
     1          arg1,arg2,qe,qdo_fmt,ndec,infunk_string)

        character *(*)arg1,arg2
        PARAMETER (maxnode=6000)
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

        common   /cm_simp/
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra
        integer
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra

        integer *2 i22,getchasm
        character *12 ans,ascird_2a,atmp
        character *1 cvtucase

        character *4 ablink,abold,ablock,anormal
        common /calc_screen/ablink,abold,ablock,anormal

        character *3 yno(2)

        logical qe,qdo_fmt
        integer ndec,infunk_string

        integer freenodes
        common /calc_freenode2/freenodes


        yno(1)='NO'
        yno(2)='YES'

        PROP_Free=100.0*FLOAT(FREENODES)/MAXNODE

 10     CONTINUE
        print *,' '
        PRINT *,'   % Of Work Spaces Currently Free: ',prop_free

        if (arg1.eq.' ') then

           ans=ascird_2a(
     1 ' Select /INIT option (ESC to Exit, F1 for help):',io)

       else
             ans=arg1
             io=13
        endif

       if (io.gt.255.or.ans.eq.' ') ans='?'
       if (io.eq.15104.or.io.eq.8960) then
                         ans='?'
       elseif (io.gt.255.or. io.eq.27) then
                return
       endif
       call captaliz(ans)
       if (ans.eq.'?') then
         write(6,8981)abold,anormal
 8981     format(3x,a,
     1  ' Initialization options: ',a/
     1  ' ALL      == Clear all variables and function '/
     1  ' FUNC     == Clear a user-selected function '/
     1  ' VAR      == Clear a user-selected variable '/
     1  ' FMT      == Specify format for writing results '/
     1  ' SIMP     == Set simplification options '/
     1  ' ESC key  == Return to ? prompt '/)
            goto 10
        endif

C ALL
        if (ans(1:3).eq.'ALL') then
            call zero_anstack
            call calc_init
            if (arg1.eq.' ') goto 10
            return
        endif

c FMT

        if (ans(1:3).eq.'FMT') then
           if (cvtucase(ascird_2a(
     1     ' Enter E to use exponential notation (eg 0.12E-02)?',kk))
     2                                             .EQ.'E') then
              qe=.true.
           else
              qe=.false.
           end if

           ndec=valimit(
     1     ' Enter # of digits to right of decimal point:',
     1              0.,5.,15.)

            infunk_string=valimit(
     1       ' Enter # of characters to use when displaying function:',
     1         1.,float(infunk_string),4500.)

            if (cvtucase(ascird_2a(
     1    ' Enter N to NOT use MATRIX commands'//
     2        ' when writing WP style functions:',kk)).ne.'N') then
                qdo_fmt=.true.
            else
                qdo_fmt=.false.
            endif


        endif
c FUNC
        if (ans(1:3).eq.'FUN') then
           if (nfunk.le.npermfunk) then
                print *,' No user functions to remove '
                if (arg1.eq.' ') goto 10
                return
           endif

           do ij=npermfunk+1,nfunk
                iaddr=funkaddress(ij-npermfunk)
                if (iaddr.eq.0) cycle
                atmp=funknames(ij)

c prompt user for each function
                if (arg2.eq.' ') then
                    call tree_stats(iaddr,
     1                        ntotnodes,maxdepth,nprim,nterminals)
                    write(6,578)atmp,ntotnodes
 578                format(7x,' Function: ',a,
     1                   ' (# nodes=',i5,') : Y to remove ? '\)
                    i22=0
                    do while(i22.le.0)
                       i22=getchasm()
                    enddo
                    if (i22.eq.27.or.i22.gt.255) goto 10
                    if (.not.(i22.eq.89.or.i22.eq.121)) THEN
                       call down_line(1)
                       cycle                  !else, get next one
                    ENDIF
                else                            !command line selection
                  if (atmp(1:len_trim(atmp)).ne.arg2(1:len_trim(arg2)))
     1                    cycle
                endif

c note that once a function is removed, it can not be unremoved
c (although space is not actually freed up until calc_garbageman is called)

                call remove_funk(atmp,istat)
                if (istat.eq.1) then
                    print *,' Impossible function name:',atmp
                    stop
                elseif (istat.eq.2) then
                   print *,'   Trying to Remove permanent function: ',
     1                         atmp
                   stop
                else
                     if (arg2.eq.' ') then
                          print *,' << REMOVED >> '
                     else
                          print *,' Function: ',atmp, ' removed.'
                      endif
                endif
            enddo
            call zero_anstack
            call calc_garbageman(1)   !force em immediately available
            if (arg1.eq.' ') goto 10
            return
        endif

        if (ans(1:3).eq.'VAR') then
           if (nvari.le.3) then
                 print *,' No user variables '
                 if (arg1.eq.' ') goto 10
                 return
          endif

          do ij=4,nvari

            atmp=varinames(ij)
            if (atmp.eq.' ') cycle
            if (arg2.ne.' ') Then
                  if (arg2(1:len_trim(arg2)).ne.atmp(1:len_trim(arg2)))
     1                          cycle
            else
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
            endif
c if here, user or command line seledcted
            call remove_variname(atmp,istat)
            if (istat.eq.1) then
                print *,'       No such variable: ',atmp
                if (arg1.eq.' ') goto 10
                 return
             elseif (istat.eq.2) then
                 print *,'     Can not remove permanent variable:',atmp
                 if (arg1.eq.' ') goto 10
                 return
             else
                     if (arg2.eq.' ') then
                          print *,' << REMOVED >> '
                     else
                          print *,' Variable: ',atmp, ' removed.'
                      endif
             endif
          enddo
          if (arg1.eq.' ') goto 10
          return
        endif

        if (ans(1:3).eq.'SIM') then            !set simplificaton flags
 20        if (arg2.eq.' ') then
           write(6,511)abold,anormal,
     1       yno(doiter+1),yno(doparen+1),yno(doneg+1),yno(domath+1),
     2           yno(dofunc+1),yno(dosimple+1),yno(doalgebra+1)
            endif
 511    format(/12x,a,' Current Simplification options: ',a/
     1      t5,'1) REPeat simplification until no improvement:',t64,a/
     1      t5,'2) REMove extra parenthesis [eg. ((X/4)) ==> (X/4) ]:',
     1             t64,a/
     1      t5,'3) MOVe - into [eg. -(3-5) ==> (-3+5) ]:',t64,a/
     1      t5,'4) Compute simple MATh [eg. (3+4) ==> 7 ]:',t64,a/
     1      t5,'5) Compute simple FUNctions [eg. (LN(3) ==> 1.09 ]:',
     1            t64,a/
     1      t5,'6) Do SIMple algebra [eg. (X*1) ==> X ]:',t64,a/
     1      t5,'7) Do complex ALGebra [eg. ((X*X)/X) ==> X ]:',t64,a/)


            if (arg2.eq.' ') then
               ans=ascird_2a(
     1         ' Toggle which option ? (1 to 7, ESC when done):',io)
            else
              ans=arg2
              io=13
            endif
           if (io.eq.27.or.io.gt.255) then
               if (arg1.eq.' ' ) goto 10
               return
           endif
           call captaliz(ans)
           if (ans.eq.'1' .or. ans(1:3).eq.'REP') then
                doiter=1-doiter
           elseif (ans.eq.'2'.or.ans(1:3).eq.'REM') then
                doparen=1-doparen
           elseif (ans.eq.'3'.or.ans(1:3).eq.'MOV') then
                doneg=1-doneg
           elseif (ans.eq.'4'.or.ans(1:3).eq.'MAT') then
                domath=1-domath
           elseif (ans.eq.'5'.or.ans(1:3).eq.'FUN') then
                dofunc=1-dofunc
           elseif (ans.eq.'6'.or.ans(1:3).eq.'SIM') then
                dosimple=1-dosimple
           elseif (ans.eq.'7'.or.ans(1:3).eq.'ALG' ) then
                doalgebra=1-doalgebra
           else
                if (arg2.eq.' ') then
                    goto 10
                endif
           endif
           if (arg2.eq.' ') then
               goto 20
           endif

          if (arg1.eq.' ') goto 10
           write(6,511)abold,anormal,
     1       yno(doiter+1),yno(doparen+1),yno(doneg+1),yno(domath+1),
     2           yno(dofunc+1),yno(dosimple+1),yno(doalgebra+1)
          return
        endif


        end
