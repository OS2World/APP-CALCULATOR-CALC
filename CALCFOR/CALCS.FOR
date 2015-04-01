c @for the public domain.
c====================================================================
C  A CALCULATOR PROGRAM -- small version of CALC.
C
C WRITTEN 2/84 BY DMH at Brandeis for VAX.
c Modified for DOS on PC, 1/92 by DMH at ERS.
c
C
c  NOTE:: for a fairly complete "user level" description of this program,
c  see the subroutine "Calc_Help".
c
C CALC.EXE can be run in 2 ways
c
c 1) Normal method.
c   Simply type  CLC (from the DOS prompt) and wait for the prompt.
c
c  2) "Inline" method.
c
C
C      Usage::
c  1)    C:>CLC Equation.  The answer will then be typed,
c           and then you are returned to DOS.
c             eg;   C:>CLC 9-(4^2*2)
C               9-(4^2*2)=-23.00000         !output
c  2)    C:>CLC
c           ?                         !prompt from CLC

C====================================================================
c main program:: STAND ALONE CALCULATOR

        program calc_prog

        character *80 get_commandline,ascird_2
        CHARACTER *80 Pstrung,strung,openwrite,outname

        LOGICAL QE

        logical qout

        character *24 charans
        character *1 cvtucase,alevel,aa
        character *12 Argument          !used if setfunk
        character *12 Name              !used if setfunk or setvari
        Real *8 answer

        logical qsave
        INTEGER TODO

        character *1 a2,next_char,apost


        integer exit,setvari,showvari,help,calc,FMTSET,INIT,outset
        common /actions/
     1  exit,setvari,showvari,help,calc,FMTSET,INIT,outset
        data
     1 exit,setvari,showvari,help,calc,FMTSET,INIT,outset
     1 /1,2,3,4,5,6,7,8/

        data iout/21/
        DATA NDEC/6/
        DATA QE/.FALSE./

        data outname/' '/               !io defaults
        data in_unit/5/
        data qout/.false./

        data nat,ngot/1,0/
        apost=char(39)

C BEGIN calc PROGRAM

        CALL calc_INIt0(1)          !initialize calculator

c Kommand= 0-user input, 1-from command line, 2-Esc, 3=help
 4      strung=GET_COMMANDLINE(' HELP for Help ',kommand)

        if (kommand.eq.2) stop ' bye.'
        if (kommand.eq.3) strung='HELP'    ! a hack, but it works
        Call captaliz(strung)
        if (strung.eq.'?') strung='HELP'   ! see above
        if (strung.eq.' ') goto 200       ! nil string

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
        else if (todo.eq.help) then
            call Calc_Help
        ELSE IF (TODO.EQ.FMTSET) THEN
           aa=ascird_2(
     1     ' Enter E to use exponential notation (eg 0.12E-02)?',kk)
           call captaliz(aa)
          if (aa.eq.'E') then
              qe=.true.
           else
              qe=.false.
           end if

           ndec=valimit(
     1              ' Enter # of digits to right of decimal point:',
     1              0.,5.,15.)


       elseIF (TODO.EQ.SHOWVARI) THEN
           CALL CALC_SHOWVARIS(5,ndec,qe,' ')
           IF (QOUT)call CALC_shoWVARIS(IOUT,ndec,qe,' ')
           GOTO 200


        ELSE IF (TODO.EQ.OUTSET) THEN
            IF (QOUT) THEN
               CLOSE(UNIT=21)
               QOUT=.FALSE.
               print *,' Closing output file :',outname
            endif
            outname=openwrite(' Enter name for output file ?',21,nerr)
            if (nerr.eq.0) then
                   qout=.true.
            else
                   print *,' Results will NOT be saved to output file'
            endif

        END IF             ! switches

c  else ,here to figure out equation OR to define a function OR to define a variable

c Case TODO == CALC,SETVARI, or SETFUNK (or SUMUP after call to SETUP_SUMUP)

        IF (ToDo.eq.Calc) then          !analyze an equation, type a value
          CALL calc_equation(Answer,Pstrung,nerr)
           if (nerr.gt.0) goto 200
           nn=len_trim(Pstrung)
           Call calc_WRITENUM(ANSwer,CharAns,NDEC,QE,NLEN)
           if (nlen.eq.0) then
              nlen=24
              charans=' Cant print number.'
           end if
           write(6,77)Pstrung(1:nn),charans(1:nlen)
           IF (QOUT)write(IOUT,77)Pstrung(1:nn),charans(1:nlen)
 77        format(1x,a,' == ',a)


        else if (todo.eq.setvari) THEN
           Call calc_variable(Answer,NAME,Pstrung,nerr,Qsave)   !Qsave=TRUE if /S option
           if (nerr.gt.0) goto 200
           nn=len_trim(name)
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
     1                ' run CLC without an argument.'
              stop ' '
        endif


 2003      strung=ascird_2(' ',iarg)


        if (iarg.gt.255) then
          call calc_help
          goto 200
        elseif (iarg.eq.27) then
            stop ' Exiting CLC '
        END IF
        call captaliz(strung)
        if (strung.eq.' ') goto 200             !ignore blank lines
        if (strung.eq.'?') strung='HELP'

        GOTO 10

        END


C----------------------------------------------------------
c See what kind of action to under take, and return some values
c  depending on action.
c  At this point we expect a "processed" strung.

        Subroutine ACTION_TYPE(strung,          !from caller
     1          Todo,Pstrung,Name,Argument,Qsave)  !returned to caller


        Character *(*) strung,pstrung,name,argument

        Integer Todo
        Logical Qsave
        character *1 aa,next_char

        integer exit,setvari,showvari,help,calc,FMTSET,INIT,outset
        common /actions/
     1  exit,setvari,showvari,help,calc,FMTSET,INIT,outset

c set some pointers
        ngrab=0
        nlen=len_trim(strung)
        neq=index(strung,'=')

        if (neq.gt.1) then
           nparen=index(strung(1:neq),'(')
           IF (NPAREN.gt.0) THEN
               call Calc_Error(' No User Functions allowed')
               return
           END IF
        end if

c now,see what kind of action


        if (strung.eq.'HELP' )then
           Todo=help
        else if (strung(1:4).eq.'/OUT') then
           TODO=OUTSET
        else if (strung(1:4).eq.'/VAR') then
           Todo=showvari
        else if (strung(1:4).eq.'/FMT') then
           todo=fmtset
        else if (strung.eq.'EXIT'.or.strung.eq.'QUIT') then
           Todo=exit
        else if (neq.le.0) then
           Todo=calc
           Pstrung=strung
        else if (neq.gt.0)  then                     ! here to set vari
           Name=strung(1:neq-1)         !the variable
           Pstrung=strung(NEQ+1:LEN(strung))    !strung SHOULD BE THE EQUATION ONLY
           IF (strung(Neq+1:Neq+2).EQ.'/S') THEN        !SAVE VALUE OF MOST RECENT EQUATONS
              Qsave=.TRUE.
           ELSE                                 !SAVE FOLLOWING EQUATION
             Qsave=.False.
           END IF
           Todo=SetVari

        end if


        return
        end


c----------------------------------
        SUBROUTINE calc_init0(ivar)


        PARAMETER (maxnode=250)
        parameter(NPERMFUNK=25)
        PARAMETER (MAXFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)
        parameter (MAXVARI=10)

C BASIC DATA STRUCTURE
C    POINTERS   To Left , Right
C Note a son is attached to either the either the LS (left) or RS (right)
C  node of its parent
        INTEGER LS,RS
        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        integer nil2                    !needed by calc_routines
        common /calc_cmnil/nil2

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR


c recursion, etc. flags
        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI

        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        INTEGER BP,EP
        common /calc_PTYPE/BP,EP


c cell allocation pointer
        INTEGER NEXTFREE
        common /calc_FREENODE/NEXTFREE

c user defined variable structure
        CHARACTER *12 VARINAMES(MAXVARI)
        real *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        CHARACTER *12 FUNKNAMES(MAXFUNK)
        common /calc_FUNCTIONS/FUNKNAMES

c nodes needed by deriv routines
        real *8 v8

        logical qradians                !radians or degrees flag
        common /trigtype/qradians

        DATA NVARI/3/           !E and PI are predefined (and first vari is
                                !special "function argument index")
        DATA BP,EP/1,2/
        DATA EQEND,VALU,OPER,PAREN,FUNK,VARI/6,1,2,3,4,5/
        data nil2/0/
        DATA NIL,LS,RS/0,1,2/
        DATA IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP/1,2,3,4,5,6,7/
        data varivalues(1) /0.0/
        data varinames(1)/' '/
        data varivalues(2)/3.1415926535/
        data varivalues(3)/2.718281828459045/
        data varinames(2)/'PI'/
        data varinames(3)/'E'/

        DATA FUNKNAMES/'SIND','COSD','TAND','SINR','COSR',  'TANR',
     1  'ARCSIND','ARCCOSD','ARCTAND','ARCSINR','ARCCOSR','ARCTANR',
     1                 'ABS','EXP','EXP10','LOG','LOG10',
     2                 'INT','SQRT','RADTODEG','DEGTORAD',
     2                 'SINH','COSH','TANH','FACT'/




        qradians=.false.

        if (ivar.eq.1) then
           nvari=3                 !3 is number of permanent variables
        endif

        DO IJ=1,MAXNODE-1
           NODE(IJ,rs)=IJ+1
           VALUE(IJ)=0.
           OPR(IJ)=NIL
        END DO
        OPR(MAXNODE)=NIL
        VALUE(MAXNODE)=0.
        NODE(MAXNODE,RS)=NIL

        NEXTFREE=1

        RETURN
        END

c-----------------------------------
        subroutine calc_help

         WRITE(6,  100)
  100 FORMAT(
     1 ' CLC. FreeWare from DMH @ USDA\ERS\RTD (1/92).'//
     1 '                   Calculator Program (small version). '/
     1  /
     1 ' Example :   (8*7) - (9^4)/PI          (PI and E are hard-cod',
     1                     'ed)'/
     1  /
     1 ' 10 Variables can be retained: eg;  A=EXP(2.3)/100 '/
     1 '                                    A/(33+5) '/
     1 ' 25 Functions are available: '/
     1 '   SIND      COSD      TAND     SINR  COSR  TANR   (abcR  for',
     1                     ' radians)'/
     1 '   ARCSIND   ARCCOSD   ARCTAND                     (abcdefR f',
     1                     'or radians)'/
     1 '   RADTODEG  DEGTORAD  SINH     COSH  TANH    FACT ')


         WRITE(6,  102)
  102 FORMAT(
     1 '   ABS   EXP   EXP10     LOG (or LN)    LOG10   INT   SQRT '/
     1  /
     1 ' Precedence of Math operators: -  & + , / * and  % (mod), ^ (',
     1                     'power), functions'/
     1  /
     1 ' Special commands: '/
     1 ' /OUT  -- send results to output file. '/
     1 ' /FMT  -- change display format. '/
     1 ' /VAR  -- display values of current variables. '/
     1  /
     1  )
        return
        end

c--------------------------------------------
c
C Display prompt, then get a word.
c Differs from ASCIRD in two ways
c  1) If non-ascii character (such as arrow key, home key, etc.)
c     is hit, then return a numeric value for that character in the iarg
c     argument
c
c Thus, return word as function return, and return character value in iarg
c
c  If normal entry, then iarg equals the ascii value of first character
c  (which is < 256)
c  If non-ascii character, then function return is set to ""


        character *(*) function ascird_2(prompt,iarg)
        character *(*) prompt
        character *80  aword
        character *1 a1

        integer *2 ain2,getchasm
        character *4 a4,a4a

        a4(1:1)=char(27)
           a4(2:2)='['
           a4(3:3)='1'
           a4(4:4)='m'
           a4a=a4
        a4a(3:3)='0'

 42     Write(6,44)a4,prompt,a4a
 44     format('$',a4,a,'?',a4,' '\)

        ithlet=0
        aword=" "
        iarg=0

c keep entering characters
        do ij=1,10000
c get 1 character
            ain2=0
            do while (ain2.le.0)
                ain2=getchasm()
            enddo
c Is it non-ascii character ?
            if (ain2.eq.21248) ain2=8           ! convert DEL to BACKSPACE
            if (ain2.eq.9) ain2=32      !convert tab to space

            if (ain2.gt.255) then
                a1=char(13)
                print *,a1
                ascird_2=AWORD
                iarg=ain2
                return
            endif
c ENTER CR or LINEFEED hit ?
            if (ain2.eq.10 .or. ain2.eq.13) then
                ascird_2=aword
                a1=char(13)
                print *,a1
                return
            elseif (ain2.eq.27) then
                ascird_2=aword
                if (iarg.eq.0)iarg=27
                print *,' '
                return
            endif


c ^E
           if (ain2.eq.5) then
                call godos('ZIPFIP')
                goto 42
           endif

c BACKSPACE
            if (ain2.eq.8) then
               if (ithlet.gt.0) then
                   aword(ithlet:ithlet)=" "
                   ithlet=ithlet-1
                   a1=char(8)
                   write(6,411)a1
                   a1=" "
                   write(6,411)a1
                   a1=char(8)
                   write(6,411)a1
                   cycle
              ELSEIF (ITHLET.EQ.0) THEN
                 A1=CHAR(7)
                 write(6,411)a1
                 CYCLE
              endif
            endif

C else, keep accepting characters
            if (ithlet.lt.80) ithlet=ithlet+1
            if (ithlet.eq.1) iarg=ain2
            a1=CHAR(ain2)
            write(6,411)a1
            aword(ithlet:ithlet)=a1

 411               format(a1\)

        end do

        end

c====================================
c Get arguments from the command line.  If none given, then
c display prompt and get from user.

        character *(*) function get_commandline(prompt,kommand)


        character *(*)prompt

        integer *2 i2a,i2b
        character *70 aline,choice,ascird_2

         nn=nargs()
         if (nn.lt.2) goto 20

        aline=' '

        i2a=0
        kommand=1
        iat=1

        do ij=2,nn
           i2a=i2a+1
           call getarg(i2a,choice,i2b)
           if (i2b.lt.0) then
                 print *, ' Can not read Equation '
                 exit
           endif
           aline(iat+1:len(choice))=choice(1:len_trim(choice))
           iat=len_trim(aline)
        enddo

        choice=aline


 20       if (i2b.le.0.or.nn.lt.2) then          ! get from user
               kommand=0
 10            choice=ascird_2(prompt,iarg)
               if (iarg.gt.255) then
                  kommand=3
                  choice=' '
               elseif (iarg.eq.27) then
                   choice=' '
                   kommand=2
              endif
        endif


        call captaliz(choice)
        get_commandline=choice

        return

        end

c=============================================
c---------------------------
c real *8 version of val
        REAL *8 FUNCTION VAL8(ANS,iERR)

        real *8 vv
        CHARACTER *50 TMP
        CHARACTER *(*) ANS

        ierr=0

 661    FORMAT(E20.10)
 66        format(f20.10)

        call captaliz(ans)
        IF (ANS.EQ.' ')THEN
           VAL8=0.0
           RETURN
        ELSEIF (ANS.EQ.'PI') THEN
           VAL8=3.1415926535
           RETURN
        ELSEIF (ANS.EQ.'E') THEN
           VAL8=dexp(1.0)
           RETURN
        ENDIF
        IF (INDEX(ANS,'.').GT.0) THEN
           READ(ANS,66,ERR=200)VV
           VAL8=vv
           return
        ELSE
           IE2=INDEX(ANS,'E')
           IF (IE2.GT.1) THEN      ! E notation
               TMP=ANS(1:IE2-1)//'.'//ANS(IE2:LEN_TRIM(ANS))
               READ(tmp(1:LEN_TRIM(TMP)),661,ERR=200)VV
            ELse
                ifoo=len_trim(ans)
                TMP=ANS
                TMP(IFOO+1:IFOO+1)='.'
                READ(TMP(1:IFOO+1),66,ERR=200)VV
            ENDIF
            VAL8=vv
            RETURN

         endif


 200    CONTINUE
        VAL8=0.0
        ierr=1
        RETURN

        END

C---------------------------
C --- GIVEN A STRING, RETURNS REAL NUMBER VALUE, OR SETS ERROR FLAG

        REAL FUNCTION val(ANS,iERR)

        CHARACTER *50 TMP
        CHARACTER *(*) ANS

        ierr=0

 661    FORMAT(E16.7)
 66        format(f16.7)

        call captaliz(ans)
        IF (ANS.EQ.' ')THEN
           val=0.0
           RETURN
        ELSEIF (ANS.EQ.'PI') THEN
           val=3.1415926535
           RETURN
        ELSEIF (ANS.EQ.'E') THEN
           val=exp(1.0)
           RETURN
        ENDIF
        IF (INDEX(ANS,'.').GT.0) THEN
           READ(ANS,66,ERR=200)VV
           val=vv
           return
        ELSE
           IE2=INDEX(ANS,'E')
           IF (IE2.GT.1) THEN      ! E notation
               TMP=ANS(1:IE2-1)//'.'//ANS(IE2:LEN_TRIM(ANS))
               READ(tmp(1:LEN_TRIM(TMP)),661,ERR=200)VV
            ELse
                ifoo=len_trim(ans)
                TMP=ANS
                TMP(IFOO+1:IFOO+1)='.'
                READ(TMP(1:IFOO+1),66,ERR=200)VV
            ENDIF
            val=vv
            RETURN

         endif


 200    CONTINUE
        val=0.0
        ierr=1
        RETURN

        END
C---------------------------------------------------------------
c read a value between limits.


        FUNCTION VALIMIT(AMESS,aLOW,DEFVAL,alimit)

        character *(*) amess
        CHARACTER *40 ANS

 42     continue
           Write(6,44)AMESS,defval
 44     format(' ',a,'(<CR>=',f11.4, ') ?'\)

 10     read(5,45)ANS
 45     FORMAT(A40)

        if (ichar(ans).eq.5) then
           call godos('CALC')
           goto 42
        endif

        if (ans.eq.'?') then
              write(6,678)alow,alimit
 678     format(
     1   ' Enter a value between',f15.7,' and ',f15.7/
     1   '    Special values are: ',
     1        'E= 2.7182   PI=3.1413 '/
     1   '    Any entry format may be used (real,integer,exponential)'/
     1   '    Note: For temporary exit to DOS, Enter ^E ' /
     1   '$ REENTER::')
            goto 10
        endif

        IF (ANS.EQ.' ')THEN
           valimit=defval
           RETURN
        ENDIF

        TT=VAL(ANS,iERR)
        IF (iERR.ne.0) THEN
            WRITE(6,66)
 66     FORMAT('$ %BAD INPUT -- REENTER :: ')
             GOTO 10
        ENDIF

        aVAL=TT

        IF (AVAL.GT.Alimit.OR.AVAL.LT.ALOW) THEN
          write(6,555)ALOW,ALIMIT,DEFVAL
          GOTO 10
        else
           VALIMIT=aval
           RETURN
        END IF

 555    FORMAT(' LOWEST & HIGHEST VALUES ALLOWED ARE=',
     1          F15.7,',',F15.7/
     1         ' :: PLEASE REENTER  (<CR>=',F15.7,') ? ')

        end



c=================
c temp exit to DOS
        subroutine godos(thisprog)

        character *(*) thisprog

        print *,' '
        print *,' Enter one DOS command.'
        print *,'   (Hint: for temporary exit to DOS, enter COMMAND,'
        print *,
     1       '        and then enter EXIT to return to ',thisprog,')'

        pause ' ... ? '
        print *,' '
        return
        end

c======================================
C FUNCTION TO CONVERT FROM lower TO upper CASE
        subroutine captaliz(sent)

        character *(*)sent
        character *1 char
        character *100 tmp

        tmp=' '
        ij=1
        do 100 ij0=1,len_trim(sent)
            let=ichar(sent(ij0:IJ0))
            if (let.eq.44.or.let.eq.59.or.let.eq.58.or.let.eq.32) then
                cycle           !skip filler characters
            elseif (ij.gt.100) then
                exit                    !all filled up
            endif
             tmp(ij:ij)=char(let)
            if (let.ge.97.and.let.le.122) tmp(ij:ij)=char(let-32)
            if (let.eq.91.or.let.eq.123)  tmp(ij:ij)=char(40)
            if (let.eq.93.or.let.eq.125)  tmp(ij:ij)=char(41)
            ij=ij+1
 100    continue
        sent=tmp
        return
        end


C-----------------------------------------------------------------
C Routine to open a sequential data file, with 'UNKNOWN' status,
C
        character *(*) function  OPENWRITE(PROMPT,UNITNUMBER,NERR)


        CHARACTER *(*) PROMPT
        INTEGER UNITNUMBER

        character *60 ascird_2,filename

        filename=ascird_2(prompt,iarg)
        if (iarg.eq.27.or.iarg.gt.255) then
          goto 900
        elseif (filename.eq.' ') then
          goto 900
        endif

        OPEN(UNIT=UNITNUMBER,FILE=FILENAME,ACCESS='SEQUENTIAL',err=900,
     1          status='UNKNOWN')

        openwrite=filename
        NERR=0
        RETURN

 900    continue
        openwrite=' '
        NERR=1
        RETURN

        END


c===================================================
c move down a line, but no characters
        subroutine down_line(ido)
        character *1 a
        a=char(13)
        write(6,511)a
 511    format(a\)
        ij=1
        a=char(10)
        do while (ij.le.ido)
          write(6,511)a
          ij=ij+1
        enddo

        return
        end

c======================================================================
c======================================================================
c=[[[[[[[[[[[[[[[  library stuff ....

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

c                               !if false, undefined variables are fatal errors.

        NERR=0                  !RESET ERROR COUNTER
        NERR2=1                         !ASSUME ERROR

        PSTRING1=PSTRING
        call captaliz(pstring1)

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
        call captaliz(pstring1)
        VNAME1=VNAME
        CALL CAptaliz(VNAME1)


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



c==========================================================

C ROUTINE TO DISPLAY ALL CURRENT VARIABLES

        SUBROUTINE CALC_SHOWVARIS(IOUT,ndec,qe,ANAME)

        PARAMETER(MAXVARI=10)

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

c=========================================
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
        CALL CALC_init0(0)
        call zero_anstack

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

        PARAMETER (maxnode=250)

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

        PARAMETER (maxelem=100)

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

        PARAMETER (maxelem=100)
        PARAMETER (maxnode=250)

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


c       call load_funkargname(userarg)  !no longer used (no functions)

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
        PARAMETER (maxelem=100)
        PARAMETER (maxnode=250)

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


        INTEGER OPTYPE
        CHARACTER *(*) STRING

        CHARACTER *12 HOLD
        CHARACTER *1 ACHAR,apost


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
        IF (AVAL.EQ.0) THEN          !UNDEFINED
            CALL Calc_Error(' ! REFERENCING UNDEFINED VARIABLE ')
            RETURN
        ELSE                            !DEFINED OR UNDEFINED
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


c=======================================================================


C
C  ANALYZE TREE
C  Value Returned in Function.  Base is Base address of tree to analyze.
C Basic Procedure: NOTE:: LS and RS can hold either value or address depending
c                       on where we are in tree processing.
c       0) calc_Push a nil node to act as brake
c       1) Get first  node from tree.(fill LS,RS,and CurrentNode)
c       2) START LOOP:: NOTE. First node should always match a) below.
c               a) If current node is IOPR or FUNK (no LS ) then
c                   2) calc_Push (LS,LsNone,CurrentNode,RS)
c                   3) Get RS
c                   4) GOTO 2
c               b)  IF node is VALU  or VARI, then
c                   0) If VARI, get current variables current value.
c                       1)Store Value (from VALU or from VARI) into HOLDVALUE
c                   2)POP2(VLS,LsStat,NewNode,RS)  (we have a HOLDVALUE)
c                       a) IF NewNode = Nil, then we are done. RETURN HOLDVALUE
c                   3) If LsStat is LsNotDone (left not done of a Math Oper)
c                       b) calc_Push (HOLDVALUE,LsDone,address,RS) (left done)
c                       c) Get RS node
c                       d) GOTO 2
c                   4) ELSE , 1 of 3 returns to RS.
c                       a) Get OPTYPE and OPVALUE from NewNode
c                       b1) If LsStat=LsDone, we are returning to a
c                       math OPER. We have left part of operation (VLS).
c                           1) Call DoMath with optype,opvalue,VLS, and Holdvalue
c                           2) Goto 2, with Holdvalue = to results of DoMath
c                       b2) If LsStat=LsNone; we are returning to either a
c                          function (PERM or USER) or to a ( ).
c                         1)IF user function, we want to start analyzing
c                           the particular user function tree. We do this
c                           in a recursive fashion.  This entails calc_Pushing
c                           a LsFunk frame and "CURRENTNODE" equal to
c                           base of function subtree. We also must load
c                           the value from RS into the argument stack.
c                           When we come back from user function, it will
c                           be signaled by the LsFunk value of LsStat.
c                         2) IF not userfunction,
c                           Pass LS,HOLDVALUE , OPTYPE and OPVALUE to
c                           Math module.
c                             a) HOLDVALUE gets results of Math module.
c                             d) GOTO 2.
c                        b3) If LsSTAT=LsFunk, we are returning from
c                            user function analysis.
c                            1) HoldValue is the result of user function
c                               analysis, hence we just want to do an
c                               IOPR type operation.  Call DoMath with
c                               OpType and Opvalue, and with a flag
c                               instructing DOMATH to '
c                               a)Perform an IOPR opreation.
c                               b) calc_pop an argument from argument stack.
c                            2) DoMath returns Holdvalue:: Go to 2
c=====================================================================

        real *8 FUnction ANALYZE_TREE(Base,farg)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)
        PARAMETER (maxstak2=150)
        parameter( NPERMFUNK=25)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
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

        integer *2 stknode(MAXSTAK2)
        Integer *2 LsStats(MAXSTAK2)
        REAL *8 VALL(MAXSTAK2)          !holds value
        REAL VALR(MAXSTAK2)             !could be integer *2
        INTEGER NSTACK2
        common /calc_STACK2/NSTACK2,VALL,LsStats,STKNODE,VALR

        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        real *8 vls,holdvalue
        real *8 calc_math,dov1          !dov1 used to hold flag
        real *8 opvalue,calc_get_varivalue

        common /calc_cmerror/nerr


        Integer Base,farg                       !the base of tree to be analyzed
                                        !and the location of function argument
                                        !value (if nil , NO function argument )

        Integer Optype,CurrentNode,NewNode
        real *8 nil8

        Data LsNotdone,LsDone,LsNone,Lsfunk/1,2,3,4/

c zero analysis stack
        CALL ZERO_ANSTACK


c  calc_Push a nil to act as a brake
        nil8=nil
        Call calc_PushS(nil8,LsNone,nil,float(nil))

c Get First Node.

        CurrentNode=Base

c  THIS IS BASE OF ITERATIVE "RECURSION"  (COME HERE FOR USERFUNCTION)
 90     Call GET_NodeINFO(CURRENTNODE,OPTYPE,OPVALUE,INLS,INRS)
        flagopv=set_flagval(optype,opvalue)


 100    Continue
        If (nerr.gt.0) return                   !check for error

c Case 1 :: IOPR or FUNK. There is no need to differentiate between
c   user functions and permanent functions at this point "on the way down".
c   Regardless of type, we still need to analyze the subtree that will
c   yield the value of the argument to be used by the function (either
c   user or perm).

        IF (optype.eq.funk.or.(optype.eq.OPER.and.
     1          abs(flagopv).eq.IOPR)) then   !use abs (watch out for neg functions)
            Vls=0                       !signal used by user functions
            Vrs=Inrs                    !Address of function or IOPR argument
            Call calc_PushS(Vls,LsNone,CurrentNode,Vrs)
            CurrentNode=INRS            !now analyze RS argument.
            Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
            flagopv=set_flagval(optype,opvalue)
            goto 100
        endif

C Case 2:: VALU OR VARI

        If (OpType.eq.VALU.or.OPTYPE.eq.VARI) then
           IF (OpType.eq.VARI) then
                HOLDVALUE=calc_GET_VariValue(flagopv)
           else
                HOLDVALUE=OpValue               !*8 value
           endif
           goto 200
        endif


c  CASE 3:: we have a math oper. So, we need to get both left and right nodes.
c  Record this "state" on Stack.
        vls=inls
        vrs=inrs
        Call calc_PushS(vls,LsNotDone,CurrentNode,vrs)  !save left and right nodes
        CurrentNode=InLs                        !start with left side

        Call Get_NodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
        flagopv=set_flagval(optype,opvalue)

        Goto 100



C HERE After Succesful SUBTREE analysis, or if Node is simple (valu or VARI)
c At this point, HoldValue contains the value of either a LS or RS subtree.

 200       continue                             !jump here if returning from subtree analysis
           if (nerr.gt.0) return                !check for error
           Call calc_popS(VLS,LsStat,NewNode,VRS)  !vrs only used to hold address

           IF (NewNode.eq.Nil) Then             !equation all done
             Analyze_TRee=HoldValue
             RETURN                             !so Return
           endif

           IF (LsStat.eq.LsNotDone) then                !returning to Left node
              Call calc_PushS(Holdvalue,LsDone,NewNode,Vrs)  !ready to get RS
              CurrentNode=Vrs

              Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
              flagopv=set_flagval(optype,opvalue)
              goto 100
           else                                 !returning to Right Node
              Call Get_NodeInfo(NewNode,OPtype,OPvalue,InLS,InRS)  !inls and inrs ignored
              flagopv=set_flagval(optype,opvalue)

              if (LsStat.eq.LsDone) then           !Returning to MATH OPER
                 HoldValue=calc_Math(OpType,flagOpv,VLS,HoldValue) !vls is value

              else              !LsStat must be LsNone

c              ...........................
c             note::we are ready to analyze a funk or () operation
c               (specified by Optype and flagOpv).
c              ...........................


                   doV1=0.
                   HoldValue=calc_Math(OpType,flagopv,DoV1,HoldValue)  !dov1 is flag
                                        !New Holdvalue uses old Holdvalue

              endif                    !Return to Rs (3 types)
              goto 200                  !LS or RS subtree value for higher node

           endif                       !Return to LS or RS


        End

ccccccccccc NOTES

c NOTE 1
c  Function ( permanent functions) and
c   IOPR "()" are handled here.
c IOPRS and Perm Functions are handled by calc_MATH, but the last (user function)
c requires some tricky, recursive type operations. Specifically, we
c must "recursively" process a  tree, using the value (HOLDVALUE) as
c  the value of variable(1) :: the "user function variable".


c-----------------------------
c set flag, checks for overflow
        function set_flagval(itype,val)

        real *8 val

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        if (itype.ne.valu) then
           set_flagval=val         !use a *4 variable for flag type work
        else
           set_flagval=0
        endif
        return
        end


C=================================================
C=================================================
c Stack manipulation routines:
c  To save array space, these routines use the CALC_PUSHS and CALC_POPS routines.
c
c   --------------------------------------------------------

        Subroutine calc_pushd(Opr,Ls,Rs,Pc)

        implicit none

        integer opr,ls,rs,pc

        real *8 v1
        real v2

        v1=opr
        v2=pc
        call calc_pushs(v1,ls,rs,v2)

        return
        end

c--------------------------------------------

        Subroutine Calc_PopD(opr,ls,rs,pc)

        implicit none

        integer opr,ls,rs,pc

        real *8 v1
        real v2


        call calc_pops(v1,ls,rs,v2)
        opr=v1
        pc=v2

        return
        end

C=================================================



c=====================================================================
c calc_Push onto analyze stack (USED BY SEVERAL ROUTINES, INCLUDING
C BUILD_TREE, ANALYZE_TREE,

        SUBROUTINE calc_PushS(V1,Lstat,NADR,V2)


        PARAMETER (maxstak2=150)

        real *8 v1

        integer *2 stknode(MAXSTAK2)
        Integer *2 LsStats(MAXSTAK2)
        REAL *8 VALL(MAXSTAK2)
        REAL valr(MAXSTAK2)
        INTEGER NSTACK2
        common /calc_STACK2/NSTACK2,VALL,LsStats,STKNODE,VALR

        NSTACK2=NSTACK2+1

        IF (NSTACK2.GE.MAXSTAK2-5) THEN
            CALL Calc_Error(
     1 ' ! OUT OF MEMORY (PERHAPS A SELF REFERENCING USER FUNCTION)')
            CALL ZERO_ANSTACK           !AND CLEAR STACK FOR FUTURE USE
            RETURN

        endif

        VALL(NSTACK2)=V1
        VALR(NSTACK2)=V2
        STKNODE(NSTACK2)=NADR
        LsStats(NSTACK2)=Lstat

        RETURN
        END

c=====================================================================
c calc_pop FROM analyze stack
c (USED BY SEVERAL ROUTINES, INCLUDING
C BUILD_TREE, ANALYZE_TREE, AND

        SUBROUTINE calc_popS(V1,Lstat,NADR,V2)


        PARAMETER (maxstak2=150)

        real *8 v1

        integer *2 stknode(MAXSTAK2)
        Integer *2 LsStats(MAXSTAK2)
        REAL *8 VALL(MAXSTAK2)
        REAL valr(MAXSTAK2)
        INTEGER NSTACK2
        common /calc_STACK2/NSTACK2,VALL,LsStats,STKNODE,VALR

        IF (NSTACK2.LT.1) THEN
            CALL Calc_Error(' ! PROBLEM DURING EQUATION ANALYSIS ')
            RETURN
        endif

        V1=VALL(NSTACK2)
        V2=VALR(NSTACK2)
        NADR=STKNODE(NSTACK2)
        Lstat=LsStats(NSTACK2)

        NSTACK2=NSTACK2-1

        RETURN
        END


C---------------------------------------------------------------
C FUNCTION TO Return the results of some operation (typically a math
c  operation).  2 side (VLS and VRS), and a type of math operation
c  (OPTYPE and OPVALUE will specify).
C
C WE DO NOT COME HERE IF USER FUNCTION IS about to be ANALYZED.
c User function analysis consists of a "recursive" branching to
c  another subtree, with storage onto "variable location 1 stack", where
c it can be referenced from the subtree.  This is a recursive process
c (since the equation of a user function can include any and all
c user functions, including itself), hence we do NOT want to come
c here.

        Real *8 FUNCTION calc_Math(OpType,OpValue,VLS,VRS)

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER OPTYPE
        REAL OPVALUE                    !opvalue is used as a flag

        real *8 vls,vrs                 !vls and vrs are values
        real *8 analyze_permfunk,vv,calc_exponentiate

        if (Optype.eq.OPER.and.abs(OpValue).eq.Iopr) then
            calc_math=vrs*sign(1.,opvalue)      !vls used for a flag...
            return
        endif

        if (optype.eq.Funk) then                !IF HERE AND FUNK, IT IS A PERM FUNK
c       print *,' call analypermfunk, opvalue,vrs ',opvalue,vrs
           VV=Analyze_PERMfunk(OpValue,VRS)     !vls not needed
           calc_math=vv
           return
        endif

        if (optype.eq.oper) then                !a math operator
           if (opvalue.eq.add) then
             calc_math=vls+vrs
           elseif (opvalue.eq.minus) then
             calc_math=vls-vrs
           elseif (opvalue.eq.mult) then
             calc_math=vls*vrs
           elseif (opvalue.eq.div) then
             if (vrs.eq.0.and.vls.eq.0.) then
                calc_math=1.                    !do this to avoid deriv problems
             elseif (vrs.eq.0.) then
               call Calc_Error(' ! Divide by 0 ')
               return
             else
                calc_math=vls/vrs
             endif
           elseif (opvalue.eq.modop) then         !mod OPERATION (9 % 4 = 1 )
             if (vrs.eq.0.and.vls.eq.0.) then
                calc_math=0.
             elseif (vrs.eq.0.) then
               call Calc_Error(' ! Divide by 0 ')
               return
             else
                CALC_MATH=DMOD(VLS,VRS)
C               CALC_MATH=VLS-(DINT(VLS/VRS)*VRS)
             endif
           elseif (opvalue.eq.exp) then
              vv=calc_exponentiate(vls,vrs)
              calc_math=vv
           else
              call Calc_Error(' ! Unrecognized math operator ')
           endif

           return
        endif

c should never get here, but...

        call Calc_Error(' ! Failure in Math Module ')
        return
        end


c===========================================================

        Real *8 function calc_Exponentiate(v1,v2)

        Real *8 v1,v2,tmp


c take care of 0**n

        if (v1.eq.0.0) then
            if (power.lT.0.) then
              call CALC_error(' ! Bad Exponentiation of 0.0 ')
            elseif (POWER.EQ.0.0) THEN
               calc_EXPONENTIATE=1.0
            ELSE
                calc_exponentiate=0.0
            endif
            return
        endif

c now we dont have to worry about divide by 0.
c take care of n**0, n**1 and n ** -1

        if (v2.eq.0.0) then
           calc_exponentiate=1.
           return
        elseif (v2.eq.1.0 ) then
           calc_exponentiate=v1
           return
        elseif (v2.eq.-1.0) then
           calc_exponentiate=1./v1
           return
        endif

        t1=dlog(dabs(v1))*v2            !check for overflow
        if (abs(t1).gt.700) then
            call Calc_Error(
     1             '! Exponentiation error: too large')
            return
        endif

        if (v1.lt.0 .and. v2.ne.dint(v2)) then
               call Calc_Error(
     1             '! Exponentiation error: negative number')
                return
        elseif (dabs(v2).gt.100) then             !big exponent
             calc_exponentiate=v1**v2
             return
        endif

        Power=V2
        Ipower=Power

c we have a non trivial exponentiation
c  Since greater accuracy from standard multiplication, we use
c  multiplication when integer power.

        if (v1.lt.0) then               !only integer powers allowed if - num
           if (power.ne.dint(power)) then
               call Calc_Error(
     1             '! Exponentiation error: negative number')
               return
           else                 !integer power w/neg root
              tmp=1.0
              do ij=1,iabs(ipower)
                 tmp=tmp*v1
              end do
              if (ipower.lt.0.) then            !eg  -10.**-2
                 tmp=1./tmp                     !took care of / by 0.0 above.
              endif
              calc_exponentiate=tmp
              return
           endif                               !v1 is negative

        else                            !v1 is positive

            if (ipower.ne.power) then
               calc_exponentiate=v1**v2
               return
             else
                tmp=1.
                do ij=1,iabs(ipower)
                  tmp=tmp*v1
                end do
                if (ipower.lt.0) then
                   tmp=1./tmp
                endif
                calc_exponentiate=tmp
                return
            endif

        endif                  !positive / negative number

        end

c======================================================================
c
c function analyzer
c We should NEVER get here if user function,,, since user functions are
c branches to subtrees, and are not analyzed "mathematically"

        Real *8 Function Analyze_PERMfunk(OpValue,arg)

        real opvalue
        real  *8 arg,calc_exponentiate,radval
        REAL *8 ANALYZE_FUNK
        REAL *8 TEN,aa                  !USED BY EXP10

        integer ifunk
        data PI/3.1415926535/

C       'SIN','COS',  'TAN','SINR','COSR',  'TANR',
C      'ARCSINR','ARCCOSR','ARCTANR','ARCSINR','ARCCOSR','ARCTANR',
C      'ABS','EXP','EXP10','LOG','LOG10',
C                      'INT','SQRT','RADTODEG','DEGTORAD',

        ifunk=abs(opvalue)
        asign=sign(1.,opvalue)
c       print *,' ifunk asign ',ifunk,asign

        radval=(ARG/360.)*(2*PI)     ! used for Degree trig funcs
        analyze_funk=0                  !just a default (if ierr>0)

C       'SIN','COS',  'TAN','SINR','COSR',  'TANR',

        if (ifunk.ge.1 .and. ifunk.le.6) then
            if (dabs(radval).ge.1.0e10) then
                call calc_error(
     1             ' ! Trig Function fails with large numbers')
                analyze_permfunk=0
                return
             endif
        endif

c else, not a bad trig function to analyse
        if (ifunk.eq.1) then
           analyze_funk=Dsin(radval)*asign
        elseif (ifunk.eq.2) then
           analyze_funk=Dcos(radval)*asign
        elseif (ifunk.eq.3) then
           TARG=radval                     !AVOID *8 PROBLEMS
           if (abs(amod(Targ-90.,180.)).lt..0001) then
             call Calc_Error(' ! Bad argument to tangent (DEGREES) ')
           else
               analyze_funk=Dtan(radval)*asign
           endif
        elseif (ifunk.eq.4) then
           analyze_funk=DSIN(ARG)*asign
        elseif (ifunk.eq.5) then
           analyze_funk=DCOS(ARg)*asign
        elseif (ifunk.eq.6) then
           TARG=ARG
           if (abs(amod(Targ-(PI/2.),PI)).lt..0001) then
             call Calc_Error(' ! Bad argument to tangent (RADIAN) ')
           else
               analyze_funk=Dtan(arg)*asign
           endif

C      'ARCSIN','ARCCOS','ARCTAN','ARCSINR','ARCCOSR','ARCTANR',
        elseif (ifunk.eq.7) then
            IF (DABS(ARG).GT.1.) THEN
              CALL Calc_Error(' ! IMPOSSIBLE ARCSIN ')
            ELSE
                radval=DASIN(arg)*asign
                ANALYZE_FUNK=(radval/(2*PI))*360.*ASIGN
            endif
        elseif (IFUNK.EQ.8) THEN
            IF (DABS(ARG).GT.1.) THEN
              CALL Calc_Error(' ! IMPOSSIBLE ARCCOS ')
            ELSE
                radval=DACOS(arg)*asign
                ANALYZE_FUNK=(radval/(2*PI))*360.*ASIGN
            endif
        elseif (IFUNK.EQ.9) THEN
            radval=DATAN(ARG)*ASIGN
            ANALYZE_FUNK=(radval/(2*PI))*360.*ASIGN
        elseif (ifunk.eq.10) then
            IF (DABS(ARG).GT.1.) THEN
              CALL Calc_Error(' ! IMPOSSIBLE ARCSINR ')
            ELSE
                analyze_funk=DASIN(arg)*asign
            endif
        elseif (IFUNK.EQ.11) THEN
            IF (DABS(ARG).GT.1.) THEN
              CALL Calc_Error(' ! IMPOSSIBLE ARCCOSR ')
            ELSE
                analyze_funk=DACOS(arg)*asign
            endif
        elseif (IFUNK.EQ.12) THEN
            ANALYZE_FUNK=DATAN(ARG)*ASIGN

C      'ABS','EXP','EXP10','LOG','LOG10',
        elseif (IFUNK.EQ.13) THEN
           ANALYZE_FUNK=DABS(ARG)*ASIGN
        elseif (IFUNK.EQ.14) THEN
           if (arg.lt.750) then
               ANALYZE_FUNK=DEXP(ARG)*ASIGN
           else
               call calc_error(' ! Value to large to exponentiate')
           endif
        elseif (IFUNK.EQ.15) THEN
           TEN=10.0
           ANALYZE_FUNK=calc_EXPONENTIATE(TEN,ARG)*ASIGN        !USE MY EXPONENTIATION
        elseif (IFUNK.EQ.16) THEN
           IF (ARG.LE.0.) THEN
             CALL Calc_Error(' ! CAN NOT TAKE LOG. ')
           ELSE
             ANALYZE_FUNK=DLOG(ARG)*ASIGN
           endif
        elseif (IFUNK.EQ.17) THEN
           IF (ARG.LE.0.) THEN
             CALL Calc_Error(' ! CAN NOT TAKE LOG10 ')
           ELSE
             ANALYZE_FUNK=DLOG10(ARG)*ASIGN
           endif
C                      'INT','SQRT','RADTODEG','DEGTORAD',
        elseif (IFUNK.EQ.18) THEN
           JTMP=ARG
           ANALYZE_FUNK=JTMP*ASIGN
        elseif (IFUNK.EQ.19) THEN
           if (arg.lt.0.) then
             call Calc_Error(' ! Square Root of negative number ')
           else
              ANALYZE_FUNK=DSQRT(ARG)*ASIGN
           endif
        elseif (IFUNK.EQ.20) THEN              !RADIANS TO DEGREES
           ANALYZE_FUNK=(ARG/(2*PI))*360.*ASIGN
        elseif (IFUNK.EQ.21) THEN              !DEGREES TO RADIANS
           ANALYZE_FUNK=(ARG/360.)*(2*PI)*ASIGN
        elseif (IFUNK.EQ.22) THEN
           ANALYZE_FUNK=SINH(ARG)*ASIGN
        elseif (IFUNK.EQ.23) THEN
           ANALYZE_FUNK=COSH(ARG)*ASIGN
        elseif (IFUNK.EQ.24) THEN
           ANALYZE_FUNK=TANH(ARG)*ASIGN
        elseif (IFUNK.EQ.25) THEN
           IF (ARG.GT.170.OR.ARG.LT.0) THEN
               CALL CALC_ERROR(
     1           '  !  > 170 or <0, can Not take factorial ')
           else
               IF (DINT(ARG).NE.ARG) THEN
                      CALL CALC_ERROR(
     1                    '  ! Can Not take factorial of fraction ')
                else
                    aa=1.
                    do ij=2,arg
                       aa=aa*ij
                     end do
                     analyze_funk=asign*aa
                endif
             endif
        endif

        ANALYZE_PERMFUNK=ANALYZE_FUNK
        RETURN
        end



c=============================================================================

c Function and variable manipulation routines



c-------------------------------------------------------------
C RETURN LOCATION OF VARIABLE (0 IF NO match)

        SUBROUTINE FIND_VARINAME(HOLD0,AVAL)
        PARAMETER (Maxvari=10)
        CHARACTER *(*) HOLD0

        character *12 hold,tmp

        CHARACTER *12 VARINAMES(MAXVARI)
        REAL *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        hold=hold0
        if (hold.ne.' ') hold=hold(1:len_trim(hold))

        DO IJ=1,NVARI
          tmp=varinames(ij)
          if (tmp.eq.' ' .and. ij.ne.1) then
                cycle
          elseif (tmp.ne.' ') then
                tmp=tmp(1:len_trim(tmp))
          endif

          IF (HOLD.EQ.tmp) THEN
             AVAL=IJ
             RETURN
          endif
        END DO

        AVAL=0                  !FLAG, COULD NOT FIND


        RETURN
        END

C-----------------------------------------
C ENTER VARI NAME IN LIST (IF NOT THERE)

        SUBROUTINE SET_VARINAME(HOLD0,AVAL)     !AVAL WILL POINT TO VAR TABLE

        PARAMETER (Maxvari=10)
        CHARACTER *(*) HOLD0

        character *12 hold,tmp

        CHARACTER *12 VARINAMES(MAXVARI)
        REAL *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        hold=hold0
        if (hold.ne.' ')hold=hold(1:len_trim(hold))

        DO IJ=1,NVARI                           !note:: this routine CAN
          tmp=varinames(ij)
          if (tmp.eq.' ' .and. ij.ne.1) then
                cycle
          elseif (tmp.ne.' ') then
                tmp=tmp(1:len_trim(tmp))
          endif
          IF (HOLD.EQ.tmp) THEN       !redefine pi and E.. it is
             AVAL=IJ                            !calc_equations responsibility
             RETURN                             ! to prevent this
          endif
        END DO

        IF (NVARI.LT.MAXVARI) THEN
            NVARI=NVARI+1
            VARINAMES(NVARI)=HOLD
            VariValues(nvari)=0.
            AVAL=NVARI
            RETURN

c else, see if open spot
        else
            do ij=3,maxvari
              if (varinames(ij).eq.' ') then
                varinames(ij)=hold
                varivalues(ij)=0.
                aval=ij
                return
              endif
            enddo               !if no blanks, then give up
        endif

C ELSE, error
        CALL Calc_Error(' ! TOO MANY VARIABLES SPECIFIED ')
        RETURN
        END



c==========================================================
C Search for function name
        SUBROUTINE  FIND_FUNC(HOLD0,AVAL)     !AVAL POINTS TO FUNCTION TABLE

        PARAMETER (maxfunk=25)
        PARAMETER (NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)

        CHARACTER *(*) HOLD0

        character *12 hold,tmp2
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        common /calc_FUNCTIONS/FUNKNAMES


        logical qradians
        common /trigtype/qradians


        hold=hold0
        if (hold.ne.' ') hold=hold(1:len_trim(hold))

c these next few lines are a gooopy hack
        if (hold.eq.'LN') Then
            hold='LOG'
        elseif (hold.eq.'SIN') then
           if (qradians) then
                hold='SINR'
           else
                hold='SIND'
           endif
        elseif (Hold.eq.'COS') THen
           if (qradians) then
                hold='COSR'
           else
                hold='COSD'
           endif
        elseif (hold.eq.'TAN') then
           if (qradians) then
                hold='TANR'
           else
                hold='TAND'
           endif

        elseif (hold.eq.'ARCSIN') then
           if (qradians) then
                hold='ARCSINR'
           else
                hold='ARCSIND'
           endif
        elseif (Hold.eq.'ARCCOS') THen
           if (qradians) then
                hold='ARCCOSR'
           else
                hold='ARCCOSD'
           endif
        elseif (hold.eq.'ARCTAN') then
           if (qradians) then
                hold='ARCTANR'
           else
                hold='ARCTAND'
           endif

        endif

        DO IJ=1,NpermFUNK
          tmp2=funknames(ij)
          if (ij.gt.npermfunk) then
            call calc_error(' no user functions ')
            return
          endif
          tmp2=tmp2(1:len_trim(tmp2))
          IF (hold.eq.tmp2) THEN
             AVAL=IJ
             RETURN
          endif
        END DO

C IF HERE, COULD NOT FIND THE FUNCTION

C ELSE, NOTE THAT FUNCTION NAME NOT FOUND BY RETURNING A ZERO
        AVAL=0
        RETURN
        END




c============================================================
c return function name given location

        Character *(*) Function get_funkname(AVAL)

        PARAMETER (maxfunk=25)
        PARAMETER (NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)


        CHARACTER *12 FUNKNAMES(MAXFUNK)
        common /calc_FUNCTIONS/FUNKNAMES


        if (aval.gt.maxfunk) then
           call calc_error(' !non existent function ')
           return
        else
           ij=aval
           get_funkname=funknames(ij)
        endif

        RETURN
        END



c==========================================================
c routine to fill variable value  and to return variable name
c NOTE:: we don't fill varivalue(1) (function argument) here.
c Note:: mvari can be ANY value, hence to prevent overwrite of
c first 3 locatons (function value,Pi, and E) you must check
c prior to calling Calc_putvari (eg; this is done in calc_equation)

        subroutine CALC_putvari(mvari,vname,aval)

        parameter (Maxvari=10)

        character *(*) vname

        REAL *8 AVAL

        CHARACTER *12 VARINAMES(MAXVARI)
        real *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        varivalues(mvari)=aval          !assign value

        vname=varinames(mvari)          !and return the name


        return
        end


c==========================================================
c
c function to return current value of a variable

        REAL *8 FUNCTION calc_GET_VariValue(AVAR)

        parameter (Maxvari=10)
        PARAMETER (maxnode=250)

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        Real Avar

        CHARACTER *12 VARINAMES(MAXVARI)
        real *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues
                                !E and PI are predefined (and first vari is
                                !special "function argument index")

c note:: varinames(1) is reserved for "function argument". This
c location will be loaded with the current "function argumentname".
c When this location is requested, we will use the value at varinames(1)
c  as a pointer to a location where the value (returned from lower in
c  the tree) is being stored.


        IVAR=abs(AVAR)

        if (ivar.ne.1) theN

           calc_get_varivalue=varivalues(ivar)*sign(1.,avar)
           return
        endif

c else, we have a function variable.  Go get the current function argument
c  value

        iv=Dabs(varivalues(ivar))               !IVAR = 1

        calc_Get_varivalue=Value(iv)*sign(1.,avar)      !use tree nodes for storage

        return

        end

c==========================================================
c
c function to return current value of a variable

        Character *(*) FUNCTION GET_VariName(AVAR)

        parameter (Maxvari=10)
        PARAMETER (maxnode=250)

        Real Avar

        CHARACTER *12 VARINAMES(MAXVARI)
        real *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

c note: varinames(1) should be loaded with argumentname of
c  function being displayed.

        IVAR=abs(AVAR)

        if (ivar.gt.nvari) theN
           call calc_error(' ! referencing nonexistent variable ')
         else
           get_variname=varinames(ivar)
        endif

        return

        end


C=================================================
C FUNCTION TO RETURN NUMBER OF VARIABLES CURRENTLY ACTIVE (both perm and user)

        INTEGER FUNCTION CALC_NumbVars()

        parameter (Maxvari=10)
        CHARACTER *12 VARINAMES(MAXVARI)
        real *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        CALC_NUMBVARS=NVARI             !3 PERMANENT VARIABLES

        RETURN
        END





C=================================================
C ZERO ANALYSIS STACK

        SUBROUTINE ZERO_ANSTACK

        PARAMETER (maxstak2=150)

        INTEGER *2 STKNODE(MAXSTAK2)
        Integer *2 LsStats(MAXSTAK2)
        REAL *8 VALL(MAXSTAK2)          !holds value
        REAL VALR(MAXSTAK2)             !could be integer *2
        INTEGER NSTACK2
        common /calc_STACK2/NSTACK2,VALL,LsStats,STKNODE,VALR
        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        nstack2=0
        if (maxstak2.eq.0) return
        do ij=1,maxstak2
           stknode(ij)=0
           LsStats(ij)=LsNotDone
           vall(ij)=0
           valr(ij)=0
        end do
        RETURN
        END




C----------------------------------------------------------
C
C  PRIMITIVE OPERATIONS :::
c  The following routines are used in equation tree construction
c     and a few other places.
c  Calc_init             Calc initialization, eg;
c                               Initialize a free list from several arrays.
c
C calc_PUTFREE(Anode)     Puts a node back on free list
c  Left_assign(from,to)
c  Right_assign(from,to)  to becomes left or right son of from.
c   Function calc_Getfree()      Returns a free node, NIL if no more nodes.
c Function Make_valnode(value)    Creates a Value Node, NIL if no more nodes
c Function Make_OPRnode(OPTYPE)   Creates a OPERATOR Node, NIL if no more nodes
c FUNCTION MAKE_FUNKNODE
c GET_NodeINFO(NodeAddress,OPTYPE,OPVALUE,INLS,INRS) !info connected to a node
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


C=========================================================

        FUNCTION calc_GETFREE()

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        integer freenodes
        common /calc_freenode2/freenodes

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        INTEGER NEXTFREE
        common /calc_FREENODE/NEXTFREE

        integer thisnode

        data freenodes/maxnode/

        IF (NEXTFREE.EQ.NIL) THEN
              calc_GETFREE=NIL
              RETURN
        endif

c       ELSE

           calc_GETFREE=NEXTFREE
           ThisNode=NextFree
           NextFree=NODE(ThisNode,RS)
           NODE(ThisNode,LS)=NIL        !CONVERT INTO FREE STANDING NODE
           NODE(ThisNode,RS)=NIL        !CONVERT INTO FREE STANDING NODE
           freenodes=freenodes-1

        RETURN
        END

C--------------------------------------------------------------

        SUBROUTINE calc_PUTFREE(MNODE)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER NEXTFREE
        common /calc_FREENODE/NEXTFREE

        integer freenodes
        common /calc_freenode2/freenodes

        if (mnode.gt.0 .and. mnode.le.maxnode) then
           NODE(MNODE,RS)=NEXTFREE
           OPR(MNODE)=NIL
           NEXTFREE=MNODE
           freenodes=freenodes+1
        endif
C else, bad number, so ignore
        RETURN
        END


C=========================================================
C ATTACH  Node at TO to LEFT side of node at FROM

        SUBROUTINE LEFT_ASSIGN(FROM,TO)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        INTEGER FROM,TO


C Join FROM to TO

        NODE(FROM,LS)=TO

        RETURN
        END


C=========================================================
c Join TO to RS of FROM

        SUBROUTINE RIGHT_ASSIGN(FROM,TO)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        INTEGER FROM,TO



C Join FROM to TO

        NODE(FROM,RS)=TO

        RETURN
        END



c==========================================================
C  Create a Value Node, NIL if no more nodes
c  If atype is VARI, then create a value node using the curren
c  value of the variable indexed by aval.

        INTEGER  Function Make_valnode(Aval,atype)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        INTEGER ANODE,atype
        REAL *8 AVAL

        ANODE=calc_GETFREE()

        IF (ANODE.EQ.NIL) THEN
            MAKE_VALNODE=NIL
            RETURN
        endif

C ELSE, WE HAVE A NODE

        if (atype.eq.vari) then         !variable, MARK AS SUCH
           OPR(ANODE)=VARI
        ELSE
           OPR(ANODE)=VALU
        endif

        VALUE(ANODE)=AVAL

        MAKE_VALNODE=ANODE

        RETURN
        END



C===========================================================
        INTEGER  Function Make_OPRnode(OPTYPE)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        INTEGER ANODE
        REAL OPTYPE


        ANODE=calc_GETFREE()

        IF (ANODE.EQ.NIL) THEN
            MAKE_OPRNODE=NIL
            RETURN
        endif

C ELSE, WE HAVE A NODE
        OPR(ANODE)=OPER
        VALUE(ANODE)=OPTYPE
        MAKE_OPRNODE=ANODE

        RETURN
        END


c============================================================
        INTEGER  Function Make_Funknode(FunkTYpe)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        INTEGER ANODE
        REAL FUNKTYPE


        ANODE=calc_GETFREE()

        IF (ANODE.EQ.NIL) THEN
            MAKE_FunkNODE=NIL
            RETURN
        endif

C ELSE, WE HAVE A NODE
        OPR(ANODE)=FUnk
        Value(anode)=Funktype           !NOTE::: - VALUE OF FUNKTYPE IMPLIES
        MAKE_FunkNODE=ANODE             !WE WILL RETURN NEGATIVE OF FUNCTION.

        RETURN
        END


C===============================================================
C
C   Routine to return OPTYPE,OPVALUE,LS, and RS of a given NODE
c NOTE, sometimes we dont care about LS and RS.

        Subroutine GET_NodeINFO(NodeAddress,OPTYPE,OPVALUE,INLS,INRS)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)
        PARAMETER (maxstak2=150)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        Integer OPTYPE
        real *8 opvalue

        if (nodeaddress.le.0 .or. nodeaddress.gt.maxnode) then
           print *,' Bad Address for node: ',nodeaddress
           stop
        endif
        Optype=Opr(NodeAddress)
        OpValue=Value(NodeAddress)
        INLS=Node(NodeAddress,LS)               !if IOPR, contains sign flag
        INRS=NODE(NodeAddress,RS)


        return
        end



C========================================================
C  ROUTINE write a value (ans) into a string (charans)
c
c Ndec== # of decimal digits
c Qe = true if use E notation
c Nlen == # of significant characters in charans (is returned)
c Nlen = 0 if error.
c If QE=false, and if the value can be expressed as an integer, do so!

        Subroutine calc_WRITENUM(ANS,CharAns,NDEC0,QE,NLEN)


        REAL *8 ANS
        CHARACTER *(*) CHARANS

        character *10 efmt,make_afmt
        LOGICAL QE,qetmp

        ndec=ndec0
        maxlen=len(charans)             !size of character variable
        qetmp=qe

c here for exponential output
 100    continue
        IF (QETmp)      THEN            !EXPONENTIAL NOTATION
           NDECTMP=MAX(1,NDEC)          !NEED AT LEAST ONE
           NLEN=NDECTMP+7               !NEED AT LEAST ONE
           if (ans.lt.0) nlen=nlen+1         ! room for - sign
           if (nLEN.gt.maxlen) then
              charans='*****'
              nlen=0
              return
           end if

           efmt=make_afmt('E',nlen,ndectmp)
           WRITE(CharAns,efmt)ANS
           RETURN
        END IF


c else, here for standard output

c See if okay as integer (this number only, hence use of ndec=ndec0)
        IF (DABS(ANS).LT.2.0E+9) THEN           !SUFFICIENTLY SMALL
           if (dint(ans).eq.ans) then           !AN INTEGER
             ndec=0
           ENDIF
        endif

        IF (DABS(ANS).LT.10.0) THEN     !RIGHT OF DECIMAL
          Il=1
        elsE
           IL=DLOG10(DABS(ANS))
        end if

        if (il+ndec.gt.17) then         !cant use regular, use Exponential
            qetmp=.true.                !force exponential output
            goto 100                    !goto beginning
        end if

        if (ndec.eq.0) then             !integer format
           NLEN=IL+1
           if (ans.lt.0) nlen=nlen+1         ! room for - sign
           if (NLEN.gt.maxlen) then
              charans='*******'
              nlen=0
              return
           end if
           ians=ans                     !WILL FIT
           efmt=make_afmt('I',nlen,0)
           write(charans,efmt)ians
           RETURN
        ELSE                            !REAL FORMAT
           NLEN=IL+NDEC+3       !2 FOR DECIMAL AND SIGN,1 EXTRA
           if (ans.lt.0) nlen=nlen+1         ! room for - sign
           if (NLEN.gt.maxlen) then
              charans='******'
              nlen=0
              return
           end if
           efmt=make_afmt('F',nlen,ndec)
           write(charans,efmt)ans
           RETURN
        END IF

        END

c-----------------------
c make a format statement given type and field lengths
        character *(*) function make_afmt(atype,nlen,ndectmp)

        character *1 atype
        character *10 efmt

        if (atype.eq.'E'.or.atype.eq.'F') then
           if (nlen.gt.9) then
                if (ndectmp.gt.9) then
                   write(efmt,811)atype,nlen,ndectmp
                else
                   write(efmt,812)atype,nlen,ndectmp
                endif
           else
                if (ndectmp.gt.9) then
                   write(efmt,813)atype,nlen,ndectmp
                else
                   write(efmt,814)atype,nlen,ndectmp
                endif
            endif
        elseif (atype.eq.'I') then
                if (nlen.gt.9) then
                   write(efmt,911)atype,nlen
                else
                   write(efmt,912)atype,nlen
                endif
        endif
        make_afmt=efmt
        return

  811      format('(',A1,i2,'.',i2,')')
  812      format('(',A1,i2,'.',i1,')')
  813      format('(',A1,i1,'.',i2,')')
  814      format('(',A1,i1,'.',i1,')')

 911       format('(',a1,i2,')')
 912       format('(',a1,i1,')')

        end




C==================================================

        SUBROUTINE Calc_Error(A)

        common /calc_cmerror/nerr

        CHARACTER *(*) A

        data nerr/0/

        if (a.eq.' ') then
           if (nerr.lt.1) then
             write(6,77)
 77          format(' Error :: Input ignored. ')
           else
              continue                  !do nothing if already an error
           endif
        else                            !a distinct message
           WRITE(6,55)A
        endif
 55     FORMAT(/' ERROR==',A)

        nerr=nerr+1
        RETURN
        END




C==================================
C  Routine to convert a tree into a string, sort of the
c opposite of readequation & Build_tree.
c Ndec and qe used to display numbers. Base is the address of tree
c  Fstring will contain the "tree" string (fstring is returned)


        Subroutine Tree_string(fstring,base,ndec,qe)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=250)
        PARAMETER (maxstak2=150)
        parameter(NPERMFUNK=25)

        logical qe
        character *(*) fstring

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER BP,EP
        common /calc_PTYPE/BP,EP

        INTEGER *2 STKNODE(MAXSTAK2)
        integer *2 LsStats(MAXSTAK2)
        REAL *8 VALL(MAXSTAK2)          !holds value
        REAL VALR(MAXSTAK2)             !could be integer *2
        INTEGER NSTACK2
        common /calc_STACK2/NSTACK2,VALL,LsStats,STKNODE,VALR

        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        real *8 vls,holdvalue
        real *8 opvalue,nil8

        character *12 get_variname,get_funkname
        character *30 charans

        common /calc_cmerror/nerr


        Integer Base                    !the base of tree to be analyzed
        Integer Optype,CurrentNode,NewNode

        Data LsNotdone,LsDone,LsNone,Lsfunk/1,2,3,4/

c zero analysis stack
        CALL ZERO_ANSTACK

        inum=1          !initialize string
        fstring=' '

c  calc_Push a nil to act as a brake
        nil8=nil
        Call calc_PushS(nil8,LsNone,nil,float(nil))

c Get First Node.

        CurrentNode=Base

        Call GET_NodeINFO(CURRENTNODE,OPTYPE,OPVALUE,INLS,INRS)
        flagopv=set_flagval(optype,opvalue)


 100    Continue
        If (nerr.gt.0) return                   !check for error

c Case 1 :: IOPR or FUNK.
c   Functions and () are treated similar, except function name is
c   displayed.
c   Regardless of type, we still need to display the subtree that will
c   that is the argument to this function or ().
c   NOTE:: User and perm functions are completely undifferentiated in
c   this routine.

        IF (optype.eq.funk.or.(optype.eq.OPER.and.
     1          abs(flagopv).eq.IOPR)) then   !use abs (watch out for neg functions)
            Vls=0                       !signal used by user functions
            Vrs=Inrs                    !Address of function or IOPR argument

            if (optype.eq.funk) then
               ll=lsfunk
            else
               ll=lsnone
            endif
            Call calc_PushS(Vls,Ll,CurrentNode,Vrs)

c now display Funk(  or  (


            if (flagopv.lt.0) then              !negative function
               call calc_put_out(Inum,'-',fstring)
            endif

            if (optype.eq.funk) then
               charans=get_funkname(ABS(flagopv))
               call calc_put_out(Inum,charans,fstring)
            else
               call calc_put_out(Inum,'(',fstring)
            endif


            CurrentNode=INRS            !now analyze RS argument.

            Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
            flagopv=set_flagval(optype,opvalue  )

            goto 100
        endif

C Case 2:: VALU OR VARI

        If (OpType.eq.VALU.or.OPTYPE.eq.VARI) then
           IF (OpType.eq.VARI) then
                if (flagopv.lt.0) call CALC_put_out(Inum,'-',fstring)   !neg var
                charans=GET_VariName(flagopv)
           else
                call calc_writenum(opvalue,charans,ndec,qe,nlen)
           endif
           call calc_put_out(Inum,charans,fstring)
           goto 200
        endif


c  CASE 3:: we have a math oper. So, we need to get both left and right nodes.
c  Record this "state" on Stack.
        vls=inls
        vrs=inrs
        Call calc_PushS(vls,LsNotDone,CurrentNode,vrs)  !save left and right nodes
        CurrentNode=InLs                        !start with left side
        Call Get_NodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
        flagopv=set_flagval(optype,opvalue)

        Goto 100


C HERE After Succesful SUBTREE analysis, or if Node is simple (valu or VARI)

 200       continue                             !jump here if returning from subtree analysis
           if (nerr.gt.0) return                !check for error
           Call calc_popS(VLS,LsStat,NewNode,VRS)  !vrs only used to hold address

           IF (NewNode.eq.Nil) Then             !equation displayed
             RETURN                             !so Return
           endif

           IF (LsStat.eq.LsNotDone) then   !returning from Left node (of oper)


              Call Get_nodeInfo(Newnode,OPtype,OPvalue,InLS,InRS)
              flagopv=set_flagval(optype,opvalue)

              call CALC_oper_cvt(charans,flagopv)       !display the math operator
              call calc_put_out(inum,charans,fstring)

              Call calc_PushS(Holdvalue,LsDone,NewNode,Vrs)  !ready to get RS
              CurrentNode=Vrs

              Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
              flagopv=set_flagval(optype,opvalue)

              goto 100

           else                                 !returning to Right Node

              if (LsStat.eq.LsDone) then                !Returning to MATH OPER
                continue                        !do nothing
              elseif (lsstat.eq.lsfunk) then   !perm or user functions
                continue
              else              !LsStat must be LsNone: )
                call calc_PUT_OUT(Inum,')',fstring)  !end of argument
              endif

              goto 200                  !LS or RS subtree value for higher node

           endif                       !Return to LS or RS


        RETURN
        End


C========================================================
C
C Fill a string. Inum is where to put string. Chara is substring.
c Fstring is string. Inum will be returned as old inum+len_trim(substring)
c  Hence, inum is returned as next free space.

        SUBROUTINE calc_PUT_OUT(Inum,CHARA,fstring)

        CHARACTER *(*)CHARA,fstring

        il=len(fstring)
        if (inum.gt.il) return          !cant fit any of substring

        IC=len_trim(CHARA)
        IF (IC.EQ.0) IC=1       !A SPACE

        if (inum+ic.gt.il) ic=il-inum   !cant fit all of substring


        fstring(inum:inum+ic-1)=chara(1:IC)

        inum=inum+ic            !return next free space

        RETURN
        END



C=================================================
c make a node LOGE(ANODE)
        integer function make_lognode(anode)

        implicit none

        integer nil

        integer anode,tmpnode,cd_oprn2
        integer make_funknode,nnode

c note:: "PERMANENT FUNCTION" index for LogE is 16.
        real flndex

        data nil/0/
        data flndex/16./

        tmpnode=cd_oprn2(anode)

        nnode=make_funknode(flndex)
        call left_assign(nnode,nil)
        call right_assign(nnode,tmpnode)

        make_Lognode=nnode

        return
        end

C=================================================
c cause parens to appear around each subtree.
c eg: attach anode to an IOPR node

        Integer function cd_oprn(anode)

        implicit none

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer anode,nnode,make_oprnode
        real *8 opvalue
        integer optype,t1,t2

        integer nil
        data nil/0/

C  NOTE: "SIMPLIFICATION" TEST before making "IOPR" node.

        call get_nodeinfo(anode,optype,opvalue,t1,t2)
        if (optype.eq.oper) then
           if (opvalue.ne.iopr) then    !some kind of math operator
              goto 200
           endif
        endif

c not a math operator
        cd_oprn=anode                           !no need for extra parens.
        return

c else, some kind of math operation
 200    continue
        nnode=make_oprnode(float(Iopr))
        call left_assign(nnode,nil)
        call right_assign(nnode,anode)
        cd_oprn=nnode

        return
        end

C=================================================
c no simplification version of above

        integer function cd_oprn2(anode)

        implicit none

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer anode,nnode,make_oprnode

        integer nil
        data nil/0/

        nnode=make_oprnode(float(Iopr))
        call left_assign(nnode,nil)
        call right_assign(nnode,anode)
        cd_oprn2=nnode

        return
        end

C=================================================

C=================================================
c routine to put together a "math" node, given operator,ls and rs nodes

        integer function calc_mathnode(anoper,ls,rs)

        implicit none
        integer maxnode

        PARAMETER (maxnode=250)

        integer anoper,ls,rs,tmpnode,itmp,ACALC_MATHNODE
        integer cd_oprn,make_valnode,make_oprnode
        real *8 calc_math

        integer nil
        common /calc_cmnil/nil

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,modop

        integer t1,t2,optype1,optype2,optypetmp,t4,t5
        real *8 opvalue1,opvalue2,aval,opvaltmp

c This routine will also do simplifications.
c  Note: If IOPR node, check to see if it hold just a value or vari.
c               (Eg: peel parentheses off if just a value or vari inside)

        if (ls.eq.nil.or.rs.eq.nil) goto 800   !no simplification possible

        call get_nodeinfo(ls,optype1,opvalue1,t1,t2)
        optypetmp=optype1
        opvaltmp=opvalue1
 12     continue
        if (optypetmp.eq.oper.and.dabs(opvaltmp).eq.iopr) then  !peel parens?
           call get_Nodeinfo(t2,optypetmp,opvaltmp,t4,t5)

C          if (optypetmp.eq.oper.and.dabs(opvaltmp).eq.iopr) then !keep peeling
C             t2=t5
C             goto 12

           if (optypetmp.eq.valu.or.optypetmp.eq.vari) then   !use the
                optype1=optypetmp               !valu or vari, not the (xx)
                opvalue1=opvaltmp
            endif

        endif                  !if internal is not valu or vari, no change

        call get_nodeinfo(rs,optype2,opvalue2,t1,t2)
        optypetmp=optype2
        opvaltmp=opvalue2
C 14    continue
        if (optypetmp.eq.oper.and.dabs(opvaltmp).eq.iopr) then   !same as above
           call get_Nodeinfo(t2,optypetmp,opvaltmp,t4,t5)

            if (optypetmp.eq.valu.or.optypetmp.eq.vari) then
                optype2=optypetmp
                opvalue2=opvaltmp
            endif

        endif                  !if internal is not valu or vari, no change


        if (.not.(optype1.eq.valu.or.optype2.eq.valu)) goto 800 !!simplify?

c if here, check for simplification possibilities.

c First, are both sides "values". If so, analyze it.
        if (optype1.eq.valu.AND.optype2.eq.valu) then
           aval=calc_math(oper,float(anoper),opvalue1,opvalue2)
           Acalc_mathnode=make_valnode(aval,valu)
           GOTO 900
        endif

C       GOTO 800

c If here, then ONE of the two is a valu. Hence, many possibilities for
c       simplification.

        if (anoper.eq.add) then                 !addition
           if (opvalue1.eq.0..AND.OPTYPE1.EQ.VALU) Then
             Acalc_mathnode=cd_oprn(rs)
             GOTO 900
           elseif (opvalue2.eq.0. .AND.OPTYPE2.EQ.VALU) then
             Acalc_mathnode=cd_oprn(ls)
             GOTO 900
           else
             goto 800                   !no simplification
           endif

        elseif (anoper.eq.minus) then          !subtraction
           if (opvalue2.eq.0. .AND.OPTYPE2.EQ.VALU) Then
             Acalc_mathnode=cd_oprn(ls)
             GOTO 900
           elseif (opvalue1.eq.0..AND.OPTYPE1.EQ.VALU) then
             itmp=make_oprnode(-1.)
             call right_assign(itmp,rs)
             Acalc_mathnode=itmp
             GOTO 900
           else
             goto 800                   !no simplification
           endif

        elseif (anoper.eq.mult) then           !multiplication
1100    CONTINUE
           if ( (opvalue1.eq.0..AND.OPTYPE1.EQ.VALU)
     1         .or.  (opvalue2.eq.0..AND.OPTYPE2.EQ.VALU) ) then
               aval=0.
               Acalc_mathnode=make_valnode(aval,valu)
               GOTO 900
           elseif (opvalue1.eq.1..AND.OPTYPE1.EQ.VALU) then
               Acalc_mathnode=cd_oprn(rs)
               GOTO 900
           elseif (opvalue2.eq.1. .AND.OPTYPE2.EQ.VALU) then
               itmp=cd_oprn(ls)
                Acalc_mathnode=itmp
               GOTO 900
           else
                goto 800
           endif

        elseif (anoper.eq.div.OR.ANOPER.EQ.MODOP) then        !division
           if (opvalue1.eq.0. .AND.OPTYPE1.EQ.VALU) then
             aval=0.
             Acalc_mathnode=make_valnode(aval,valu)
             GOTO 900
           elseif (opvalue2.eq.0. .AND.OPTYPE2.EQ.VALU) then
             call calc_error(' ! Divide by 0 in derivative ')
             GOTO 900
           else
             goto 800
           endif

        elseif (anoper.eq.exp) then            !exponentiation
            if (opvalue2.eq.1. .AND.OPTYPE2.EQ.VALU) then
              Acalc_mathnode=cd_oprn(ls)
              GOTO 900
            elseif (opvalue2.eq.0. .AND.OPTYPE2.EQ.VALU) then
               aval=1.
               Acalc_mathnode=make_valnode(aval,valu)
               GOTO 900
            elseif (opvalue1.eq.1. .AND.OPTYPE1.EQ.VALU) then
               aval=1.
               Acalc_mathnode=make_valnode(aval,valu)
               GOTO 900
           elseif (opvalue1.eq.0. .AND.OPTYPE1.EQ.VALU) then
               aval=0.
               Acalc_mathnode=make_valnode(aval,valu)
               GOTO 900
           else
              goto 800
           endif
        endif

 800    continue                                !here if no simplification

        tmpnode=make_oprnode(FLOAT(anoper))
        IF (TMPNODE.LT.0) STOP ' WOW '
        call left_assign(tmpnode,ls)
        call right_assign(tmpnode,rs)
        Acalc_mathnode=cd_oprn(tmpnode)

 900    CONTINUE
        CALC_MATHNODE=ACALC_MATHNODE

        return
        end

c==================================================
        character *1 function next_char(nchar,newnchar,strung)

        character *1 achar

        character *(*)strung

        ilen=len_trim(strung)

        if (nchar.lt.ilen) then         !there are legit characters
          iat=nchar+1
          do while (iat.le.ilen)
            achar=strung(iat:iat)
            if (achar.eq.' '.or.achar.eq.';'.or.achar.eq.':') then
              iat=iat+1
              cycle
            else
              newnchar=iat
              next_char=achar
              return
            endif
          enddo
        endif

c if here, no legit characters
           next_char=' '
           newnchar=len(strung)

        return
        end



