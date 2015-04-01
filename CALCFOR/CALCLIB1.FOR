c @c for the public domain. D. Hellerstein, ERS/RTD, 11/91
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
c       CALC_SHOWFUNKS(IOUT,NDEC,QEXP,AFUNK,ilen,iwp51,qdo_fmt)  -- Displays
c          the  Name,Argument,
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
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c======================================================================

        SUBROUTINE calc_EQUATION(Answer,pSTRING,NERR2)


        CHARACTER *(*)pSTRING

        CHARACTER *650 PSTRING1
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

        CHARACTER *650 PSTRING1
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


        CHARACTER *650 PSTRING1
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
c maxlen0 is the number of characters to display
c iwp51 is 1 if "display in wp 5.1 format", else not calc format
c Note: if iwp51=1, maxlen0 of 4500 will be used

        Subroutine CALC_ShowFunks(IOUT,
     1                        ndec,qe,afunk,maxlen0,iwp51,qdo_fmt)

        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)
        character *(*) afunk
        logical qdo_fmt,qe
        character * 4500 fstring

        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        character *24 ffn,ffname,expand_apost
        character *12 funkargs(ntmpfunk),atmp
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        character *4 ablink,abold,ablock,anormal
        common /calc_screen/ablink,abold,ablock,anormal

        integer maxlen0,iwp51

        common /calc_cmerror/nerr

        NERR=0                  !RESET ERROR COUNTER

c note: iwp51 =1 only if afunk ne ' '
        maxlen=maxlen0
        if (afunk.NE.' ') maxlen=4500       !force complete display of
                                        ! single functions
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
              CALL Tree_string(fstring,II,ndec,qe,iwp51,qdo_fmt)
              nn=min(len_trim(fstring),maxlen)
              iadd=140

c special output if iwp51
              if (iwp51.eq.1) then
                  ffname=expand_apost(funknames(npermfunk+ij),1)
                  ffn=expand_apost(ffn,1)
                  write(IOUT,6613)ffname(1:len_trim(ffname)),
     1                           ffn(1:len_trim(ffn))
 6613       FORMAT(2x,a,'( ',a, ' ) =' )
c from 1 to len_trim(fstring), max of 80 per line, but look for space
                  i1=1
                  i2=0
                  do while (i2.lt.len_trim(fstring))
                    i2=NEXT_NON_CHAR(I1+45,fSTRiNG,len_trim(fstring))
                     write(iout,6614)
     1                  fstring(i1:min(len_trim(fstring),i2))
 6614                format(3x,a)
                    i1=i2+1
                  enddo

              else      ! ASCII style output

                if (iout.eq.6) iadd=40
                ffname=funknames(npermfunk+ij)
                if (nn.gt.iadd) then   ! wrap it around
                    i1=1
                    i2=iadd
                    write(IOUT,66)ffname(1:12),
     1                           ffn(1:12),fstring(i1:i2)
                    do while (i2.lt.nn)
                       i1=i1+iadd
                       i2=min(i2+iadd,nn)
                       if (i2.eq.nn .and. i2.lt.len_trim(fstring) ) then
                         write(IOUT,6612)fstring(i1:i2),ablink,anormal
                       else
                          write(IOUT,661)fstring(i1:i2)
                       endif
                    enddo
                else                      !fit on one line
                  if (len_trim(fstring).le.nn) then
                    write(IOUT,66)ffname(1:12),ffn(1:12),fstring(1:nn)
                  else                     !too long
                    write(IOUT,662)ffname(1:12),ffn(1:12),
     1                             fstring(1:nn),ablink,anormal
                  endif
                endif
              endif
        end do

 662        FORMAT(t2,': ',a,'(',a,') = ',a,a,' ¯',a)
 66        FORMAT(t2,': ',a,'(',a,') = ',a)
 661       format(t29,':::',t34,a)
 6612      format(t29,':::',t34,a,a,' ¯',a)
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
        CALL CALC_GARBAGEMAN(0)

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

        PARAMETER (maxnode=6000)

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

        PARAMETER (MAXELEM=600)

        real *8 aval
        INTEGER ATYPE

        Integer *2 elemopr(MAXELEM)
        REAL *8 ELEMVALS(MAXELEM)
        INTEGER ELEMCOUNT

        common /calc_ELEMENTS/ELEMCOUNT,LASTELEM,ELEMOPR,ELEMVALS

        DATA ELEMCOUNT/0/

C WE COULD CHECK FOR OVERFLOW, BUT IT IS SOOO UNLIKELY AS TO BE IMPOSSIBLE
C (650 CHARACTER MAX ON STRING LENGTH)

        ELEMCOUNT=ELEMCOUNT+1
        AVAL=ELEMVALS(ELEMCOUNT)
        ATYPE=ELEMOPR(ELEMCOUNT)

        RETURN
        END







C==============================================
C ROUTINE TO READ AN EQUATION FROM TERMINAL, and create an "equation list"

        SUBROUTINE calc_readequation(string,userarg)

        PARAMETER (MAXELEM=600)
        PARAMETER (maxnode=6000)

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
        PARAMETER (maxelem=600)
        PARAMETER (maxnode=6000)

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

        PARAMETER (maxnode=6000)
        PARAMETER (maxstak2=1750)
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


c zero analysis stack
        CALL ZERO_ANSTACK

c set "function argument pointer"; if farg=nil, no function argument
c  (standard equation)

        call put_funkarg(farg)
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

              if (LsStat.eq.LsFunk) then        !returning from user function analysis
                 OpType=OPER
                 flagopv=Iopr*sign(1.,flagopv)  !"negative" function
                 DoV1=1.
                 HoldValue=calc_Math(OpType,Flagopv,DoV1,HoldValue)  !DoV1 is flag

              elseif (LsStat.eq.LsDone) then           !Returning to MATH OPER
                 HoldValue=calc_Math(OpType,flagOpv,VLS,HoldValue) !vls is value

              else              !LsStat must be LsNone: ANY FUNK or ( )

c              ...........................
c             note::we are ready to analyze a funk or () operation
c               (specified by Optype and flagOpv). Special actions if
c               a UserFUnction (NOTE 1 below)
c              ...........................

c Is it User Function?
                 IF (Optype.eq.funk.and.abs(flagopv).gt.npermfunk)
     1               then

c              ...........................
C             Note:: DO_CALC_userfunk will do some important work to allow for smooth
c             transition between "equation" tree and "function" tree
c             (both coming and going). See Note 2 below
c              ...........................

                  Call DO_Calc_userfunk(
     1                 NewNode,flagopv,holdvalue,currentnode) !set Currentnode
                  goto 90                       !start recursion (USE CURRENTNODE)

                else            !not a user function

                   doV1=0.
                   HoldValue=calc_Math(OpType,flagopv,DoV1,HoldValue)  !dov1 is flag
                                        !New Holdvalue uses old Holdvalue
                endif                  !type of function

              endif                    !Return to Rs (3 types)
              goto 200                  !LS or RS subtree value for higher node

           endif                       !Return to LS or RS


        End

ccccccccccc NOTES

c NOTE 1
c  Function ( permanent functions,  and USER FUNCTIONS) and
c   IOPR "()" are handled here.
c IOPRS and Perm Functions are handled by calc_MATH, but the last (user function)
c requires some tricky, recursive type operations. Specifically, we
c must "recursively" process a  tree, using the value (HOLDVALUE) as
c  the value of variable(1) :: the "user function variable".

c NOTE 2
C NOTE:: DO_calc_USERFUNK will calc_PushS a "Ls_Funk" frame that points to the user
c  function node. This compares with a "Ls_None" frame that is
c  calc_Pushed when normal function, a ( ) operation, or "first
c  encounter with user-function" (prior to analysis)  occurs.
c  Therefore, after function analysis we return to
c  a frame that means "return from user function". This calls for
c  special treatment: to wit, we tell calc_math to do an IOPR operation,
c  instead of a FUNK (what we would do if we treated ls_funk the same as Ls_none.)
c  In other words, invocation  (of an analysis stack frame) after
c user-function analysis will be completely the same as (undifferentiated from)
c   return from a normal IOPR   OPERation ( "(" ).
c   This is necessary to avoid an endless loop.


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
C BUILD_TREE, ANALYZE_TREE, AND calc_GARBAGEMAN)

        SUBROUTINE calc_PushS(V1,Lstat,NADR,V2)


        PARAMETER (maxstak2=1750)

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
c           CALL ZERO_ANSTACK           !AND CLEAR STACK FOR FUTURE USE
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
C BUILD_TREE, ANALYZE_TREE, AND calc_GARBAGEMAN)

        SUBROUTINE calc_popS(V1,Lstat,NADR,V2)


        PARAMETER (maxstak2=1750)

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
             if (vls.eq.1.) then                !return after user function analysis
               call calc_pop_funkarg            !so remove the argument to this function.
             endif
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
               call Calc_Error(' ! 0/0 is undefined ')
               return
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
c       print *,' a_f ',analyze_funk
        RETURN
        end

c===================================================================
C Initialization for userfunction
c
C There are several things we must do
c
c NOTE::: We are using the "analysis" stack, as accessed by calc_popS and calc_PushS,
c         to save prior nodes (which act as subtree bases). Hence,
c         function analysis works by switching trees, WITHOUT changing
c         prior frames on this analysis stack. Upon switching trees
c         we start putting frames pointing to this new tree on analysis stack.
c         So, returning from function analysis is a simple process of
c         calc_popSing the analysis stack, eventually returning to
c         the stack frame containing  pointers to where
c         we left off in the original equation tree.  In other words,
c         the structure is fully recursive.
c
c
c  STEPS::
c      NODEF is the base node  of the user function (points up to original tree)
c       OPV will contain the user function number.
c   1) get the function address  :: calc_Get_FunkAddress(Ftype)
c               This function address will be the next node processed.
c  6) calc_PushS(0.,LsFUnk,NODEF,0.) The LsFunk
c     signals "returning a value that is the result of function analysis",
c    as opposed to "returning value FOR function analysis". A better name
c      would be LSFunkReturn.
c  7) Create a value node using opv, and attach this to the current
c     "argument stack".  Also, load the address on the stack into
c      varivalues(1). Therefore, when function tree wants argument, it
c      will look to varivalue(1) and find the pointer to the node containig
c      the desired value.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


        SUBROUTINE DO_calc_USERFUNK(NodeF,Ftype,ARgvalue,CurrentNode)

        PARAMETER (maxnode=6000)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL  *8 VALUE(MAXNODE)
        Integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        common /calc_cmerror/nerr


        REAL *8 ARGVALUE                !A VALUE
        REAL FTYPE                      ! FLAG
        Integer CurrentNOde
        REAL *8 ZERO
        DATA ZERO /0./

c CurrentNode is used in ANALYZE_TREE, it is the first node of the
c   function equation.
        CurrentNode=calc_Get_FunkAddress(Ftype) !the tree address of user function

c check to see if it was deleted
        if (currentnode.eq.0) then      !node was deleted
           call calc_error(' ! Accessing deleted function ')
           return
        endif

        call calc_PushS(ZERO,LsFunk,NODEF,0.)        !when this is calc_popSed, it will signal
                                        !return from User function analysis

        mvnode=make_valnode(ArgValue,VALU)

        if (nerr.gt.0.or.mvnode.eq.nil) then
           call Calc_Error(' ')
           return
        endif

        Karg=calc_get_funkarg()

        if (karg.ne.nil) then                   !not first function argument.
           Node(karg,rs)=Mvnode         !add to a very simple list
           node(mvnode,ls)=karg         !karg is parent of mvnode (dont need ls(
        endif


c else, this is first user defined function encountered, so there are no
c preceding function arguments. Hence, MVNODE's parent is NIL.

        call put_funkarg(mvnode)

        return
        end



c=============================================================================

c Function and variable manipulation routines



c-------------------------------------------------------------
c remove variable
        SUBROUTINE remove_VARINAME(HOLD0,istat)

        PARAMETER (MAXVARI=100)
        character *(*)hold0

        CHARACTER *12 VARINAMES(MAXVARI)
        REAL *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        call find_variname(hold0,aval)

        if (aval.le.0) then
          istat=1
        elseif (aval.le.3) then
          istat=2
        else
          varinames(int(aval))=' '
          istat=0
        endif
        return
        end

c-------------------------------------------------------------
C RETURN LOCATION OF VARIABLE (0 IF NO match)

        SUBROUTINE FIND_VARINAME(HOLD0,AVAL)
        PARAMETER (MAXVARI=100)
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

        PARAMETER (MAXVARI=100)
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


C----------------------------------------------------
c routine to load the name of the current functions's argumentname.
c
c this is used by calc_readequation, specifically, when a user defined
c function is being created.  Under normal equation analysis,
c this function is called with a ' ' (to indicate NO function argument )

        SUBROUTINE LOAD_FUNKARGNAME(ArgName)


        PARAMETER (MAXVARI=100)
        character *(*) argname

        CHARACTER *12 VARINAMES(MAXVARI)
        REAL *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        varinames(1)=argname

        return
        end


C----------------------------------------------------
c routine to remove the top of the function argument stack.
c Loads new function argument node into the function argument variable.

        Subroutine calc_pop_funkarg             !so remove the argument to this function.

        PARAMETER (maxnode=6000)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        karg=calc_get_funkarg()         !gets address of function argument node

        npar=node(karg,ls)              !parent in LS
        call calc_putfree(karg)

        call put_funkarg(npar)

c NOTE:: this will automatically put a nil back into function argument pointer
c  when there are no more active-function-arguments. This is because LS of a
c  valnode is nil until specifically filled, and LS of first (IN LIST)
C  function_argument_pointer is not so filled; hence it is nil(see
C  DO_calc_userfunk)

        return
        end
C----------------------------------------------------
c return the node containing current function argument.

        Function calc_get_funkarg

        PARAMETER (MAXVARI=100)

        CHARACTER *12 VARINAMES(MAXVARI)
        REAL *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        calc_get_funkarg=varivalues(1)
        return
        end
C----------------------------------------------------
c fill the pointer to the current function argument

        Subroutine Put_funkarg(nnode)

        PARAMETER (MAXVARI=100)

        CHARACTER *12 VARINAMES(MAXVARI)
        REAL *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        varivalues(1)=nnode

        return
        end

C----------------------------------------------------
c Return the address of function referenced by FDX

        Function calc_Get_FunkAddress(FDX)

        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)

        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKargs(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,Funkargs


        NF=abs(fdx)-npermfunk

        calc_get_funkaddress=funkaddress(nf)

        return
        end


c===========================
c remove function by setting address to zero: istat=0 ok,1=no such,2=permanent

        subroutine remove_funk(funcname,istat)

        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)

        CHARACTER *(*) funcname

        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        CHARACTER *12 FUNKargs(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        call find_func(funcname,aval)
        if (aval.eq.0) then
                istat=1
        elseif (aval.le.npermfunk) then
                istat=2
        else
                ntmp=int(aval-npermfunk)
                call calc_putfree(funkaddress(ntmp))
                funkaddress(ntmp)=0
                istat=0
        endif
        return
        end

c----------------------------------------
c function to create new function index slot.

        subroutine set_funk(username,node,string)

        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)

        CHARACTER *(*) username,string

        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        CHARACTER *12 FUNKargs(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        if (nfunk.ge.maxfunk) then

c look for emptied slots.
           do ij=npermfunk+1,nfunk
              if (funkaddress(ij-npermfunk).eq.0) then
                  iuse=ij
                  goto 30
               endif
            enddo
            call Calc_Error(' ! Too many functions defined ')
            return
         endif

c else, we have room

        nfunk=nfunk+1
        iuse=nfunk
 30     continue
        call fill_funk(iuse,username,string,node)


        return
        end


c----------------------------------------
c routine to put a function name, string and address into function indices
c Mvari is the location in the function index, username is its name, aname is
c argument name  and node is address of start of the ]tree defining function
        Subroutine Fill_funk(mvari,username,aname,node)

        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)

        CHARACTER *(*) username,aname

        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        CHARACTER *12 FUNKargs(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        funknames(mvari)=username
        funkargs(mvari-NPERMFUNK)=aname
        funkaddress(mvari-npermfunk)=node


        return
        end

c==========================================================
C Search for index, given function name (return in aval)
        SUBROUTINE  FIND_FUNC(HOLD0,AVAL)     !AVAL POINTS TO FUNCTION TABLE

        PARAMETER (MAXFUNK=59)
        PARAMETER (NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)

        CHARACTER *(*) HOLD0

        character *12 hold,tmp2
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKargs(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs


        logical qradians
        common /trigtype/qradians

        DATA NFUNK /NPERMFUNK/          !START WITH # OF PERMANENT FUNCTIONS

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

        DO IJ=1,NFUNK
          tmp2=funknames(ij)
          if (ij.gt.npermfunk) then
             if (funkaddress(ij-npermfunk).eq.0) then
                cycle
             endif
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
c get the name use for the function argument
        character *(*) function get_funk_argname(fnum)

        PARAMETER (MAXFUNK=59)
        PARAMETER (NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)


        character *12 funkargs(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        integer funkaddress(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        if (fnum.le.npermfunk) then
             call calc_error(
     1         ' ! No argument name for permanent functions')
                get_funk_argname=' '
        elseif  (fnum.gt.nfunk) then
           call calc_error(' !No argname for non existent function ')
           get_funk_argname=' '
        else            !okay
            get_funk_argname=funkargs(int(fnum)-npermfunk)
        endif
        return
        end


c============================================================
c return function name given location

        Character *(*) Function get_funkname(AVAL)

        PARAMETER (MAXFUNK=59)
        PARAMETER (NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)


        character *12 funkargs(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        integer funkaddress(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs


        if (aval.gt.nfunk.or.aval.le.0) then
           call calc_error(' !non existent function ')
           return
        else
           ij=aval
           get_funkname=funknames(ij)
        endif

        RETURN
        END


