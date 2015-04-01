C SECOND HALF OF CALCM PROGRAM
c=======================================================================
C  A CALCULATOR PROGRAM -- Regular Version
c @c for the public domain. D. Hellerstein, ERS/RTD, 12/91, 202-19-0444
c              NOTE TO PROGRAMMERS
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

        PARAMETER (maxnode=1400)
        PARAMETER (maxstak2=400)
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


        PARAMETER (maxstak2=400)

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


        PARAMETER (maxstak2=400)

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

        PARAMETER (maxnode=1400)

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

        PARAMETER (maxnode=1400)

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

c==========================================================
c routine to fill variable value  and to return variable name
c NOTE:: we don't fill varivalue(1) (function argument) here.
c  See Put_funkarg.
c Note:: mvari can be ANY value, hence to prevent overwrite of
c first 3 locatons (function value,Pi, and E) you must check
c prior to calling Calc_putvari (eg; this is done in calc_equation)

        subroutine CALC_putvari(mvari,vname,aval)

        parameter (MAXVARI=100)

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

        parameter (MAXVARI=100)
        PARAMETER (maxnode=1400)

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

        parameter (MAXVARI=100)
        PARAMETER (maxnode=1400)

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

        parameter (MAXVARI=100)
        CHARACTER *12 VARINAMES(MAXVARI)
        real *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

        CALC_NUMBVARS=NVARI             !3 PERMANENT VARIABLES

        RETURN
        END





c==================================


c=============================================================================
c  garbage collection routines
c
C----------------------------------------------------
C ROUTINE TO CONSIGN THE nodes of the function at mvari to freelist

        SUBROUTINE calc_garbageman      !garbage_funk calls calc_garbageman

        IMPLICIT NONE
        INTEGER MAXNODE,MAXFUNK,NPERMFUNK,NTMPFUNK,maxstak2

        PARAMETER (maxnode=1400)
        PARAMETER (maxstak2=400)
        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)

        integer funkaddress(ntmpfunk)
        character *12 funkargs(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs
        INTEGER NFUNK,NNODE
        integer zeronode,onenode,twonode,tennode,negonenode
        common /globalnodes/
     1       zeronode,onenode,twonode,tennode,negonenode

        integer freenodes
        common /calc_freenode2/freenodes


c garbage collection structure
        Integer *1 DIRTYnode(MAXNODE)
        common /calc_DIRTY/dirtynode

        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        integer *2 node(MAXNODE,2)              !4 POINTERS:
        common /calc_NODES/NODE,LS,RS,NIL

        INTEGER IJ,nerr
        common /calc_cmerror/nerr



        if (freenodes.gt.400) then
          return                        !dont bother
        endif

        do ij=1,maxnode
           dirtynode(ij)=0
        end do

        if (nfunk.Le.npermfunk) then            !no functions to mark
           GOTO 200
        else
          do ij=npermfunk+1,nfunk
             nnode=funkaddress(ij-npermfunk)
             call calc_marknodes(nnode)
             if (nerr.gt.0) return
          end do
        endif


        call calc_marknodes(zeronode)
        call calc_marknodes(onenode)
        call calc_marknodes(twonode)
        call calc_marknodes(negonenode)
        call calc_marknodes(tennode)

c now, all currently active nodes should be okay
 200    CONTINUE
        freenodes=0

        do ij=1,maxnode
           if (dirtynode(ij).eq.0) then
             call calc_putfree(ij)
           endif
        end do

        return
        end

c===================================================================
C
C Mark as non-garbage

        SUBROUTINE calc_marknodes(nNODE)


        PARAMETER (maxnode=1400)

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

c garbage collection structure
        Integer *1 DIRTYnode(MAXNODE)
        common /calc_DIRTY/dirtynode

        INTEGER nerr
        common /calc_cmerror/nerr

        Integer Ps,Pn,From,Type
        real type2

        integer n1,n2

        if (nnode.eq.nil) return        !aviod errors


        Type=Opr(nNode)         !get type of base of subtree

c it is a single leaf (not really a tree)

        if (Type.eq.valu.or.type.eq.vari) then  !At a leaf?
           goto 500                             ! Then we are at end of subtree !
        endif

c else, place break on stack
        call calc_pushd(nil,nil,nil,nil)        !bottom of stack

c else, it is a subtree
c Almost always, Nnode will be a FUNK or an IOPR (usually an IOPR)

        type2=Dabs(value(nnode))                !-IOPR used for negative "-(9)"

        If (type.eq.funk.or.(type.eq.oper.and.type2.eq.Iopr)) then
            call calc_Pushd(nil,rs,nnode,nil)           !save parent/side
            From=Node(nnode,rs)        !no left tree if funk or "(" ( an "IOPR")
        else                            !a MATH OPER
            call calc_Pushd(nil,ls,nnode,nil)           !save parent/side
            From=Node(nnode,ls)         !so do left side first
        endif


c  Done with first node, now start tree processing

 100    continue
        if (nerr.gt.0) return

        type=opr(from)

        if (type.eq.vari.or.type.eq.valu) then
           dirtynode(from)=1
           call calc_popd(n1,ps,pn,n2)          !get parent/side
           goto 200                     !goto parent..
        elseif (type.eq.nil) then              !ignore this node
           call calc_popd(n1,ps,pn,n2)          !get parent/side
           goto 200                     !goto parent..
        else
           type2=Dabs(value(from))              !-IOPR used for negative "-(9)"

           If (type.eq.funk.or.(type.eq.oper.and.type2.eq.Iopr)) then
               call calc_Pushd(nil,rs,from,nil)         !save parent/side
               From=Node(From,rs)               !no left tree if funk or (
           else                         !OPER
               call calc_Pushd(nil,ls,from,nil)         !save parent/side
               From=Node(From,ls)
           endif
           goto 100                     !continue down tree
        endif

c here if after dirtying a node.  If the PS (gotten from marked node)
c  is RS, then the parent node has ALL subtrees marked, so mark the
c parentnode (PN). If PS=LS, we still must do right side.

 200    continue
        if (nerr.gt.0) return

        if (pn.eq.nil) then             !back to base of subtree? THen all done
            goto 500
        elseif (Ps.eq.RS) then  !Parent's subtree filled, so go back up again
           dirtynode(pn)=1
           call calc_popd(n1,ps,pn,n2)          !get parent/side
           goto 200             !process this new parent.
        else                            !coming back to left, so go get PN's
           call calc_Pushd(nil,rs,pn,nil)               !get parent/side
           From=Node(Pn,Rs)             !RS
           Goto 100             !(and check the RS subtree)
        endif



c here if we have returned to base node

 500    continue

        dirtynode(nnode)=1

        return

        end

C=================================================
C ZERO ANALYSIS STACK

        SUBROUTINE ZERO_ANSTACK

        PARAMETER (maxstak2=400)

        INTEGER *2 STKNODE(MAXSTAK2)
        Integer *2 LsStats(MAXSTAK2)
        REAL *8 VALL(MAXSTAK2)          !holds value
        REAL VALR(MAXSTAK2)             !could be integer *2
        INTEGER NSTACK2
        common /calc_STACK2/NSTACK2,VALL,LsStats,STKNODE,VALR
        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        nstack2=0
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


        SUBROUTINE calc_init


        PARAMETER (maxnode=1400)
        parameter(NPERMFUNK=25)
        PARAMETER (MAXFUNK=59)
        parameter(ntmpfunk=maxfunk-npermfunk)
        parameter (MAXVARI=100)

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

c garbage collection structure
        Integer *1 DIRTYNODE(MAXNODE)         !USED FOR CLEANUP
        common /calc_DIRTY/DIRTYNODE

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
c functions (permanent and user defined)
        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        CHARACTER *12 FUNKargs(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

c nodes needed by deriv routines
        real *8 v8
        integer zeronode,onenode,twonode,tennode,negonenode
        common /globalnodes/
     1       zeronode,onenode,twonode,tennode,negonenode

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
     2                 'SINH','COSH','TANH','FACT',
     2                 NTMPFUNK*' '/




        qradians=.false.

        nfunk=npermfunk
        nvari=3                 !3 is number of permanent variables

        DO IJ=1,MAXNODE-1
           NODE(IJ,rs)=IJ+1
           VALUE(IJ)=0.
           OPR(IJ)=NIL
           DIRTYNODE(IJ)=0              !UNUSED
        END DO
        OPR(MAXNODE)=NIL
        VALUE(MAXNODE)=0.
        NODE(MAXNODE,RS)=NIL
        DIRTYNODE(MAXNODE)=0

        NEXTFREE=1

c make some nodes needed by DERIV function.
        v8=0.
        zeronode=make_valnode(v8,valu)
        v8=1.
        onenode=make_valnode(v8,valu)
        v8=-1.
        negonenode=make_valnode(v8,valu)
        v8=2.
        twonode=make_valnode(v8,valu)
        v8=10.
        tennode=make_valnode(v8,valu)



        RETURN
        END



C=========================================================

        FUNCTION calc_GETFREE()

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=1400)

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
           call calc_error(
     1 ' ! CALC workspace filled, try deleting functions with /INIT')
         endif

c       ELSE , got space

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

        PARAMETER (maxnode=1400)

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

        PARAMETER (maxnode=1400)

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

        PARAMETER (maxnode=1400)

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

c------------------
c swap ls and rs branches of anode
        subroutine swap_branches(anode)

        implicit none
        integer anode,ileft,iright,get_left,get_right

        ileft=get_left(anode)
        iright=get_right(anode)
        call left_assign(anode,iright)
        call right_assign(anode,ileft)
        return

        end


c==========================================================
C  Create a Value Node, NIL if no more nodes
c  If atype is VARI, then create a value node using the curren
c  value of the variable indexed by aval.

        INTEGER  Function Make_valnode(Aval,atype)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=1400)

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

        PARAMETER (maxnode=1400)

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

c============================================
c functions to tell what type of node this is

        logical function is_math(nno)
        PARAMETER (maxnode=1400)

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        logical is_neg,is_vari,is_valu,is_iopr,is_func,is_vvf
        logical is_vvfi,is_math2

        character *8 aa

c Is it a math node ( + - / * ^ % )
        is_math=.false.
        aa='ismath'
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        if (opr(nno).eq.oper.and.dabs(value(nno)).ne.iopr) then
           is_math=.true.
        endif
        return

c Is it a particualr math node ( + - / * ^ % )
        entry is_math2(nno,ntype)
        is_math2=.false.
        aa='ismath2'
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        if (opr(nno).eq.oper.and.dabs(value(nno)).eq.ntype) then
           is_math2=.true.
        endif
        return

c Is it an Iopr node ( (
        entry is_iopr(nno)
        aa='isiopr'
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        is_iopr=.false.
           if (opr(nno).eq.oper.and.dabs(value(nno)).eq.iopr) then
           is_iopr=.true.
        endif
        return

c is it a funk node
        entry is_func(nno)
        aa='isfunc'
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        is_func=.false.
        if (opr(nno).eq.funk) then
           is_func=.true.
        endif
        return

c is it a valu node
        entry is_valu(nno)
        aa='isvalu'
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        is_valu=.false.
        if (opr(nno).eq.valu) then
           is_valu=.true.
        endif
        return

c is it a vari node
        entry is_vari(nno)
        aa='isvari'
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        is_vari=.false.
        if (opr(nno).eq.vari) then
           is_vari=.true.
        endif
        return

c is it a negative node ( nb: false if a math node )
        entry is_neg(nno)
        aa='isneg'
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        is_neg=.false.
        if (opr(nno).eq.oper.and.dabs(value(nno)).ne.iopr) then
           continue                             !false
        elseif (value(nno).lt.0) then
           is_neg=.true.
        endif
        return

c is it valu,vari,or funk
        entry is_vvf(nno)
        aa='isvvf '
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        is_vvf=.false.
        if ( opr(nno).eq.valu .or.
     1        opr(nno).eq.vari .or. opr(nno).eq.funk) then
              is_vvf=.true.
        endif
        return

c is it valu,vari,or funk
        entry is_vvfi(nno)
        aa='isvvfi '
        if (nno.le.0 .or. nno.gt.maxnode) goto 3000
        is_vvfi=.false.
        if ( (opr(nno).eq.valu) .or.
     1       (opr(nno).eq.vari) .or.
     1       (opr(nno).eq.funk) .or.
     1       ( (opr(nno).eq.oper).and.(dabs(value(nno)).eq.iopr) )
     1                           ) then
              is_vvfi=.true.
        endif
        return



 3000 continue
        print *,' Error, bad node # in ',aa, 'node=',nno
        if (aa.eq.'ismath2') print *,' oper=',ntype
        stop

        end


c============================================================
        INTEGER  Function Make_Funknode(FunkTYpe)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=1400)

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

        PARAMETER (maxnode=1400)
        PARAMETER (maxstak2=400)

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
           if (dabs(ans).lt.1.0) then    !try to add 0
              im=index(charans,'.')
              if (im.gt.1) then
                 if (ans.gt.0) then
                     charans(im-1:im-1)='0'
                 else
                    if (im.gt.2) then
                       charans(im-2:im-1)='-0'
                    endif          !add -0
                 endif          !add 0
              endif             !no room at all
           endif        !abs(ans)<0

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


c=============================================================
c clean up string. Gets rid of spaces and other junk characters,
c capitalizes, and converts [{ and ]}

        Subroutine calc_cleanup_strung(String,nospace)


        CHARACTER *(*) STRING
        CHARACTER *1 ACHAR,CVTUCASE

        llen=len(string)
        IF (STRING(1:1).EQ.'=') STRING=STRING(2:LLEN)   !IGNORE FIRST CHAR "="

        IJJ=0
        nlen=LEN_TRIM(string)

       DO IJ=1,NLEN                    !CLEAN UP STRING
          ACHAR=STRING(IJ:IJ)
          if (nospace.eq.1) then
              IF (ACHAR.EQ.' '.OR.ACHAR.EQ.';'.OR.ACHAR.EQ.':') THEN
                 cycle
              else
                 IJJ=IJJ+1
                 STRING(IJJ:IJJ)=CVTUCASE(ACHAR)
              endif
          ELSE
             IJJ=IJJ+1
             STRING(IJJ:IJJ)=CVTUCASE(ACHAR)
          endif
        END DO

        call captaliz(string)           !capitalize, retain blanks

        NLEN=IJJ

        DO IJ=1,NLEN
           ACHAR=STRING(IJ:IJ)
           IF (ACHAR.EQ.'['.OR.ACHAR.EQ.'{') ACHAR='('
           IF (ACHAR.EQ.']'.OR.ACHAR.EQ.'}') ACHAR=')'
           STRING(IJ:IJ)=ACHAR
        END DO
        string(Nlen+1:Llen)=' '
        return

        END


C==================================
C  Routine to convert a tree into a string, sort of the
c opposite of readequation & Build_tree.
c Ndec and qe used to display numbers. Base is the address of tree
c  Fstring will contain the "tree" string (fstring is returned)


        Subroutine Tree_string(fstring,base,ndec,qe)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=1400)
        PARAMETER (maxstak2=400)
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
c Fstring is string. Inum will be returned as old inum+messlen(substring)
c  Hence, inum is returned as next free space.

        SUBROUTINE calc_PUT_OUT(Inum,CHARA,fstring)

        CHARACTER *(*)CHARA,fstring

        il=len(fstring)
        if (inum.gt.il) return          !cant fit any of substring

        IC=MESSLEN(CHARA)
        IF (IC.EQ.0) IC=1       !A SPACE

        if (inum+ic.gt.il) ic=il-inum   !cant fit all of substring


        fstring(inum:inum+ic-1)=chara(1:IC)

        inum=inum+ic            !return next free space

        RETURN
        END



