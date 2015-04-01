c@cfor the public`domain, D. Hellerstein, ERS/RTD 12/91
C=================================================
c simplify a function (ANAME=NAME OF FUNCTION TO SIMPLFY
        subroutine simplify_it(aname,level,simpname0)

        parameter(NPERMFUNK=25)

        character *(*)aname,simpname0

        character *12 simpname,simparg,get_funk_argname
        character *3 calg

        common   /cm_simp/
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra
        integer
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra


        logical is_neg,is_iopr,is_func,qagin
        integer icomp,donealg

        integer ioptype,inls,inrs,lsstat,OLDSIMP,get_right
        real *8 opval8
        real *8 opval8j
        integer kollapse_tree,TMPNODE
        logical is_ioprpos,iseq_neg
        integer cd_oprn2

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        integer dosimple2
        common /cm_simp2/dosimple2

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        common /calc_CMERROR/NERR


        integer *2 getchasm,ifoo2
        integer dosims(7),tmpsims(7),ij

        equivalence(dosims,doiter)

c If level ne 0, then set some flags

        do ij=1,7                   !save current state
            tmpsims(ij)=dosims(ij)
        enddo

c set state for this attempt (level=0 means use defaults set by /INIT SIM )
        if (level.ne.0) then
           do mm=1,7                    !reset to 0
             dosims(mm)=0
           enddo
           if (level.ge.1) then
                doparen=1
           endif
           if (level.ge.2) then
                domath=1
                doneg=1
           endif
           if (level.eq.3) then
                doiter=1
                dosimple=1
                doalgebra=1
                dofunc=1
           endif
        endif

        dosimple2=1
        if (doalgebra.eq.1) then      !turn on one latter iteration
            dosimple2=0
        endif


C Clean up old trees.
        CALL CALC_GARBAGEMAN(0)
        call zero_anstack
        nerr=0
        nodesimp2=0
        OLDSIMP=0
        simpname=simpname0        !avoid problems

c IF level<0, then we are copying a tree
        if (level.lt.0) then
          nodesimp=copy_node(iabs(level),0)
          goto 1000
        endif

c else, look up name, set name, etc.
        call find_func(aname,fnum)   !FIND id
        if (fnum.eq.0) then
                   print *,' No such function:',ANAME(1:LEN_TRIM(ANAME))
                   goto 6000
        elseif (fnum.le.npermfunk) then
                call calc_error(' Can`t simplify permanent function')
                goto 6000
        else
           numf=calc_Get_FunkAddress(Fnum)        !the tree address of function
           simparg=get_funk_argname(fnum)
        endif

        if (simpname.eq.' ') then
            simpname=aname(1:min(len_trim(aname),10))//'_S'   !the name to use
        endif                   !else use given name

c ========  Copy the old function to new location

c Start copying loop.

        call find_func(simpname,simpnum)   !FIND id
        if (simpnum.eq.0) then                  !no prior name
           nodesimp=copy_node(numf,0)           !copy basic info
           call set_funk(simpname,nodesimp,simparg)
           call find_func(simpname,simpnum)   !might need it below
        else                            !use prior id
           nodesimp=calc_Get_FunkAddress(simpnum)
           nodesimp=copy_node(numf,nodesimp)
           call fill_funk(int(simpnum),simpname,simparg,nodesimp)
        endif

        write(6,551)aname(1:len_trim(aname)),
     1     simpname(1:len_trim(simpname)),simparg(1:len_trim(simparg))
 551    format(2x,' Simplifying ',a,' w/name=',a,'(',a,')'\)

c  JUMP here if level<0 (copy_tree call)
 1000   continue

        ithpass=1
        donealg=1      !start w/o algebra, then wait

c ==============================   START 'RECURSION'
c note: jump here on later iterations

  4     continue

        call get_nodeinfo(nodesimp,ioptype,opval8,inls,inrs)

C special case: tree consists of just one node
        if (inrs.eq.nil) then           !all done
           call down_line(1)
             goto 6000
        endif

c   else .. not so simple. Prepare to recurse tree
        CALL calc_PUSHd(NODESIMP,inls,nil,nil)   !NIL IN LSSTAT MEANS TOP NODE

        nodedo=nodesimp
        newnode=nodedo
        goto 12                 !got info, some skip a command

 10     continue                ! here if traversing down tree
c when going down, the node has never been encountered.
c Thus, make a copy of it.  Note that we will be changing
c the ls and rs of the copy (if necessary)


        newnode=copy_node(nodedo,0)             !copy its basic info
        call get_nodeinfo(newnode,ioptype,opval8,inls,inrs)

        flagvl=set_flagval(ioptype,opval8)

 12     continue                        !jump here at start of recursion

        if (nerr.gt.0) then
            call down_line(1)
            goto 6000
        endif

c at a terminal node.           !note: top node is NEVER terminal.
        if (inrs.eq.nil) then          !Terminal node?
           call calc_popd(nodedo,inls,inrs,lsstat) !get parent
c note: if something in ls, then ALWAYS something in rs
           if (lsstat.eq.lsnotdone) then        !belongs to ls
                call left_assign(nodedo,newnode)  !nodedo is a copy
                call calc_pushd(nodedo,newnode,inrs,lsdone)
                nodedo=inrs
                goto 10
           elseif (lsstat.eq.lsnone) then  !belongs to rs
                call right_assign(nodedo,newnode) !here, nodedo is the copy
                newnode=nodedo             !this subtree is the real newnode
                goto 20                 !no need to read nodedo info
           elseif (lsstat.eq.lsdone) then
                call calc_pushd(nodedo,inls,newnode,lsdone)
                goto 20                 !no need to read nodedo info
           else
               call down_line(1)
               call calc_error(' ! Impossible status of Terminal node')
               goto 6000
           endif
        endif                           ! processing of terminal node


c a non-math, non terminal node
        if (is_func(newnode).or.is_iopr(newnode)) then
            call calc_pushd(newnode,inls,inrs,lsnone)
            nodedo=inrs                 !examine RS
c Check on simplification [basic (( collapsing, but not (-( collapsing ]
c            if ( (doparen.eq.0) .or. is_neg(nodedo) .or.
c    1               (.not.(is_iopr(nodedo)))
c    1                ) then                   !do not try to remove extra (s
             if ( (doparen.eq.0) .or. iseq_neg(nodedo) .or.
     1               (.not.(is_iopr(nodedo)))
     1                ) then                   !do not try to remove extra (s
                   goto 10
             endif
c  ...   else, remove extra (
  42        call get_nodeinfo(nodedo,ioptype,opval8,inls,inrs)
            if (is_iopr(inrs)) then    !remove it
                nodedo=inrs
                goto 42          !keep looking
            else
                goto 10
            endif
        endif


c Math Node:  (both ls and rs),
        call calc_pushd(newnode,inls,inrs,lsnotdone)  !prepare to do ls branch
        nodedo=inls
        goto 10

c Jump Here if returning from NON-terminal (node==NewNode):
c  1- after processing parent of terminal node (above)
c  2- After returning from a subtree (below)
c nb: newnode is rememberd and used   below
 20     continue
        if (nerr.gt.0) then
             call down_line(1)
              goto 6000
        endif
        call calc_popd(nodedo,inls,inrs,lsstat) !get parent
        if (lsstat.eq.lsnotdone) then      !returning from ls
           call left_assign(nodedo,newnode)
           call calc_pushd(nodedo,newnode,inrs,lsdone) !prepare to recurse down
           nodedo=inrs
           goto 10

        elseif (lsstat.eq.lsdone.or.lsstat.eq.lsnone) then  !returning from rs

           call right_assign(nodedo,newnode)
c see if artihmetic type simplifiation can be done (IF SO AND SUCCESSFUL
C  the value of newnode will be adjusted
           newnode=kollapse_tree(nodedo)

            if (nerr.gt.0) then
               call down_line(1)
               goto 6000
            endif


c See if a tree can
c be collapsed -- the point is that we do NOT want to remove
c IOPRs if they are attached to a function
           call get_nodeinfo(newnode,jtype,opval8j,jls,jrs)
           if (.not.is_func(newnode)) then    !cleanup possible ls
                   jrs=kollapse_iopr(jrs,0)
                   call right_assign(newnode,jrs)
           endif
           jls=kollapse_iopr(jls,0)           !but funk never points to LS
           call left_assign(newnode,jls)

c repeat these if necessary
           ITHPASS2=1
 2020      CONTINUE                     !REPEAT JUMPS HERE
           if (doiter.gt.0) then
              tmpnode=copy_node(newnode,0)
           endif
           if (donealg.eq.1) then          !is it being suppressed
              newnode=simple_algebra(newnode)  !do simple algebraic
              newnode=real_algebra(newnode) !do tougher algebra
           endif
           IF (DOITER.GT.0.AND.ITHPASS2.LT.3) THEN
                IF (KOMPARE_TREES(TMPNODE,NEWNODE).EQ.0.) THEN
                      ITHPASS2=ITHPASS2+1
                      GOTO 2020
                 ENDIF
            ENDIF

c a hack, but at this point if Func, and argument is -(, make sure
c it is F( -(
           if (is_func(newnode)) then             ! F
                inrs=get_right(newnode)
                if (.not.is_ioprpos(inrs)) then   ! F-(
                    inrs=cd_oprn2(inrs)       !F(-(
                    call right_assign(newnode,inrs)
                endif
           endif

           goto 20             !back up tree


        elseif (lsstat.eq.nil) then     !back to base of new tree
              nodesimp=kollapse_iopr(newnode,1)  !heavy duty collapse
                                                !removes all ( )

              if (nerr.gt.0)  then
                 call down_line(1)
                 goto 6000
               endif

              IF (LEVEL.GE.0) THEN
                 call fill_funk(int(simpnum),simpname,simparg,nodesimp)
                 call calc_garbageman(0)
             ENDIF

c more then one iteratoin?
                if (doiter.le.0) then
                    call down_line(1)
                     goto 6000
                endif

c else, second or greater iteration

c NOTE: 40 is the maximum iterations, per run (you can do it again).
                if (ithpass.ge.40) then !taking tooo long
                    call down_line(1)
                     print *, ' Attention: ',
     1      '   Full simplification NOT achieved after 40 iterations.'
                    goto 6000
                endif

c if here, maybe do another ...
c cases:
c doalgebra=0, icomp=1: done
c              icomp=0: repeat
c doalgebra=1, icomp=1
c                      donealg=1:   done
c                      donealg=0:  donealg=1,repeat
c              icomp=0: donealg=0, repeat

               icomp=kompare_trees(nodesimp,OLDSIMP)  !equal ?

c if doalgebra, do one more time without algebra

               if (doalgebra.eq.0) then         !no algebra
                   if (icomp.eq.0) then         !no match
                      qagin=.true.
                   else                         !match, done
                      qagin=.false.
                   endif
               else                     !algebra situation is trickier
                   if (icomp.eq.1) then         !match
                       if (donealg.eq.1) then   !after algebra
                           qagin=.false.        !done
                       else                     !try again with algebra
                            qagin=.true.
                            donealg=1
                            dosimple2=0
                       endif
                   else                         !no match
                       donealg=0                !try without algebra
                       dosimple2=1
                       qagin=.true.
                   endif
               endif

c success, or user abort
               if (.not.qagin) then
                   call down_line(1)  !note, result always saved at end of iter
                        print *,
     1            ' Simplification achieved on iteration# ',ithpass-1
                   goto 6000
               else
                  do ij=1,255
                     ifoo2=getchasm()
                     if (ifoo2.ne.0) exit
                  enddo
                  if (ifoo2.eq.27) then
                     call down_line(1)
                     print *,' Simplification stopped by user.'
                     goto 6000
                  endif
               endif

c if here, do it again
              ithpass=ithpass+1
              nodesimp2=copy_node(nodesimp,0)  !
              OLDSIMP=NODESIMP
              call down_line(0)
              call del_to_end
              calg='w/o'
              IF (DONEALG.EQ.1)CALG='w/'
              write(6,5511)
     1         simpname(1:len_trim(simpname)),ithpass,cALG
 5511         format(2x,' Simplifying ',a,'.',4x,
     1                  ' Iteration # ',i3,', ',a,' algebra',
     1                  ' (ESC to stop)'\)

              nodesimp=nodesimp2
              goto 4                  ! AND RECURSE !!

        endif                           !lstat eq nil

        print *,' Impossible in Simplification '
        stop

c--------
 6000   continue                !reset simplificaiton
        do ij=1,7
           dosims(ij)=tmpsims(ij)
        enddo
        if (level.lt.0) level=nodesimp  ! copy_tree call
        return

        end

c===============================
c Copy a tree, starting at a node.  Uses special features of

        integer function copy_tree(nnode)    !copy the tree first.
        implicit none
        integer level,nnode

        level=-nnode
        call simplify_it(' ',level,' ')

        copy_tree=level
        return
        end

c===============================
c simplify a tree, level 2 (collapse ((s, do simple math
c a guiding assumption of this routine is that all trees lower
c have been 'reasonably' collapsed


        integer function kollapse_tree(nodedo)

        parameter(NPERMFUNK=25)

        common   /cm_simp/
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra
        integer
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra


        integer nil                    !needed by calc_routines
        common /calc_cmnil/nil

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        real *8 theval8,val8a,val8b,val8l,val8r,val8c,calc_math
        integer opa,opb,theoper,opl,opr,the_newnode,isign

        common /calc_CMERROR/NERR

        logical is_vari,is_valu,is_iopr,is_math

c look at/simplify both sides of the tree at nodedo
        call get_nodeinfo(nodedo,theoper,theval8,inls,inrs)

        flagval=set_flagval(theoper,theval8)
        iflagval=int(flagval)



c ========= if valu or vari, just return
        if (is_valu(nodedo) .or. is_vari(nodedo)) then
            goto 3000
        endif

c === if no simplification switches turned on
        if (doiter+doparen+doneg+domath+dofunc+dosimple+doalgebra
     1          .le.0) then
           goto 3000
        endif

c ========= if (, try to collapse
        if (is_iopr(nodedo) .and.doparen.eq.1) then
           call ferret_iopr(nodedo,nnode,isign)
           nnode=copy_Node(nnode,0)
           newnode=make_oprnode(float(isign*iopr))
           call right_assign(newnode,nnode)
           kollapse_Tree=newnode
           return
        endif                   !

c ======== if Funk,  then try to analyze it
        if (theoper.eq.funk.and.abs(flagval).le.npermfunk
     1                     .and.dofunc.eq.1) then !perm function
           nnode=inrs

           call get_nodeinfo(nnode,opa,val8a,inlsa,inrsa)  !should be (
           flaga=set_flagval(opa,val8a)
           if (.not.(is_iopr(nnode))) then
                call calc_error(' ! ( does not follow function. ')
                goto 3000
           else
             call get_nodeinfo(inrsa,opb,val8b,inlsa,inrsa)
             if (opb.eq.valu) then
                 val8c=calc_Math(theoper,flagval,val8b,val8b)  !nb:vls not used
                 asign=sign(1.0,flagval)
                 mtmp=make_valnode(val8c*asign,valu)
                 if (mtmp.eq.nil ) then
                    call calc_error(
     1               ' ! Out of memory on Function simplifcation')
                     goto 3000
                 else
                     the_newnode=mtmp
                     goto 2000
                 endif                  !valnode not nil
             else                                 !only funk(val) simplifed
                goto 3000
             endif
           endif                        ! ( appears after funk

           call calc_error(' Unexpected location, Kollapse @ funk ')
           return                       ! did not change

        endif                           !node is function


c ===== If Math operator, get values.  Note: strip 1 ( ;
c so (val) + val2  will be done, but NOT ((val)) + val2

        if (is_math(nodedo).and.(domath.eq.1) ) then ! math operation

c get values from both ls and rs -- but strip 1 paren if necessary
           call get_nodeinfo(inls,opl,val8l,inlsl,inrsl)
           flagl=set_flagval(opl,val8l)
           nodebadl=0
           if (is_iopr(inls)) then      !get (val)
               call get_nodeinfo(inrsl,opa,val8a,inlsa,inrsa)
               if (opa.eq.valu) then
                   opl=valu
                   val8l=val8a*sign(1.0,flagl)
                   nodebadl=inrsl
                endif
           endif
           call get_nodeinfo(inrs,opr,val8r,inlsr,inrsr)
           flagr=set_flagval(opr,val8r)
           nodebadr=0
           if (is_iopr(inrs)) then      !get (val)
               call get_nodeinfo(inrsr,opa,val8a,inlsb,inrsb)
               if (opa.eq.valu) then
                   opr=valu
                   val8r=val8a*sign(1.0,flagr)
                   nodebadr=inrsr
                endif
           endif
c do we have two values?
           if (is_valu(inls) .and. is_valu(inrs)) then   !we can to math
                 val8b=calc_Math(theoper,flagval,val8l,val8r)
                 mtmp=make_valnode(val8b,valu)
                 if (mtmp.eq.nil ) then
                    call calc_error(
     1               ' ! Out of memory on math simplifcation')
                     goto 3000
                 else
                     the_newnode=mtmp
                     goto 2000
                 endif                  !valnode not nil
           else                         !cant do math
                goto 3000
           endif

           call calc_error(' Unexpected location, kollapse @ math')
           return

       endif

c=============== No simplificaiton  techniques to  try
        goto 3000

c========== replace contents of nodedo
 2000   continue                !modify nodedo
        nodedo=copy_node(the_newnode,nodedo)
        kollapse_tree=nodedo
        return

c  === return as is
 3000   continue
        kollapse_tree=nodedo
        return

      end

c===============================
c collapse Ioprs.  This assumes that the iopr node is NOT connected
c to a function.  It will also attempt to distribute and - sign
c into a math node (that lies at the bottom of the iopr list): eg
c convert (-(8*PI)) to (-8*PI).

        integer function  kollapse_iopr(nodedo,forceit)  !heavy duty


        common   /cm_simp/
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra
        integer
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        common /calc_CMERROR/NERR

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        logical is_neg,is_iopr,is_vvf
        logical qdidit,make_itneg

        real *8 theval8,val8a,val8b
        integer opa,theoper,usenode,forceit

c look at/simplify both sides of the tree at nodedo -- but is it nil?
        if (nodedo.eq.nil) then
            kollapse_iopr=nodedo
            return
        endif

c dont want to move negatives
        if (doneg*doparen.le.0.and.forceit.lt.2) then  !forceit>1 overrules
            kollapse_iopr=nodedo                        !doneg & doparen
            return
        endif

        call get_nodeinfo(nodedo,theoper,theval8,inls,inrs)
        flagval=set_flagval(theoper,theval8)


c NOTE: we assume that any IOPR node is NOT connected to a function:
c eg; not SIN (

c FIRST ::  if not (, return
        if (.not.(is_iopr(nodedo))) then
            kollapse_iopr=nodedo
            return
        endif

c eLse, scan down list of ((s
        isneg=1
        if (is_neg(nodedo)) isneg=-isneg        !store sign of this (
        USENODE=NODEDO
        nnode=inrs

c Look down rs (ls not relevant for tree
 20     continue                     !jump here to continue collapsing
        call get_nodeinfo(nnode,opa,val8a,inlsa,inrsa)
        flaga=set_flagval(opa,val8a)

        if (is_iopr(nnode)) then    !keep collapsing
                if (is_neg(nnode)) isneg=-isneg
                usenode=nnode        !this is why this doesnt use ferret_iopr
                nnode=inrsa
                goto 20
        else                         !no more ( to collapse
                continue                !jump below
        endiF


c If here, will get rid of at least 1 iopr node.  Also, will shift - and
c incorporate isneg into tree. Note that nnode is the first non-iopr node
c encounterd in the sub-tree

        if (isneg.gt.0) then            !no - to shift

c if nnode is a valu,vari, or funk, point directly to it. Else, point
c to ( and fix up sign to be +.

            if (is_vvf(nnode).or.forceit.ge.1) then
                  usenode=nnode           !do NOT change sign on valu,etc.
                  goto 3000             !as is
            else                  !ismath, so retain (, and make +
               goto 2000        !make +(
            endif

        endif

c  ELSE ....... IF here, isneg=-1.  Try  to make nnode negative.

        qdidit=make_itneg(nnode,doneg)       !do the dirty work

        if (qdidit) then                !success
            if (forceit.eq.0) then      !force retention of outer ( ) ?
               goto 2000                !make it (
            else
               usenode=nnode     !point into node, so (A ==> A
               goto 3000
            endif
        else                    !no success, return -(
            goto 2500
        endif


c if here, make into +(
 2000   continue
        if (.not.is_iopr(usenode)) then
            call calc_error(' Trying to make + but not ( ')
            return
        endif
        call get_nodeinfo(usenode,opa,val8b,inlsb,inrsb)
        call change_nodeinfo(usenode,opa,dabs(val8b),inlsb,inrsb)
        if (nerr.gt.0) then
           call calc_error(' ! In Kollapse_iopr ')
           return
        endif
        nodedo=copy_node(usenode,nodedo)
        kollapse_iopr=nodedo
        return

c if here, could not move - into node; so save as -(
 2500   continue
        call get_nodeinfo(usenode,opa,val8b,inlsb,inrsb)
        if (dabs(val8b).ne.iopr) then
            call calc_error(' Trying to make - but not ( ')
            return
        endif
        call change_nodeinfo(usenode,opa,-dabs(val8b),inlsb,inrsb)
        nodedo=copy_node(usenode,nodedo)
        kollapse_iopr=nodedo
        return


c usenode as is (if successful move of - into ( )
 3000   continue
        nodedo=copy_node(usenode,nodedo)
        kollapse_iopr=nodedo
        return



        end


c======================================
c do real algebraic simplification

        function real_algebra(nodedo)

        implicit none

        integer nodedo

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        common   /cm_simp/
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra
        integer
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra

        logical is_math2,is_math,make_itneg,qtmp,ISEQ_NEG
        integer kompare_trees,anoper,inls,inrs,newnode,iseq
        real *8 val8
        integer make_valnode4
        integer r1,r2,l1,l2
        integer calc_mathnode4
        integer do_divthing,do_expthing,MONDO_ADD,mondo_mult,mondo_div
        real aneg



c if not requested ..
        if (doalgebra.ne.1) goto 1400

c can only do algebra on math nodes
        if (.not.is_math(nodedo)) goto 1400

c get stuff from this math node
        call get_nodeinfo(nodedo,anoper,val8,inls,inrs)

        iseq=kompare_trees(inls,inrs)
        aneg=1.0

        if (iseq.ne.1) then
c let's see it ls = -rs
           if (make_itneg(inrs,1)) then
                iseq=kompare_trees(inls,inrs)  !is ls=-rs
                qtmp=make_itneg(inrs,2)  !restore to orignal
                if (.not.qtmp) then
                    print*,' Problem in ls=-rs '
                    stop
                endif
                if (iseq.eq.0) then
                   goto 100         !didn't work
                endif
                aneg=-1.0    ! did work
           else
                goto 100
           endif
        endif


c x/x == 1
        if (is_math2(nodedo,div)) then
                newnode=make_valnode4(1.0*aneg)
                goto 2000
         endif                  !div

c x*x = x^2. Note, don't use amult.

       if (is_math2(nodedo,mult).and.aneg.eq.1) then  !not ls=-rs
                if (iseq_neg(inls)) then
                   qtmp=make_itneg(inls,2)
                ENDIF
                newnode=calc_mathnode4(exp,inls,2.0)
                goto 2000
        endif                           !mult


c x-x = 0 or x + -X
       if (   (is_math2(nodedo,minus).and.aneg.eq.1) .OR.
     1        (is_math2(nodedo,add).and.aneg.eq.-1)
     1                              ) then
                newnode=make_valnode4(0.0)
                goto 2000
         endif                  !minus

c x + x = x- -x = 2 * x
        if (   (is_math2(nodedo,add).and.aneg.eq.1) .OR.
     1         (is_math2(nodedo,minus).and.aneg.eq.-1)
     1                                ) then
                newnode=calc_mathnode4(mult,inls,2.0)
                call swap_branches(newnode)        !prettify
                goto 2000
        endif


c if fell through to here, no conditions satistifed
 100    continue                !jump here if not equal nodes



c  Try and do : (AX) + ( BX) --- with A or B =1, and ( ) absent or deeper

c Try to accumulate over a slew of  -  + terms
c eg  ( C X) - ( A X ) + 7  - ( 8 A ) + ( 6 X ) + 5 - (2 X) ==
c     ( X ( 4 + C - A )) + 7 - (8 A )



        if (is_math2(nodedo,add).or.is_math2(nodedo,minus)) then

c if here, no simple add -- try list add
           newnode=MONDO_ADD(nodedo)
           if (newnode.ne.0) goto 2000
           goto 500
c note that get_2nodes set, if only one node on a side,  x2=nil
        endif           ! ax   + Bx check



         if (is_math2(nodedo,mult)) then                 !try list mult
                 newnode=mondo_mult(nodedo)
                 if (newnode.ne.0) goto 2000
                 goto 500
         endif

         if (is_math2(nodedo,div)) then                 !try list div
                 newnode=mondo_div(nodedo)
                 if (newnode.ne.0) goto 2000
                 goto 500
         endif


c Try and do: (AX)/(BX)

        if (is_math2(nodedo,div)) then          !possibly ax/bz
           call get_2nodes(inls,l1,l2,mult)     !get a x
           if (l1.ne.nil) then                  ! get b z
              call get_2nodes(inrs,r1,r2,mult)
           endif
           if (r1*l1.ne.0) then                 !got a legit a x and b x
              newnode=do_divthing(l1,l2,r1,r2)  !see if x=z, and process
              if (newnode.ne.0) then
                  goto 2000       !SUCCESS
              else
c Try and do: A X^n/ B X^j = A/B X^(n-j)
c ps, if here, we know its div, and we can use l1,l2,r1,r2
                 newnode=do_expthing(l1,l2,r1,r2)  !nop
                 if (newnode.ne.0) THEN
                     goto 2000       !SUCCESS
                   ENDIF
              endif
              goto 500
           endif

        endif


 500    continue                !JUMP here if not ax/bz



c  Jump here if no algebra possible
 1400   continue

        real_algebra=nodedo
        return

c if here, did some kind of change
 2000    continue

         real_algebra=newnode
        return

        end

c===================
c process a x^n /  x ^ j

        integer function do_expthing(l1,l2,r1,r2)

        implicit none
        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil
        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer l1,l2,r1,r2,do_expthing0,newnode
        logical is_exp

        newnode=0


c check each element to see if it is exp; if so, try using it
        if (is_exP(l1)) then      !l1 is exp, compare  against r1 or r2
            newnode=do_expthing0(l1,r1,1,l2,r2)
            if (newnode.eq.0) then      !form l2*newnode/r2
               newnode=do_expthing0(l1,r2,1,l2,r1)
            endif

        elseif (is_exp(l2)) then
            newnode=do_expthing0(l2,r1,1,l1,r2)
            if (newnode.eq.0) then      !form l2*newnode/r2
               newnode=do_expthing0(l2,r2,1,l1,r1)
            endif

        elseif (is_exp(r1)) then
            newnode=do_expthing0(r1,l1,0,l2,r2)
            if (newnode.eq.0) then
               newnode=do_expthing0(r1,l2,0,l1,r2)
            endif

        elseif (is_exp(r2)) then
            newnode=do_expthing0(r2,l1,0,l2,r1)
            if (newnode.eq.0) then
               newnode=do_expthing0(r2,l2,0,l1,r1)
            endif

        endif

        do_expthing=newnode
        return
        end

c=============================
c m1 is x^n, see if n1 is X^j, or X
        integer function do_expthing0(m1,n1,isnum,am1,an1)

        implicit none

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer m1,n1,isnum,l1,l2,r1,r2,newnode,nd2,am1,an1,nd1
        integer kompare_trees,calc_mathnode2,calc_mathnode


        if (m1.eq.nil .or. n1.eq.nil) then      !just in case
           do_Expthing0=0
           return
        endif

        call get_expthing(m1,l1,l2)   ! x and n in x^n
        call get_expthing(n1,r1,r2)  !will take care if it aint x^n

        if (l1.eq.nil .or. r1.eq.nil) then      !just in case
           do_Expthing0=0
           return
        endif

        newnode=0

c now compare base of m1 and n1.
        if (kompare_trees(l1,r1).eq.0) then    !not equal,
           do_Expthing0=0
           return
        endif

c equal, so: if isnum=1,return l1^(l2-r2)
c            if isnum=0, return l1^(r2-l2)  , r2 should = 1

        if (isnum.eq.1) then
             nd2=calc_mathnode(minus,l2,r2)
        else
             nd2=calc_mathnode(minus,r2,l2)
        endif
        if (am1.ne.nil.or.an1.ne.nil) then
             newnode=calc_mathnode(exp,l1,nd2)   !(exp(a-b))
        else
             newnode=calc_mathnode2(exp,l1,nd2) ! exp(a-b)
        endif


        if (newnode.ne.0) then          !add A and B (A x^n)/(B x^m)
             if (am1.ne.nil) then
               nd1=calc_mathnode2(mult,am1,newnode)
             else
               nd1=newnode
             endif
             if (an1.ne.nil) then
                 newnode=calc_mathnode2(div,nd1,an1)
             else
                newnode=nd1
             endif
        endif
        do_expthing0=newnode
        return
        end

c -----------
c modification of is_math2
        logical function is_exp(nn)
        implicit none

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        logical is_math2
        integer nn

        if (nn.eq.nil) then
           is_exp=.false.
        else
           is_exp=is_math2(nn,exp)
        endif
        return
        end

c====================
c get components of x^n
        subroutine get_expthing(m1,r1,r2)

        integer m1,r1,r2,get_left,get_right,isign
        logical make_itneg,qtmp
        logical is_exp
        integer make_valnode4


        if (is_exp(m1)) then
           r1=get_left(m1)
           call ferret_iopr(r1,r1,isign)
           if (isign.eq.-1)  qtmp=make_itneg(r1,2)

           r2=get_right(m1)
           call ferret_iopr(r2,r2,isign)
           if (isign.eq.-1) qtmp=make_itneg(r2,2)



c2         if (is_iopr(r1)) then        !delve into (
c             r1=get_right(r1)
c             goto 2
c          endif
c4         if (is_iopr(r2)) then        !delve into (
c             r2=get_right(r2)
c             goto 4
c          endif

        else
           r1=m1
           r2=make_valnode4(1.0)
        endif
        return
        end

c===================
c compare 4 possible equalities in AB / CD (a=c,a=d,b=c,b=d)

        integer function do_divthing(l1,l2,r1,r2)

        implicit none
        integer do_divthing0,l1,l2,r1,r2,newnode

           newnode=do_divthing0(l1,r1,l2,r2)  !compare l1 to r1
           if (newnode.ne.0) goto 2000

           newnode=do_divthing0(l1,r2,l2,r1)  !compare l1 to r2
           if (newnode.ne.0) goto 2000

           newnode=do_divthing0(l2,r1,l1,r2)  !compare l2 to r1
           if (newnode.ne.0) goto 2000

           newnode=do_divthing0(l2,r2,l1,r1)  !compare l2 to r2
           if (newnode.ne.0) goto 2000

           do_divthing=0                !no success
           return

 2000      continue                             !here on success
           do_divthing=newnode
           return
           end

c====================
c see if can do ax/bx
        integer function do_divthing0(m1,n1,m2,n2)

        implicit none
        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer m1,n1,m2,n2,kompare_trees,calc_mathnode2,newnode
        integer make_valnode4,calc_mathnode4

        if (m1.eq.nil .or. n1.eq.nil) then  !missing num or denom
              do_divthing0=0             !really should not happen, but.
              return
        elseif (kompare_trees(m1,n1).eq.0) then    !not equal, no algebra
              do_divthing0=0
              return
        endif

c depending on values A and B ... ( in AX/BX)
        newnode=0
        if (m2.eq.nil.and.n2.eq.nil) then   !   x / x = 1
           newnode=make_valnode4(1.0)
        elseif (n2.eq.nil) then                            ! AX/ X = a
             newnode=m2
        elseif (m2.eq.niL) then                 !  x/ ax = 1/a
             newnode=calc_mathnode4(div,n2,1.0)
             call swap_branches(newnode)
        else                            ! ax/bx  == a/b
             newnode=calc_mathnode2(div,m2,n2)
        endif

        do_divthing0=newnode
        return
        end


c================
c make a value node, using real*4 input
        integer function make_valnode4(aval)
        implicit none

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        real aval
        integer make_valnode
        real *8 aval8

        aval8=aval
        make_valnode4=make_valnode(aval8,valu)
        return
        end

c=========================
c extract either a vvf, or A and B ins A*B.  Return in nd1 and nd2.
c nd1=nil if can't find, nd2=nil if vvf.
        subroutine get_2nodes(anode,nd1,nd2,theop)

        implicit none

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        logical is_vvfi,is_math2,is_neg,is_math
        LOGICAL IS_IOPRPOS
        integer get_left,get_right,theop

        integer anode,nd1,nd2,doit

         nd1=nil                 !assume no nodes found
         nd2=nil
         doit=anode

 10     IF (IS_IOPRPOS(DOIT)) THEN       !DELVE INTO + NODES
             doit=GET_RIGHT(DOIT)
        ENDIF

 1020    continue                       !jump here on iopr delving

         if (is_vvfi(doit).or.is_neg(doit)) then         ! no ls
                nd1=doit

          elseif (is_math2(doit,theop)) then

                nd1=get_left(doit)      !get ls
                nd2=get_right(doit)      !get ls

          elseif (is_math(doit)) then
               nd1=doit
          endif

         IF (ND1.NE.NIL) THEN
 20             IF (IS_IOPRPOS(ND1)) THEN
                   ND1=GET_RIGHT(ND1)
                ENDIF
        ENDIF
        IF (ND2.NE.NIL) THEN
 30             IF (IS_IOPRPOS(ND2)) THEN
                   ND2=GET_RIGHT(ND2)
                ENDIF
        ENDIF

          return

        end

c======================================
c do simple algebraic simplification

        function simple_algebra(nodedo)

        common   /cm_simp/
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra
        integer
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra

        integer dosimple2
        common /cm_simp2/dosimple2


        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer theoper,opl,opr,opr2,opa,oprr,make_valnode4,simp_1div
        real *8 theval8,val8l,val8r,val8r2,val8,get_val8

        logical is_iopr,is_math,is_ioprpos
        logical is_vvfi,is_math2
        logical iseq_zero,QTMP,MAKE_ITNEG,ISEQ_NEG
        integer iseq_one,simp_func
        integer ab_over_cd,exp_exp_chk
        logical is_vari,is_valu,is_func


c use this stuff?
        if (dosimple.le.0) then
            simple_algebra=nodedo
            return
        endif


c The following situations will be corrected:
c X * 1  ==> X
c 1 * X ==> X
c -1 * x ==> -X
c X * 0 ==> 0
c 0 * X ==> 0
c X * -1 = -X
c A * 1/B ==> A/B
c 1/B * A ==> A/B
c 0 + A ==> A
c A + 0 ==> A
c A + -B ==> A - B
c a - (-b + c) ==> a + b -c
c a - (b -c ) == > a + c - b
c a + (B - c) ==> a + b -c
c (a+b) +-c ==> a + B +- C
c a + (b +- c) ==> a + b +- c
c A - -B ==> A + B
c A ^ 1   ==> A
c A ^ 0  ==> 1
c  +-A/-B ==> -+A/B
c 0/A ==> 0
c (1/b)/ (1/d) = ( d /b)
c A / 1/B ==> A*B
c 1 / A^-1 = A
c In most cases, parenthesis do NOT block analysis.

c if (, valu, or vari, nuttin to do
       if (is_vari(nodedo).or.is_valu(nodedo).or.is_iopr(nodedo))
     1           then
            simple_algebra=nodedo
            return
        endif

        if (is_func(nodedo)) then
            newnode=simp_func(nodedo)
            if (newnode.ne.0) goto 2000
            goto 1000
        endif

c this should not happen, but ..
        if (.not.is_math(nodedo)) then
            simple_algebra=nodedo
            return
        endif



c get node info
        call get_nodeinfo(nodedo,theoper,theval8,inls,inrs)

c if here, this is math node. Get ls and rs
        flagval=set_flagval(theoper,theval8)

        call get_nodeinfo(inls,opl,val8l,inlsl,inlsr)
        flagl=set_flagval(opl,val8l)
        call get_nodeinfo(inrs,opr,val8r,inrsl,inrsr)
        flagr=set_flagval(opr,val8r)


c do * operations.

        if (flagval.eq.mult) then

c 1 * X or X *1 , or -1*x or X*-1
            ntmp=iseq_one(inls)
            if (ntmp.ne.0) then
                newnode=simp_mult1(ntmp,inrs)
                if (newnode.eq.0) goto 1000
                goto 2000
            endif

            ntmp=iseq_one(inrs)
            if (ntmp.ne.0) then
                newnode=simp_mult1(ntmp,inls)
                if (newnode.eq.0) goto 1000
                goto 2000
            endif

c X * 0 or 0 * X
          if ( iseq_zero(inls).or.iseq_zero(inrs) ) then
                newnode=make_valnode4(0.0)
                goto 2000
           endif


c X * +-1/A or +-1/A * X  (implied parenthesis around 1/a)
c also, if X=value and v in V/A is value or variable, return (X*V)/A
         if (dosimple2.eq.1) then
           newnode=simp_1div(inls,inrs)
           if (newnode.gt.0) then
              if (newnode.eq.0) then
                  goto 1000            !no success, but tried
              else
                  goto 2000               !success
              endif
           endif
        endif


c X * -a = -x *a
           if (iseq_neg(inrs)) then
              if (make_itneg(inls,2)) then      !was able to convert x to -x
                 qtmp=make_itneg(inrs,1)
                 if (.not.qtmp) then      !should be able to do it
                    calL calc_error(' ! Error in x * -a ' )
                    return
                 endif
                 goto 1000
              else
                    calL calc_error(' ! Error in x * -a . ' )
              endif
           endif


        endif
c
c c ADD
        if (flagval.eq.add) then

c c 0 + A, A + 0
             if (iseq_zero(inls)) then
                  newnode=inrs
                  goto 2000
             elseif (iseq_zero(inrs)) then
                   newnode=inls
                  goto 2000
c c A + -B == a -b
             elseif ( iseq_neg(inrs)) then
               qtmp=make_itneg(inrs,2)       !actually, just change signs
               if (qtmp) then
                  theval8=minus
                  call change_nodeinfo(nodedo,theoper,theval8,inls,inrs)
                  goto 1000                  !direct change to nodedo
                else
                   call calc_error(' ! Error in A + -B ')
                   return
                endif
             endif

c -a + b = B - A  !maybe implement this later
c            if (iseq_neg(inls).and.(.not.iseq_neg(inrs))) then
c                  qtmp=make_itneg(inls,2)
c                  newnode=calc_mathnode(minus,inrs,inls)
c                  if (is_iopr(newnode)) newnode=get_right(newnode)
c                  goto 2000
c            endif


c a + ( b +- c )  ==> a + B  +- c
             if (is_ioprpos(inrs)) then      !dont try to mess wid -(
!check for (a +- b)
                if (is_math2(inrsr,add).or.is_math2(inrsr,minus)) then
                     call right_assign(nodedo,inrsr)
                     goto 1000
                endif
c ( b +c ) +- a  ==>  b + c  +- a
             elseif (is_ioprpos(inls))  then
                if (is_math2(inlsr,add)) then
                     call left_assign(nodedo,inlsr)
                     goto 1000
                endif
             endif
        endif

c
c c MINUS
        if (flagval.eq.minus ) Then
c c A - 0
             if (iseq_zero(inrs)) then
                 newnode=inls
                 goto 2000
c 0 - A
             elseif (iseq_zero(inls)) then
                  if (make_itneg(inrs,2)) then
                     newnode=inrs
                     goto 2000
                  else
                      goto 1000
                  endif
c
c c A - -B == A + B
              elseif (iseq_neg(inrs)) then

                  qtmp=make_itneg(inrs,2)
                  if (qtmp) then
                      theval8=add
                      call change_nodeinfo(
     1                      nodedo,theoper,theval8,inls,inrs)
                  endif
                  goto 1000                  !direct change of nodedo

             endif

             if (is_ioprpos(inrs)) then
c A - ( c - B) ==> A + B - c
                if (is_math2(inrsr,minus)) then
                  call swap_branches(inrsr)
                  theval8=add
                  call change_nodeinfo(
     1                          nodedo,theoper,theval8,inls,inrs)
                  goto 1000                  !direct change of nodedo

                elseif (is_math2(inrsr,add)) then
c A - (-B + C) ==> A + B - C
                   call get_nodeinfo(inrsr,oprr,val8r2,inrsrl,inrsrr)
                   if (iseq_neg(inrsrl)) then         !is it -C
                       if (make_itneg(inrsrl,2)) then  !changed to positive
                          val8r2=minus
                          call change_nodeinfo(
     1                          inrsr,oprr,val8r2,inrsrl,inrsrr)
                          theval8=add
                          call change_nodeinfo(
     1                           nodedo,theoper,theval8,inls,inrsr)
                         goto 1000
                       endif      !made -c to c
                    endif               !is -c
                endif             !add do minus

c (A + c) - B  ==> A + B - c
             elseif (is_ioprpos(inls)) then
                if (is_math2(inlsr,add)) then
                  call left_assign(nodedo,inlsr)
                  goto 1000                  !direct change of nodedo
                endif
             endif

        endif                         !minus

c c EXP
        if (flagval.eq.exp) then
c A ^ 1
           ntmp=iseq_one(inrs)
           if (ntmp.eq.1) then
                  newnode=inls
                  goto 2000
           endif

c A^0
             if (iseq_zero(inrs)) then
                  newnode=make_valnode4(1.0)
                  goto 2000
             endif


c  -A^2 ==>, A^2 (2 can be any even number
             if (iseq_neg(inls)) then
                call ferret_iopr(inrs,inrs2,isign)
                if (is_valu(inrs2)) then
                   aval8=get_val8(inrs2)
                   if (dmod(aval8,2).eq.0) then
                        qtmp=make_itneg(inls,2)
                        goto 1000
                   endif
                endif
             endif

c  (A/B)^-j >> (B/A)^J
             if (iseq_neg(inrs)) then           !negative exponent
                call ferret_iopr(inls,inls2,isign)  !into ( )
                if (is_math2(inls2,div)) then    !fraction base
                    call swap_branches(inls2)
                    qtmp=make_itneg(inrs,2)
                    goto 1000
                endif
             endif

             newnode=exp_exp_chk(inls,inrs)
             if (newnode.ne.0) goto 2000
             goto 1000

        endif

c c DIV
        if (flagval.eq.div) then


c A/1 or A/-1
            ntmp=iseq_one(inrs)
            if (ntmp.ne.0) then
                if (ntmp.eq.1) then
                    newnode=inls
                    goto 2000
                else
                  qtmp=make_itneg(inls,2)   !make it negative.
                  if (.not.qtmp) then
                     call calc_error(' ! problem in A/1 ')
                     return
                  else
                     newnode=inls
                     goto 2000
                  endif
                endif
             endif

c (0)/A
            if (iseq_zero(inls)) then
                    newnode=make_valnode4(0.0)
                    goto 2000
            endif



c c 1 / A^-1
          ntmp=iseq_one(inls)
          if (ntmp.ne.0) then                     ! -+1 /
             if (is_math2(inrs,exp)) then       !  A ^

                call get_nodeinfo(inrs,opr2,val8r2,inrsl,inrsr)
                if (iseq_one(inrsr).eq.-1.0) then       ! ^ -1
                   if (ntmp.eq.1) then      !no problemo
                       newnode=inrsl
                       goto 2000
                   else                 !might be neg of math node, cant do
                      if (is_vvfi(inrsl)) then  !make negative
                          call get_nodeinfo(inrsl,opa,val8,inl,inr)
                          call change_nodeinfo(inrsl,opa,-val8,inl,inr)
                          newnode=inrsl
                          goto 2000
                      endif
                   endif

                endif

             endif
          endif


c (a/b) / (C) or (a/b) / (c/d) -- also,  spiff up sign of +-a/+-b
          newnode=ab_over_cd(inls,inrs)
          if (newnode.ne.0) goto 2000


        endif                           !end of DIV

c if fell through to here, no conditions satistifed
 1000   continue
        simple_algebra=nodedo
        return

c if here, did some kind of change
 2000    continue
          simple_algebra=newnode
        return

        end

c-----------------------------
c If here, is a function.  See if its of form g(f(, and if so, can
c it be simplified.

        integer function simp_func(nodedo)

        implicit none
        integer nodedo,inls,inrs
        integer opa,opb,inlsa,inrsa,isign
        logical is_func

        real *8 v8,v8a

        simp_func=0             !assume failure

        call get_nodeinfo(nodedo,opa,v8,inls,inrs)

        call ferret_iopr(inrs,inrs,isign)
        if ((.not.is_func(inrs)).or.(isign.eq.-1)) then     !not F(G(
             simp_func=0
             return
        endif

c Codes: exp=14, exp10=15, ln=16, log10=17
c sin,cos,tan (D)=1,2,3 (R)=4,5,6    (arcxxx 7-12)

c else, F(G -- see if it can be collapsed
c Cases: EXP(LN(A) = A      LN(EXP(A))=A
c        EXP10(Log10(A))=A log10(exp10(a))=a


        call get_nodeinfo(inrs,opb,v8a,inlsa,inrsa)

        if ( (v8.eq.14 .and. v8a.eq.16) .or.
     1       (v8.eq.15 .and. v8a.eq.17)  .or.
     1       (v8.eq.16 .and. v8a.eq.14) .or.
     1       (v8.eq.17 .and. v8a.eq.15) ) then
           simp_func=inrsa

        elseif ( (v8.ge.1 .and. v8.le.12) .and.
     1           (v8a.ge.1 .and. v8a.le.12)    ) then   !might be sin(arcsin(a
                if (dabs(v8a-v8).eq.6.0) simp_func=inrsa
        endif

        return
        end


c================================
c check for (x^a)^b ==>  x^(a*B). We know its inls^inrs
c also, check for (1/x)^j ==> x^-j
        integer function exp_exp_chk(inls,inrs)
        implicit none

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer inls,inrs,inls2,inlsr,inlsl,tmpnode,calc_mathnode
        integer calc_mathnode2,isign,get_left,get_right,cd_oprn2,itmp
        integer iseq_one,inls3,inrs2,tmp1,tmp2
        logical is_math2,is_math,qtmp,make_itneg,is_neg,isexp,isdiv

        call ferret_iopr(inls,inls2,isign)

        isexp=is_math2(inls2,exp)
        isdiv=is_math2(inls2,div)

        if (.not.(isexp.or.isdiv)) then
              exp_exp_chk=0               !do nothing
              return
        elseif (isexp.and.isign.lt.0) then    !too wierd (-x^j)^j
              exp_exp_chk=0               !do nothing
              return

        endif

c do (y/x)^a ==> Y^a*x^-a -----------------------
        if (isdiv) then                 !check for 1/x
             inls3=get_left(inls2)

             itmp=iseq_one(inls3)*isign
             if (itmp.eq.0) then     !  not -1 or 1, so do (x/y)^a=(x^a)/(y^a)

                if (isign.lt.0) qtmp=make_itneg(inls3,2)  !move sign in
                if (is_neg(inls3).or.is_math(inls3))
     1                  inls2=cd_oprn2(inls3)

                inrs2=get_right(inls2)        !note: sign for inls2 in inls3
                if (is_neg(inrs2).or.is_math(inrs2))
     1                  inrs2=cd_oprn2(inrs2)

                tmp1=calc_mathnode(exp,inls3,inrs)
                tmp2=calc_mathnode(exp,inrs2,inrs)
                exp_exp_chk=calc_mathnode2(div,tmp1,tmp2)
                return

             else                       ! base is -+ 1/x
                inrs2=get_right(inls2)

                if (itmp.lt.0) then     !move -1 to -x
                     qtmp=make_itneg(inrs2,2)
                endif
                if (is_math(inrs2).or.is_neg(inrs2)) then   !parens needed
                       inrs2=cd_oprn2(inrs2)
                endif
                qtmp=make_itneg(inrs,2)      !make a, of (1/x)^a, into -a
                if (is_math(inrs).or.is_neg(inrs)) inrs=cd_oprn2(inrs)
                exp_exp_chk=calc_mathnode2(exp,inrs2,inrs)

             endif
             return
         endif

c DO (x^a)^b ---------------

c we have an exp in ls, form x^(a+b) OR (-x)^(a+B) (if isign<0)
        inlsl=get_left(inls2)
        inlsr=get_right(inls2)


        tmpnode=calc_mathnode(mult,inlsr,inrs)


        call ferret_iopr(inlsl,inls2,isign)

        if (isign.lt.0) then             ! [ -(-a)]^b == [(--a)]^b
                qtmp=make_itneg(inls2,2)
        endif                        !else, leave  alone

        if (is_math(inls2).or.is_neg(inls2)) inls2=cd_oprn2(inls2)

        tmpnode=calc_mathnode2(exp,inls2,tmpnode)

        exp_exp_chk=tmpnode
        return
        end



c========================
c (a/b) / (C) or (a/b) / (c/d)
       integer function ab_over_cd(inls0,inrs0)
        implicit none

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer inls,inrs,inls0,inrs0,isign,isign0
        integer tmpnode,vlnode
        integer calc_mathnode2,cd_oprn2,calc_mathnode
        integer ferret_left_abs,ferret_right_abs
        integer inlsr,inrsr,inlsl,inrsl,dennode,numnode,MAKE_VALNODE
        REAL *8 AVAL8,AB_OVER_CD_2
        logical is_math2,make_itneg,qtmp,iseq_neg,is_math
        logical lisdiv,risdiv

        isign=1

        call ferret_iopr(inls0,inls,isign)
        if (iseq_neg(inls)) then
           qtmp=make_itneg(inls,2)
           isign=-isign
        endif
        call ferret_iopr(inrs0,inrs,isign0)
        isign=isign*isign0
        if (iseq_neg(inrs)) then
           qtmp=make_itneg(inrs,2)
           isign=-isign
        endif

        lisdiv=is_math2(inls,div)
        risdiv=is_math2(inrs,div)

        if (.not.(lisdiv.or.risdiv)) then        !a  / b

            AVAL8=AB_OVER_CD_2(INLS,INRS,ISIGN)
            IF (AVAL8.EQ.-1) THEN
               qtmp=make_itneg(inls,2)
               if (is_math(inls))  inls=cd_oprn2(inls)
            ELSEIF  (AVAL8.NE.1) THEN
               VLNODE=MAKE_VALNODE(AVAL8,VALU)
               INLS=CALC_MATHNODE(MULT,VLNODE,INLS)
            ENDIF               !IF 1.0, LEAVE INLS AS IS

            if (is_math(inrs))  inrs=cd_oprn2(inrs)
            tmpnode=calc_mathnode2(div,inls,inrs)

        elseif (lisdiv.and. (.not.risdiv)) then   ! a/b / c == a/(b * c)
            inlsr=ferret_right_abs(inls,isign)
            inlsl=ferret_left_abs(inls,isign)

            if (is_math(inlsr)) inlsr=cd_oprn2(inlsr)
            if (is_math(inrs))  inrs=cd_oprn2(inrs)

            tmpnode=calc_mathnode(mult,inlsr,inrs)

            if (isign.lt.0) qtmp=make_itneg(inlsl,2)   !effectively -
            if (is_math(inlsl)) inlsl=cd_oprn2(inlsl)

            tmpnode=calc_mathnode2(div,inlsl,tmpnode)
            if (inls.ne.inls0) then             !was it (num)/den
                tmpnode=cd_oprn2(tmpnode)
            endif

        elseif (risdiv.and. (.not. lisdiv)) then   ! a/ b/c == a*c / b

            inrsl=ferret_left_abs(inrs,isign)
            inrsr=ferret_right_abs(inrs,isign)

            if (isign.lt.0) qtmp=make_itneg(inls,2)
            if (is_math(inls)) inls=cd_oprn2(inls)
            if (is_math(inrsr)) inrsr=cd_oprn2(inrsr)

            tmpnode=calc_mathnode2(mult,inls,inrsr)

            if (is_math(inrsl)) inrsl=cd_oprn2(inrsl)
            tmpnode=calc_mathnode2(div,tmpnode,inrsl)
            if (inls.eq.inls0)tmpnode=cd_oprn2(tmpnode)

        else                            ! a/b / c/d = a*d / b*c

            inlsl=ferret_left_abs(inls,isign)      ! a
            inlsr=ferret_right_abs(inls,isign)     !b
            inrsl=ferret_left_abs(inrs,isign)      !c
            inrsr=ferret_right_abs(inrs,isign)    !d


            if (isign.lt.0) qtmp=make_itneg(inlsl,2)
            if (is_math(inlsl)) inlsl=cd_oprn2(inlsl)
            if (is_math(inrsr)) inrsr=cd_oprn2(inrsr)
            numnode=calc_mathnode2(mult,inlsl,inrsr)


            if (is_math(inlsr)) inlsr=cd_oprn2(inlsr)
            if (is_math(inrsl)) inrsl=cd_oprn2(inrsl)
            dennode=calc_mathnode2(mult,inlsr,inrsl)
            if (inrs.ne.inrs0) dennode=cd_oprn2(dennode)

            tmpnode=calc_mathnode2(div,numnode,dennode)
            if (inls.eq.inls0) tmpnode=cd_oprn2(tmpnode)

        endif

        ab_over_cd=tmpnode
        return
        end

C=====================
C EXTRACT VALUES FROM 2 NODES, DIVIDE, CORRECT FOR SIGN

        REAL *8 FUNCTION AB_OVER_CD_2(INLS0,INRS0,ISIGN0)
        IMPLICIT NONE

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        integer inls,inrs,isign,inls0,inrs0,isign0
        integer inls2,inrs2,get_left,make_valnode4,GET_RIGHT

        logical is_valu,is_math2

        real *8 aval8,get_val8,aval8a

        aval8a=1
        aval8=isign0

        if (is_valu(inls0)) then                !num is avalue
            aval8=get_val8(inls0)*aval8
            inLs0=MAKE_VALNODE4(1.0)

        elseif (is_math2(inls0,mult))   then      !try left node of *
            inls=get_left(inls0)
            call ferret_iopr(inls,inls2,isign)
            if (Is_valu(inls2)) then
                aval8=get_val8(inls2)*aval8*isign
                inls0=GET_RIGHT(INLS0)
            endif
        endif

c examine denominator
        if (is_valu(inrs0)) then
            aval8a=get_val8(inrs0)
            inrs0=MAKE_VALNODE4(1.0)

        elseif (is_math2(inrs0,mult))  then       !try left node of *
            inrs=get_left(inrs0)
            call ferret_iopr(inrs,inrs2,isign)
            if (Is_valu(inrs2)) then
                aval8a=get_val8(inrs2)*isign
                inrs0=GET_RIGHT(INRS0)
            endif
        endif

        if (aval8a.ne.0) then
               aval8=aval8/aval8a
        else
               ab_over_cd_2=1
               call calc_error(' ! Divide by zero ')
               return
        endif

        ab_over_cd_2=aval8
        return
        end


c-----------------
c deal with V1 * V2/X or V1 * V2%X
        integer function simp_1div(anls,anrs)
        implicit none

        integer anls,anrs,cd_oprn2

        logical is_math,is_math2,is_vari,is_valu

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer anls2,anrs2,lsign,rsign,thediv,thedivr,thev,thedivr2
        integer get_left,thesign,newn,calc_mathnode2
        logical lsisit,rsisit,lsis,rsis,qtmp,iseq_neg,make_itneg

        anls2=anls
        anrs2=anrs

        lsign=1
        rsign=1

c scan down to bottom of anls and anrs
c       call ferret_iopr(anls,anls2,lsign)
c       call ferret_iopr(anrs,anrs2,rsign)

        lsis=is_math(anls2)
        rsis=is_math(anrs2)

c This only works on V * V/X (v=value or vari).
c So, if both (or neither) are math nodes, give up
c Note: for now, we do NOT move functions,etc into V/X

        if  (.not.(lsis.xor.rsis))  then    !only one can be math
            simp_1div=-1               !-1 signals did not try
            return
        endif

c one of them is math, is it div or modop
        lsisit=is_math2(anls2,div).or.is_math2(anls2,modop)
        rsisit=is_math2(anrs2,div).or.is_math2(anrs2,modop)

c one or the other must be div or modop
        if (.not.(lsisit.or.rsisit))  then  !neither are mod or div
            simp_1div=-1               !-1 signals did not try
            return
        endif

c if here, one of this is mod or div. See which one, and see if other is
c var or value
        if (lsisit) then                        !check rs
           if (is_vari(anrs2).or.is_valu(anrs2)) then      !okay
                thediv=anls2
                thev=anrs2
           else
              simp_1div=0               !0 signals / that didnt succeed
              return
           endif
        elseif (rsisit) then
           if (is_vari(anls2).or.is_valu(anls2)) then      !okay
                thediv=anrs2
                thev=anls2
           else
              simp_1div=0               !0 signals / that didnt succeed
              return
           endif
        else
                print *,' Impossible in simp_1div '
                stop
        endif


c when here, one is V, the other is Z/X.  See if Z=Val or Var

        thedivr=get_left(thediv)
        call ferret_iopr(thedivr,thedivr2,thesign)
        if (.not.(is_valu(thedivr2).or.is_vari(thedivr2))) then
             simp_1div=0
             return
        endif

c when here, we have V * Z/X where V is value or variable, and abs(z)=1
c form new node with (V*Z)/X.  Note that we'll let later iterations
c compress V*Z, if V and Z are both values.
c note: product of lsign, rsign,  thesign is the sign.

        if (iseq_neg(thedivr2)) then
           qtmp=make_itneg(thedivr2,2)
           thesign=-thesign
        endif

        if ((lsign*rsign*thesign).lt.0) then  !note: thedivr is +
             qtmp=make_itneg(thev,2)
        endif

        newn=calc_mathnode2(mult,thev,thedivr2)
c and make newn the numerator of Z/X
        call left_assign(thediv,newn)

        if ( (lsisit.and. (anls.eq.anls2) ) .or.
     1       (rsisit.and. (anrs.eq.anrs2) )   ) then
           simp_1div=thediv
        else
           simp_1div=cd_oprn2(thediv)
        endif
        return

        end


c---------------------
c do 1 * X situation (note that it might have originally been X * 1)
        function simp_mult1(isign,anode)

        implicit none

        integer anode,isign
        logical make_itneg,qtmp

c if +1, very simple ...
        if (isign.gt.0.0) then          !just return anode
           simp_mult1=anode
           return
        endif

c       print *, ' in simp mult '
c  NEGATIVE,  -----------

        qtmp=make_itneg(anode,2)    !do force it
        if (.not.qtmp) then
           simp_mult1=0                    !0 signals NO change
        else
           simp_mult1=anode
        endif
c       call show_anode(anode,' simp mult ',anode)
        return

        end


c==========================================





c==========================================
c   compare two trees
        integer function kompare_trees(tree1,tree2)

        implicit none

        integer tree1,tree2

        logical is_math,is_valu,is_vari

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        integer lstat,lfirst,lsecond,isequal

        integer  foo,tr1,tr2,kompare_nodes,get_left,get_right

        data lfirst,lsecond/10,20/

c algorithim:  recursive ...
c 1) Is this node same type and value ?. If not, return not equal
c   a) If is, call with LS of both nodes
c   b) If LS okay, call with RS (else return not equal)
c   c) If RS okay, then return equal (else return not equal)
c To do this ,use stack (store both nodes and the status, lsnotdone and lsdone)

c First, see if its a a trivial equality (both valu or vari node, or nil node)
        if (tree1+tree2.eq.0) then
             kompare_trees=1            !both nil
             return
        elseif (tree1*tree2.eq.0 .and. tree1+tree2.ne.0) then
             kompare_trees=0    !one nil, the other not
             return
        elseif (is_valu(tree1).and.is_valu(tree2) )then   !both values
            kompare_trees=kompare_nodes(tree1,tree2)
            return
        elseif (is_vari(tree1).and.is_vari(tree2)) then  !both varis
            kompare_trees=kompare_nodes(tree1,tree2)
            return
        else
          if (is_valu(tree1).or.is_valu(tree2).or.
     1       is_vari(tree1).or.is_vari(tree2)  ) then     !one is ,one aint
              kompare_trees=0
              return
          endif
        endif

c Push "brake" node

       CALL calc_PUSHd(nil,nil,nil,nil)

        isequal=1                 !assume equal

        call calc_pushd(tree1,tree2,lfirst,0)   !lfirst  for 1st encounter
        tr1=tree1
        tr2=tree2

c when here, we are "calling" with arguments of tr1 and tr2
c so, compare nodes


c this is where the recursion starts. So pop to get arguments
 100    continue                        !jump to here on recursoin

c these probably are not used, but..
        if (tr1+tr2.eq.0) then          !got beyond end node
           goto 1000                    !so return
        elseif (tr1*tr2.eq.0) then     !must not be equal
            isequal=0
            goto 1000
        endif

        isequal=kompare_nodes(tr1,tr2)    !compare current nodes

c if isequal=0, then return, with return accomplished at 1000
        if (isequal.eq.0) goto 1000    !value of isequal conditions "return"

c is equal.  Are they terminal nodes.  Note that since tr1=tr2, then
c we only need look at tr1
 150    continue                     !150 label only for aesthetics
        if (is_valu(tr1).or.is_vari(tr1)) then      !return
            goto 1000
        endif

c  If there is left side, then compare ls before rs
        if (is_math(tr1)) then          !tr1=tr2, so must both be math nodes
           call calc_pushd(tr1,tr2,lfirst,0)   !return to lsfirst
           tr1=get_left(tr1)
           tr2=get_left(tr2)
           goto 100             ! and call with these arguments
        endif

 200    continue                !jump here after return from lsfirst
c note, if here, there must be a rs (if valu or vari, would catch at 150 above

        call calc_pushd(tr1,tr2,lsecond,0)
        tr1=get_right(tr1)
        tr2=get_right(tr2)
        goto 100

c JUMP Here on "return"
 1000   continue

        call calc_popd(tr1,tr2,lstat,foo)    !get state of "calling routine"

        if (lstat.eq.nil) then          !signals end of recursion
            kompare_trees=isequal
            return
        endif

        if (isequal.eq.0) then          !if a nonequality, dont bother
              goto 1000                 !checking
        endif

c if here, returning to caller with equal LS branches.
c so, what is the next step in the calling routine (either lfirst of lsecond)
        if (lstat.eq.lfirst) then               !did ls, now do rs
           goto 200                     !200 uses tr1,tr2 from popd
        endif

c if here, equal RS branches,, so these sub-trees are equal.  Go back
c to the parent of these sub-trees
        if (lstat.eq.lsecond) then
             goto 1000
        endif

        print *,' Impossible location in kompare_trees '
        stop

        end

c---------------------
c compare type and value of two nodes, get left, get right

        integer function kompare_nodes(i1,i2)

        PARAMETER (maxnode=6000)

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        integer get_left, get_right

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        kompare_nodes=0

c check for glitch
        if (i1.gt.maxnode.or.i2.gt.maxnode.or.i1.lt.0.or.i2.lt.0) then
           goto 1000                    !fatal error
        endif

        if (i1+i2.eq.0) goto 20        !both nonexistent, so equal

        if ( (i1+i2.gt.0) .and. (i1*i2.eq.0) ) then  !only one is zero
                return                          !so unequal
        endif

        if (opr(i1).ne.opr(i2)) return          !type ne
        if (value(i1).ne.value(i2)) return    !value ne
c equal, so return

 20     continue
        kompare_nodes=1
        return


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

c----------------------
c see if this node is 1 or -1.  If not, return 0. IF so, return 1 or -1

        function iseq_one(anode0)
        implicit none

        logical is_valu
        integer anode,op1,inls,inrs,anode0,isign
        real *8 val8

        iseq_one=0                       !BY DEFAULT

        call ferret_iopr(anode0,anode,isign)

        if (is_valu(anode)) then
            call get_nodeinfo(anode,op1,val8,inls,inrs)
            if (dabs(val8).eq.1.0) then
               iseq_one=val8*isign
            endif
        endif

        RETURN
        END


c----------------------
c see if this node is 0 or (0)
      logical function iseq_zero(anode0)

        implicit none
        integer anode,op1,inls,inrs,anode0,isign
        real *8 val8

        logical is_valu

        call ferret_iopr(anode0,anode,isign)

        iseq_zero=.false.                       !BY DEFAULT
        if (is_valu(anode)) then
            call get_nodeinfo(anode,op1,val8,inls,inrs)
            if (val8.eq.0.0) then
                iseq_zero=.true.
            endif
        endif

        RETURN
        END


c----------------------
c see if this node is 0 or (0)
c return 1 if pos, -1 if negative

      logical function iseq_neg(anode0)

        implicit none
        integer anode,anode0,get_left,isign

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        logical Qneg
        logical IS_NEG,IS_MATH2

        Qneg=is_Neg(anode0)              !maybe its simple

        if (Qneg) THEN
           iseq_neg=.true.
           return
        endif

c see if you can go into an iopr, or if ls of */% is negative.

        call ferret_iopr(anode0,anode,isign)

c when here, got inside of (s

        Qneg=is_Neg(anode)              !maybe its simple
        if (.not.qneg) then
c not a simple negative.  Check to see if its */%
          if (is_math2(anode,mult).or.is_math2(anode,div)
     1      .or. is_math2(anode,modop) ) then            !is the ls negative?
              qneg=is_neg(get_left(anode))
           endif
        endif

c qtmp and isign=1 OR .not.qtmp and isign=-1
        if ( (qneg.and.(isign.eq.1)) .or.
     1       ( (.not.qneg).and.(isign.eq.-1))  ) then
           iseq_neg=.true.
        else
           iseq_neg=.false.
        endif

        return

        end

c-------------------------------
c Will make a node "negative", if possible.  How?
c If var,valu,funk or iopr: just change sign on valu
c     (nb: -var means "use neg of stored value)
c           -function means "use neg of value returned from applying function"
c           -( means "use negative of expression inside of ( )s "
c If * / %, then change LS to negative (if possible)
c If + -, then reverse signs, etc. (if possible)
c
c Otherwise, leave as is
c       Note: if forceit==2, then ADD () if necessary.
c Function will modify anode directly.  Returns TRUE if modification occured

        logical function make_itneg(nnode,forceit)

        implicit none
        LOGICAL QTMP,IS_VVFI,IS_MATH2,iseq_neg,qneg,is_neg,is_iopr

        integer nnode,inlsb,inrsb,op2,MNODE,MAKE_OPRNODE,mnode2
        REAL COPY_NODE

        integer op1,inls,inrs,forceit,anode,get_left,isign
        real *8 val8

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

         qtmp=.false.

c try to emulate
c might need this info below

c try to emulate iseq_neg -- and make a negative positive
       if (is_neg(nnode)) then          !simple
            call get_nodeinfo(nnode,op1,val8,inls,inrs)
            call change_nodeinfo(nnode,op1,-val8,inls,inrs)
            qtmp=.true.
            goto 1000
       endif

        call ferret_iopr(nnode,anode,isign)     !leave (-(-( as is

        Qneg=is_Neg(anode)              !
        if (qneg) then          !interior of node is negative
            call get_nodeinfo(anode,op1,val8,inls,inrs)
            call change_nodeinfo(anode,op1,-val8,inls,inrs)
            qtmp=.true.
            goto 1000
        elseif (isign.lt.0) then                !make (-( into -(-(
          if (is_iopr(nnode)) then    !must be..at least 1 (
             call get_nodeinfo(nnode,op1,val8,inls,inrs)
             call change_nodeinfo(nnode,op1,-val8,inls,inrs)
             qtmp=.true.
             goto 1000
          endif
        endif

c ferret did not work, try * or / or %
          if (is_math2(anode,mult).or.is_math2(anode,div)
     1      .or. is_math2(anode,modop) ) then            !is the ls negative?
              anode=get_left(anode)
              if (is_Neg(anode)) then          !interior of node is neg
                   call get_nodeinfo(anode,op1,val8,inls,inrs)
                   call change_nodeinfo(anode,op1,-val8,inls,inrs)
                   qtmp=.true.
                    goto 1000
              endif
          endif

c at this point, we are no longer emulating iseq_neg -- force a positive
c to be negative. note use of anode, not nnode (anode is the ferreted node)
          if (is_vvfi(anode)) then
                   call get_nodeinfo(anode,op1,val8,inls,inrs)
                   call change_nodeinfo(anode,op1,-val8,inls,inrs)
                   qtmp=.true.
                   goto 1000

          elseif (is_math2(anode,mult).or.is_math2(anode,div)
     1      .or. is_math2(anode,modop) ) then            !is the ls negative?
                   anode=get_left(anode)
                   call get_nodeinfo(anode,op1,val8,inls,inrs)
                   call change_nodeinfo(anode,op1,-val8,inls,inrs)
                   qtmp=.true.
                    goto 1000

          elseif (is_math2(anode,minus)) then
             inls=get_left(anode)
             if (iseq_neg(inls)) then       !  change -(-a-B) to (A+B)
                 call get_nodeinfo(inls,op2,val8,inlsb,inrsb)
                 call change_nodeinfo(inls,op2,-val8,inlsb,inrsb)
                 call get_nodeinfo(anode,op2,val8,inlsb,inrsb)
                 val8=add              ! - to +
                 call change_nodeinfo(anode,op2,val8,inlsb,inrsb)
                 QTMP=.true.
             else            ! Minus: change -(a-b) to (b-a)
                call swap_branches(anode)
                QTMP=.true.
             endif


c don't do this, for now     (when use, be sure to debug
c Plus:  change -(a+b) to  (-a-b) if desired.
c          elseif (is_math2(anode,add).and.forceit.ge.1) then
c              if (is_vvfi(inls)) then
c                   call get_nodeinfo(inls,op2,val8,inlsb,inrsb)
c                   call change_nodeinfo(inls,op2,-val8,inlsb,inrsb) !-ls
c                   call get_nodeinfo(nnode,op2,val8,inlsb,inrsb)
c                   val8=minus              ! + to -
c                   call change_nodeinfo(nnode,op2,val8,inlsb,inrsb)
c                   QTMP=.true.
c              endif

          endif

C IF FORCEIT==2, MAKE SURE ITS NEGATIVE!
        IF (.NOT.QTMP.AND.FORCEIT.EQ.2) THEN    !didnt work, but make it work
           mnode=make_oprnode(-float(iopr))             !-( node
           mnode2=copy_node(anode,0)
           call right_assign(mnode,mnode2)
           nnode=copy_node(mnode,anode)

           qtmp=.true.
        endif

 1000   continue
        make_itneg=qtmp

        return
        end

c===========================
c ferret out what's inside of (((, and return sign

        subroutine ferret_iopr(anode0,anode,isign)

        implicit none

        logical is_iopr,is_Neg
        integer anode,isign,anode0,get_right

        isign=1
        anode=anode0

 2     if (is_iopr(anode)) then        !delve into (
           if (is_neg(anode)) then
              isign=-isign
           endif
           anode=get_right(anode)
           goto 2
        endif

        return
        end

c=============================
c get left node of anode.  Then ferret left node. Then make it non
c negative.  Return results of ferret and make-non-neg by modifying isign

        integer function ferret_left_abs(anode,isign)
        implicit none
        integer anode,isign,anode2,isign0,get_left,anode3
        integer ferret_right_abs,get_right
        logical make_Itneg,iseq_neg,qtmp,isr

        anode2=get_left(anode)
        isr=.false.

        goto 10

        entry ferret_right_abs(anode,isign)
        isr=.true.
        anode2=get_right(anode)


 10     if (anode2.eq.0) then
          print *,' Error in ferret abs ',anode
          stop
        endif

        if (isign.eq.0) isign=1                 !initialize if necessary
        call ferret_iopr(anode2,anode3,isign0)
        isign=isign*isign0
        if (iseq_neg(anode3)) then
           qtmp=make_itneg(anode3,2)
           isign=-isign
        endif
        if (isr) then
            ferret_right_abs=anode3
        else
            ferret_left_abs=anode3
        endif
        return
        end

c=============================
c similar to is_iopr, but only true if +(
        logical function is_ioprpos(anode)
        implicit none
        logical is_neg,is_iopr
        integer anode

         is_ioprpos=.false.
        if (is_iopr(anode).and. (.not.is_neg(anode))) is_ioprpos=.true.
        return
        end


c------------------
c extract value, or error
        real *8 function get_val8(anode)
        implicit none
        integer anode,anop,inl,inr
        real *8 v8
        logical is_valu
        if (is_valu(anode)) then
            call get_nodeinfo(anode,anop,v8,inl,inr)
            get_val8=v8
        else
           call calc_error(' ! Unexpected non-value in get_val8 ')
           get_val8=0
        endif
        return
        end

