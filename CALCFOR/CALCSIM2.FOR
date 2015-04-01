c@ for the public domain, dmh @ers/rtd, 12/91.
c===================================
c  Do big nasty + - ...
c ps: if here, nodedo is a + or -
c eg  ( C X) - ( A X ) + 7  - ( 8 A ) + ( 6 X ) + 5 - (2 X) ==
c     ( X ( 4 + C - A )) + 7 - (8 A )
        integer function mondo_add(nodedo)

        implicit none

        integer max_nodelist
        parameter (max_nodelist=100)

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        integer LsNotdone,LsDone,LsNone,Lsfunk
        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        integer nodedo,anode,lsstat
        integer *2  nodelist(max_nodelist,2) !node, sign,ls,rs (ls,rs used if

        logical is_math2,is_valu,iseq_neg,make_itneg,qtmp
        logical is_vvfi,is_neg,IS_VVF,q1,q2
        integer get_right,get_left,isign,isign0,in_nodelist
        integer ij,INLS,INRS,ifoo,tmpnode,vlnode,inls2,inrs2
        integer posnodes,negnodes,newnode,THE_MONDO,intmp
        integer ijj,bnode,cnode
        INTEGER HOLD_1,HOLD_2,ITH_IJ,iok

        real *8 oldval,get_val8,FOO8
        integer cd_oprn2,make_valnode
        integer calc_mathnode,calc_mathnode2

        mondo_add=0             !assum failure


C IF OF FORM   X -+ y (X=VVF), THEN LEAVE AS IS
c actually, if X + Val, make into Val + x
        INLS=GET_LEFT(NODEDO)
        INRS=GET_RIGHT(NODEDO)
c check if ls not +-
        q1=is_vvf(inls).or.is_math2(inls,div).or.is_math2(inls,exp)
     1      .or.is_math2(inls,modop)
c check if rs not +-
        q2=is_vvf(inrs).or.is_math2(inrs,div).or.is_math2(inrs,exp)
     1      .or.is_math2(inrs,modop)

        IF (q1.AND.q2) THEN         !not [ls a mult or a -+]
c if +, make sure value is first.
           if (is_valu(inrs).and.is_math2(nodedo,add)) then
                call swap_branches(nodedo)
           endif
           RETURN
        ENDIF

c has to be - or +
        if (.not.(is_math2(nodedo,add).or.is_math2(nodedo,minus))) then
           return
        endif


c The plan is to create a list of (up to 100) entries.  Each
c entry contains: address of terminal node,
c and isign.  Terminal nodes are all nodes that are not + or -
c Isign contains influences of - and -(

c  Plan
c   Scan down LS, going into (.  If + - or minus, continue down LS
c   Keep track of sign, which is combination of minus and -(
c   When hit a terminal node (anything not + - or (, since ( are delved)
c   store it in the nodelist along with current sign.
c   Backup, and go down rs.  Note that we treat everything like add, but
c  if minus we add essentially include a -(.  On backup, retrieve the
c  sign current the parent node is effected by AND whether its a plus or
c  minus.  With this info, go down RS.
c Thus, stack recieves : LSstat (lsnotdone,lsdone are the ONLY possible args,
c since ( are delved and everything else is terminal), Parentnode, parentsign
c  and RSminus.



        in_nodelist=0
        anode=nodedo
        call calc_pushd(Anode,nil,nil,nil)    !brake node
        isign=1                 !assume ( a +- b), so top is +

 100    continue                      !entry point (anode,isign)


c assume its +-, but check for now

        call calc_pushd(anode,lsnotdone,isign,0)  !set up for return
        INLS=get_left(anode)
        call ferret_iopr(INLS,anode,isign0)
        isign=isign*isign0                      !adjust for -(
c ls is a +-, so go down. note that ls is isign ( rs is effected by -)
        if (is_math2(anode,add).or.is_math2(anode,minus)) then
            goto 100
        endif

c else, this is a terminal node.  Save it in nodelist
 150    in_nodelist=in_nodelist+1
        if (in_Nodelist.gt.max_nodelist) then   !too many, give up
          call calc_error(' ! List of A + B .. to long to simplify')
          return
        endIF

c else, add to list
        nodelist(in_nodelist,1)=anode
        nodelist(in_nodelist,2)=isign



c --- return.
 200    continue
        call calc_popd(anode,lsstat,isign,ifoo)

        if (lsstat.eq.nil) then                 !all done
            goto 1000                           !next step

        elseif (lsstat.eq.lsnotdone) then           !now do rs
            call calc_pushd(anode,lsdone,isign,0)  !set up for return
            if (is_math2(anode,minus))isign=-isign
            INRS=get_right(anode)
            call ferret_iopr(INRS,anode,isign0)
            isign=isign*isign0                      !adjust for -(

            if (is_math2(anode,add).or.is_math2(anode,minus)) then
                goto 100                !go down rs
            else
               goto 150                    !terminal node
            endif

        elseif (lsstat.eq.lsdone) then          !has to be this
            goto 200                            !done with this node

        else
                print *,' IMPOSSIBLE in Mondo_add '
                stop
        endif

c--------------------------------
 1000   continue                !here when got all nodes

c first, add up all value nodes

        oldval=0
        do ij=1,in_nodelist
           anode=nodelist(ij,1)
           isign=nodelist(ij,2)
           if (is_valu(anode)) then
              oldval=oldval+(isign*get_val8(anode))
              nodelist(ij,1)=0                  !cancel this node
           endif
        enddo
c form value node ..
        if (oldval.ne.0) then
            vlnode=make_valnode(oldval,valu)
        else
            vlnode=0              !no value node
        endif

        call krunch_list(nodelist,in_nodelist)   !gets rid of zeroed nodes

c if all nodes are values
        IF (IN_NODELIST.EQ.0) THEN      !ONLY GOT A MATH NODE
           if (vlnode.eq.0) then
              call calc_error(' bad vlnode ')
              return
            endif
            THE_MONDO=VLNODE
            GOTO 3000
c if all but one node is a value
        elseif (in_nodelist.eq.1) then   !just one non math node
              anode=nodelist(1,1)
              if (nodelist(1,2).lt.0) then
                  qtmp=make_itneg(anode,2)
              endif
              if (vlnode.eq.0) then
                  THE_mondo=anode
              else
                 THE_mondo=calc_mathnode(add,anode,vlnode)
              endif
              GOTO 3000
        ENDIF                           !simple cases

c if in_Nodelist gt 1, we have a bunch of non value nodes.
c So,  (extract both sides of  node and) make em positive.
c    This means, get both sides AND use ferret_iopr
c    iseq_neg and make_itneg to make each node  positive --
c    The sign is signified by making node address negative.
c    Actually, rs is only made if * node, otherwise rs=0
c  Actual algebra uses these -+ ls and rs (and vlnode)
c Note that we rewrite the old info.

c So set ls and rs  . ls has value, rs has sign
c        fstring='zero '
c        if (vlnode.ne.0) then
c           call tree_string(fstring,vlnode,3,.false.,0)
c        endif
c          print *,' valnode ',fstring(1:len_trim(fstring))


        do ij=1,in_nodelist

            anode=nodelist(ij,1)

c           call tree_string(fstring,anode,3,.false.,0)
c          print *,' pulling:ij,sign,node, ',ij,nodelist(ij,2),' , ',
c     1              fstring(1:len_trim(fstring))


c note: sign comes from minus and -( that led to this node.
            if (.not.is_math2(anode,mult)) then     !use as is if not mult
               if (iseq_neg(anode)) then
                   qtmp=make_itneg(anode,2)
                   nodelist(ij,1)=-ANODE*nodelist(ij,2) !incorporate sign
                else         !not negative
                    nodelist(ij,1)=anode*nodelist(ij,2) !incoproate sign
                endif
                nodelist(ij,2)=0
                cycle
            endif                       !else, get nodes, make positive

C IT'S *, SO GET ls and rs, make positive, store sign
c sign is stored by making node # - or +
            inls=get_left(anode)
            isign=nodelist(ij,2)                !sign of - -( leading to A*b
            call ferret_iopr(inls,inls2,isign0)
            isign=isign*isign0
            if (iseq_neg(inls2)) then
                 qtmp=make_itneg(inls2,2)
                 isign=-isign
            endif

            inrs=get_right(anode)
            call ferret_iopr(inrs,inrs2,isign0)
            isign=isign*isign0
            if (iseq_neg(inrs2)) then
                 qtmp=make_itneg(inrs2,2)
                 isign=-isign
            endif

            nodelist(ij,1)=inls2*isign         !incorporate sign in ls only
            nodelist(ij,2)=inrs2


        enddo


c-------
c do algebra
c first, compare ls against ls (note that rs > 0, and internals of
c ls and rs should be + )

        ij=1

        FOO8=0
        IF (VLNODE.NE.0) THEN
            FOO8=GET_VAL8(VLNODE)
        ENDIF

        do while (ij.le.in_nodelist)

            ITH_IJ=1
            HOLD_1=NODELIST(IJ,1)
            HOLD_2=NODELIST(IJ,2)


 4200       CONTINUE
           anode=nodelist(ij,1)
           tmpnode=nodelist(ij,2)   !tmpnode always not negative
           if (tmpnode.eq.0) then
              intmp=0
           elseif (tmpnode.gt.0) then
              intmp=1
           else
                print *,' TMPNODE < 0 '
                stop
           endif

c scan down list, and compare
           do ijj=ij+1,in_nodelist
               bnode=nodelist(ijj,1)
               cnode=nodelist(ijj,2)
               call mondo_add1(anode,tmpnode,intmp,bnode,cnode,iok)
               if (iok.eq.1) then
                  nodelist(ijj,1)=0                 !SIGNAL TO delete IJJ
               else                 !   try comparing against cnode and bnode
                  call mondo_add1(anode,tmpnode,intmp,cnode,bnode,iok)
                  if (iok.eq.1) then
                     nodelist(ijj,1)=0              !SIGNAL TO delete IJJ
                  endif
               endif
            enddo

C WHEN HERE, ANODE BECOMES EITHER INLS*TMPNODE (OR aNODE or anode*bnode)

           if (INTMP.EQ.0) then       !X did not match anything

                NODELIST(IJ,1)=IABS(HOLD_1)
                NODELIST(IJ,2)=SIGN(1,HOLD_1)

C NO MATCH, RS EXISTS, AND FIRST PASS
           ELSEIF (INTMP.EQ.1.AND.ITH_IJ.EQ.1) THEN

                NODELIST(IJ,1)=SIGN(HOLD_2,HOLD_1)
                NODELIST(IJ,2)=SIGN(HOLD_1,HOLD_2)
                ITH_IJ=2
                GOTO 4200                       !try again with rs


           else           !no match after both passes, or success
                if (intmp.eq.1) then    !no matches, but rs exists
                     anode=hold_1
                     tmpnode=hold_2     !restore originals
                endif

                if (.NOT.IS_VVFI(iabs(anode))) then
                    if (anode.gt.0) then
                        anode=cd_oprn2(anode)
                    else
                        anode=-cd_oprn2(iabs(anode))
                    endif
                endif
                if (.not.is_vvfi(tmpnode)) then
                    tmpnode=cd_oprn2(tmpnode)
                endif

                newnode=calc_mathnode2(mult,iabs(anode),tmpnode)
                nodelist(ij,1)=newnode
                nodelist(ij,2)=1
                if (anode.lt.0) nodelist(ij,2)=-nodelist(ij,2)  ! the sign

           endif    ! ============= INTMP =0,1,..

c         ifoo1=nodelist(ij,1)
c         ifoo2=nodelist(ij,2)
c              call tree_string(fstring,ifoo1,3,.false.,0)
c             print *,' nodelist, ij,sign= ',ij, ',' ,ifoo2,' , ',
c     1            fstring(1:len_trim(fstring))

c  Get rid of the nodes incoprorated into x( A + B + ..)
            call krunch_list(nodelist,in_nodelist)   !gets rid of zeroed
            ij=ij+1
        enddo                           !IJ


 4400   continue

c --------------------------------------
c now, make an iopr node containing posnodes, negnodes, vlnode
c uses node and sign info from nodelist

        posnodes=0
        negnodes=0
        do ij=1,in_Nodelist                     ! do + nodes first
           anode=nodelist(ij,1)
           isign=nodelist(ij,2)

c             call tree_string(fstring,anode,3,.false.,0)
c             print *,' nodelist, ij,sign= ',ij, ',' ,isign,' , ',
c     1            fstring(1:len_trim(fstring))

c the sign -- hence whether a psonode or a negnode, depends on sign of anode,
c isign, and the signs of inls inrs

          if (is_neg(anode))  then
               qtmp=make_itneg(anode,2)   !make it +
                isign=-isign
          endif

           if (is_math2(anode,div)) then    !put parens around
                anode=cd_oprn2(anode)
           endif

           if (isign.gt.0) then       !+ nodes
              if (posnodes.eq.0 ) then
                  posnodes=anode
              else
                 posnodes=calc_mathnode2(add,posnodes,anode)
              endif
c              call tree_string(fstring,posnodes,3,.false.,0)
c              print *,' posnodes= ',fstring(1:len_trim(fstring))
           else
              if (negnodes.eq.0 ) then
                  negnodes=anode
              else
                 negnodes=calc_mathnode2(add,negnodes,anode)
              endif
c              call tree_string(fstring,negnodes,3,.false.,0)
c              print *,' negnodes= ',fstring(1:len_trim(fstring))
            endif
        enddo


c depending on valu of vlnode, add to posnodes or negnodes
        if (vlnode.ne.0) then
          if (is_neg(vlnode)) then
             qtmp=make_itneg(vlnode,2)
             if (negnodes.ne.0) then
                negnodes=calc_mathnode2(add,vlnode,negnodes)
              else
                 negnodes=vlnode
              endif
          else
            if (posnodes.ne.0) then
                posnodes=calc_mathnode2(add,vlnode,posnodes)
            else
                posnodes=vlnode
            endif
          endif
       endif

c now, form posnodes - negnodes
c note, if here, we have at least one node
        if (posnodes.ne.0 .and. negnodes.eq.0) then  !no - nodes
            THE_mondo=posnodes
        elseif (posnodes.eq.0 .and. negnodes.ne.0) then  ! no + or values
            if (make_itneg(negnodes,2)) then
                THE_mondo=negnodes
            endif
        elseif (posnodes.eq.0 .and. negnodes.eq.0) then  !valu of zero
            call calc_error(' ! bad location in  mondo_add ')
            RETURN
        else
            if (.not.is_vvfi(negnodes)) then      !put () around list only
                 negnodes=cd_oprn2(negnodes)
            endif
            THE_mondo=calc_mathnode(minus,posnodes,negnodes)
        endif

        GOTO 3000


 3000   CONTINUE     ! RETURN FROM HERE
c           call tree_string(fstring,the_Mondo,3,.false.,0)
c          print *,' now equal ',fstring(1:len_trim(fstring))
        MONDO_ADD=THE_MONDO
        return

        end


c==========================
c compare nodes and make x(a+B...

        subroutine mondo_add1(anode,tmpnode,intmp,bnode,cnode,iok)
        implicit none

        integer anode,tmpnode,intmp,bnode,cnode,iok
        integer kompare_trees,make_valnode4,mondo_add2
        integer ima,intmp

c is it legit bnode
        if (bnode.eq.0) then
                iok=0          !nope, its not
                return
        endif

c  note: sign of bnode and cnode can be + -

C COMPARE..
         ima=kompare_trees(iabs(anode),iabs(bnode))
         if (ima.eq.0) then
            iok=0
            return
         endif

c else match
         if (cnode.eq.0) cnode=make_valnode4(1.0)

C adjust  INFO of CNODE
         if (bnode.lt.0) cnode=-cnode   !if -b*c, retain as -c (drop b)

         if (intmp.eq.0) then      !nothing in tmp, bnode is 0
                  tmpnode=make_valnode4(1.0)  !do here to avoid making unused
                  if (anode.lt.0) tmpnode=-tmpnode
                  anode=iabs(anode)             !moved any - to tmpnode
                  INTMP=1
         elseif (intmp.eq.1) then        !the original rs
                  if (anode.lt.0) tmpnode=-tmpnode
                  anode=iabs(anode)             !moved any - to tmpnode
         endif                             !nodes
c ADD CNODE TO TMPNODE (PAY ATTENTION TO -CNODE)
         tmpnode=mondo_add2(tmpnode,cnode)
         intmp=intmp+1
         iok=1

        return
        end

c-------------------------
c make a + node, convert sign of components
c note: -node1 only possible on first call when building tmpnode
        integer function mondo_add2(node1,node2)
        implicit none

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        logical qtmp,is_math2
        integer node1,node2,tmp,make_oprnode,cd_oprn2
        logical  make_itneg

        tmp=make_oprnode(float(add))
        if (node1.lt.0) then
           node1=-node1
           if (is_math2(node1,exp).or.is_math2(node1,div)) then
              node1=cd_oprn2(node1)
           endif
           qtmp=make_itneg(node1,2)
        endif
        if (node2.lt.0) then
           node2=-node2
           if (is_math2(node2,exp).or.is_math2(node2,div)) then
              node2=cd_oprn2(node2)
           endif
           qtmp=make_itneg(node2,2)
        endif
        call left_assign(tmp,node1)
        call right_assign(tmp,node2)

        mondo_add2=tmp
        return
        end


c--------------------------
c compress a*b*c*d*e* (f/g)*h ..., where A.. can be I^J
c alternatively, compress (a*b*c)/(d*e*f) (mondo_div)

        integer function mondo_mult(nodedo)

        implicit none

        integer max_nodelist
        parameter (max_nodelist=100)

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

c       CHARACTER *160 FSTRING

        integer LsNotdone,LsDone,LsNone,Lsfunk
        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        integer nodedo,lsstat,tmpnode
        integer *2 numlist(max_nodelist,2),denlist(max_nodelist,2)

        logical is_math2,is_valu,ismult,doing_num
        logical qtmp,make_itneg

        integer get_right,get_left,isign,isign0,in_nodelist

        integer ij,INLS,INRS,ifoo,inls2,inrs2,ijj,iflag
        integer anode,bnode,anode2,bnode2,inden,innum

        real *8 get_val8,aval8,tmp8
        integer cd_oprn2,make_valnode,make_valnode4
        integer calc_mathnode,calc_mathnode2,kompare_trees,MONDO_MULT3
        integer numnode,dennode,mult_list,valnode,ith0

        integer mondo_div

        mondo_mult=0             !assum failure
        ismult=.true.
        goto 2

c div is similar.
        entry mondo_div(nodedo)
        mondo_div=0
c        call show_anode(nodedo,' mondodiv ',nodedo)
c        call show_anode(get_left(nodedo),'l mondodiv ',nodedo)
c        call show_anode(get_right(nodedo),'r mondodiv ',nodedo)

        ismult=.false.

 2      continue                                !START mult and div
        if (ismult) then
           if (.not.(is_math2(nodedo,mult))) then
             return            !nb, mondo_mult set=0 above
          endif
        else
           if (.not.(is_math2(nodedo,div))) then
             return            !nb, mondo_mult set=0 above
          endif
        endif

c       call show_anode(nodedo,' start ',nodedo)

        in_nodelist=0
        anode=nodedo
        call calc_pushd(Anode,nil,nil,nil)    !brake node
        isign=1                 !assume ( a +- b), so top is +
        aval8=1.0
        doing_num=.true.                !used by mondo_div
        ith0=0

 100    continue                      !entry point (anode,isign)

c if 3rd arg is 1, then we are returning from the ls of a DIVISION.
c hence, all future nodes will be from Denominator, thus
c output of mondo_mult1 should be switched
        if (ith0.eq.0.and. (.not.ismult)) then    !first pass on div
c note: this line is the ONLY line that used ismult or ith0 !
            call calc_pushd(anode,lsnotdone,1,0)  !set up for return to num
        else
            call calc_pushd(anode,lsnotdone,0,0)  !not relevant
        endif
        ith0=ith0+1     !# of completed passes (not used except as flag

        INLS=get_left(anode)
        call ferret_iopr(INLS,anode,isign0)
        isign=isign*isign0

c ls is a * , so go down.
        if (is_math2(anode,mult)) then
            goto 100
        endif

c else, this is a terminal node.  Check for / and ^, save in numlist and
c denlist. But first, are we out of room ?
 150    in_nodelist=in_nodelist+1
        if (in_Nodelist.gt.max_nodelist) then   !too many, give up
           call calc_error(' ! Product list too long to simplify ')
           return
        endIF

c get nodes (check for / and ^ ), and maintain correct sign
        call mondo_mult1(anode,inls,inls2,inrs,inrs2,isign)

        if (doing_num) then
           numlist(in_nodelist,1)=inls
           numlist(in_nodelist,2)=inls2
           denlist(in_nodelist,1)=inrs
           denlist(in_nodelist,2)=inrs2
        else
           numlist(in_nodelist,1)=inrs
           numlist(in_nodelist,2)=inrs2
           denlist(in_nodelist,1)=inls
           denlist(in_nodelist,2)=inls2
        endif

c --- return.
 200    continue
        call calc_popd(anode,lsstat,iflag,ifoo)
        if (iflag.ne.0) then           !oh boy, turn on divswitch
           doing_num=.false.
        endif
        if (lsstat.eq.nil) then                 !all done
            goto 1000                           !next step

        elseif (lsstat.eq.lsnotdone) then           !now do rs
            call calc_pushd(anode,lsdone,0,0)  !set up for return
            INRS=get_right(anode)
            call ferret_iopr(INRS,anode,isign0)
            isign=isign*isign0                      !adjust for -(

            if (is_math2(anode,mult)) then
                goto 100                !go down rs
            else
               goto 150                    !terminal node
            endif

        elseif (lsstat.eq.lsdone) then          !has to be this
            goto 200                            !done with this node

        endif

c---
 1000   continue                !here when got all nodes

c note: all nodes are positive, and stripped of (). NOte that the
c making of nodes positive (and stripping of -( ) is reflected in
c the value of isign. This value of isign will not change below.

c first, extract math elememts from numerator and denominator lists

        do ij=1,in_nodelist
           anode=numlist(ij,1)
           anode2=numlist(ij,2)
           bnode=denlist(ij,1)
           bnode2=denlist(ij,2)
           if (anode.ne.0.and.anode2.eq.0) then
             if (is_valu(anode)) then
                aval8=aval8*get_val8(anode)
                numlist(ij,1)=0
             endif
           endif
           if (bnode.ne.0.and.bnode2.eq.0) then
              if (is_valu(bnode)) then
                  tmp8=get_val8(bnode)
                  if (tmp8.ne.0) then
                      aval8=aval8/get_val8(bnode)
                  else
                     call calc_error(
     1                  ' ! Divide by zero in simplification ')
                  endif
                  denlist(ij,1)=0
              endif
           endif
        enddo

        inden=in_Nodelist
        innum=in_nodelist
        call krunch_list(numlist,innum)         !remove value nodes
        call krunch_list(denlist,inden)

c        print *,' aval ',aval8
c        PRINT *,' AFTER REMOVING VALUES ',INNUM,INDEN,AVAL8
c       CALL PRINT_MULT(NUMLIST,INNUM,DENLIST,INDEN)

c Compare each element of denlist against elements of numlist
        do ij=1,inden
           bnode=denlist(ij,1)
           bnode2=denlist(ij,2)
           if (bnode.eq.0) then
                print *,' Zero bnode in mondo mult '
                stop
           endif
           do ijj=1,innum
              anode=numlist(ijj,1)
              anode2=numlist(ijj,2)
              if (anode.ne.0) then
                 IF (KOMPARE_TREES(anode,bnode).ne.0.) THEN ! same base
                    if (anode2+bnode2.eq.0) then  !x/X
                       denlist(ij,1)=0
                       numlist(ijj,1)=0       !they cancel out
                       exit
                    ELSE
                        NUMLIST(IJJ,2)=MONDO_MULT3(ANODE2,BNODE2,0)
                        denlist(ij,1)=0
                        exit
                     endif                !a+b=0
                 endif          !anode=bnode
              endif             !anode not canceled
           enddo                !ijj
        enddo                   !ij

c clean er up
        call krunch_list(numlist,innum)         !num/den nodes
        call krunch_list(denlist,inden)


c       PRINT *,' AFTER NUM/DEN CANCEL ',INNUM,INDEN
c       CALL PRINT_MULT(NUMLIST,INNUM,DENLIST,INDEN)


c we now have no common elements between both denominator and numerator.
c Now, look to combine remaining elements in numerator and denominator

c first, for numerator (DOES NOTHING IF INNUM=0)
        CALL MONDO_MULT2(NUMLIST,INNUM)

C THEN FOR DENOMINATOR (DOES NOTHING IF INDEN=0)
        CALL MONDO_MULT2(DENLIST,INDEN)

c clean er up
        call krunch_list(numlist,innum)         !remove repettive nodes
        call krunch_list(denlist,inden)

c       PRINT *,' AFTER MONDO MULT2 ',INNUM,INDEN
c       CALL PRINT_MULT(NUMLIST,INNUM,DENLIST,INDEN)


c First, we combined all values. Then, we
C  compared element by element numerator vs denominator, and
c remove all elements possible.  And we then compared across numerator
c and denominator separately, combining exponents when possible.
c Now, put it all together


c incorporate sign and values. BUt is it all irrelevant ?
        if (aval8.eq.0) then
            tmpnode=make_valnode4(0.0)
            if (ismult) then
                  mondo_Mult=tmpnode
            else
                  mondo_div=tmpnode
            endif
            return                       !don't matter what everything else is
        endif

c nope, not multiply by 0

        IF (INNUM.GT.0) THEN
            numnode=mult_list(numlist,innum)
            if (isign*aval8.ne.1.0) then
                if (isign*aval8.eq.-1.0) then
                    qtmp=make_itneg(numnode,2)
                else
                    valnode=make_valnode(isign*aval8,valu)
                    numnode=calc_mathnode2(mult,valnode,numnode)
                endif
            ENDIF
        ELSE
            NUMNODE=make_valnode(isign*aval8,valu)
        ENDIF

        if (inden.eq.0) then             !no denominator, we are done
           if (ismult) then
               mondo_mult=numnode
           else
               mondo_div=numnode
           endif
           return
        endif

c do denominator
        dennode=mult_list(denlist,inden)
        if (inden.gt.1) then                    !make num/(a*B)
            dennode=cd_oprn2(dennode)
        endif

c now, we have numerator and denominator.  Return (A*B*C/(D*E*F)), where
c A,.. may be of form I^J
        anode=calc_mathnode(div,numnode,dennode)

        if (ismult) then
            mondo_mult=anode
        else
            mondo_div=anode
        endif
        return
        end


c------------------------------
c multiply all elements of a list (list contains base, exponent)
c if empty, return 0
        integer function mult_list(alist,inlist)

        implicit none

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP


        integer max_nodelist
        parameter (max_nodelist=100)

        integer inlist,calc_mathnode2,anode,anode2,newnode,ij,thelist
        logical is_math,is_neg
        integer cd_oprn2
        integer *2 alist(max_nodelist,2)

        THELIST=0
        do ij=1,inlist                   !now get denominator elements
               anode=alist(ij,1)
               if (is_math(anode).or.is_neg(anode))
     1                anode=cd_oprn2(anode)
               anode2=alist(ij,2)
               if (anode2.ne.0) then
                  newnode=calc_mathnode2(exp,anode,anode2)
               else
                  newnode=anode
               endif
               if (is_math(newnode)) newnode=cd_oprn2(newnode)
               if (ij.eq.1) then
                   thelist=newnode
               else
                   thelist=calc_mathnode2(mult,thelist,newnode)
               endif
        enddo
        mult_list=thelist
        return
        end

C----------------------------------
C COMPRESS LIST OF PRODUCTS + EXPONENTS. mODIFIES NUMLIST DIRECTLY

        SUBROUTINE MONDO_MULT2(ALIST,INNUM)
        implicit none

        integer max_nodelist
        parameter (max_nodelist=100)


        integer *2 Alist(max_nodelist,2)
        INTEGER IJ,INNUM,ANODE,ANODE2,BNODE,BNODE2,IJJ
        INTEGER KOMPARE_TREES,MONDO_MULT3

        do ij=1,innum-1
            anode=Alist(ij,1)
            if (anode.eq.0) cycle     !already done
            anode2=Alist(ij,2)
            do ijj=ij+1,inNum           !compare against later elements
               Bnode=Alist(iJj,1)
               IF (BNODE.EQ.0) CYCLE   !ALREADY DONE
               Bnode2=Alist(iJj,2)
               IF (KOMPARE_TREES(anode,bnode).ne.0.) THEN
                   ANODE2=MONDO_MULT3(ANODE2,BNODE2,1)
                   ALIST(IJJ,1)=0
                ENDIF
            ENDDO
            aLIST(IJ,2)=ANODE2                !ANODE2 CONTAINS CHANGES
        ENDDO

        RETURN
        END



C-------------------
C ADD UP NODES, IF NODES=0, ASSUME VALUE OF 1.0
        INTEGER FUNCTION MONDO_mULT3(N1,N2,IADD)
        IMPLICIT NONE

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        INTEGER CALC_MATHNODE,N1,N2,MAKE_VALNODE4,IADD

        IF (N1.EQ.0) THEN
           N1=MAKE_VALNODE4(1.0)
        ENDIF
        IF (N2.EQ.0) THEN
           N2=MAKE_VALNODE4(1.0)
        ENDIF
        IF (IADD.EQ.1) THEN
            MONDO_MULT3=CALC_MATHNODE(ADD,N1,N2)
        ELSE
            MONDO_MULT3=CALC_MATHNODE(MINUS,N1,N2)
        ENDIF
        RETURN
        END


c---------------------------
c return elements of node.  4 elememts are:
c numerator, exponent of numerator,
c denominator, exponent of denominator
c for exp of num, den, and exp of denom, 0 means "does not exist"
c there will ALWAYS be a numerator.
c also update value of sign

        subroutine mondo_mult1(anode,anum,anume,aden,adene,isign)

        implicit none

        integer anode,anum,anume,aden,adene,isign
        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        logical is_math2,iseq_neg,make_itneg,qtmp
        logical isdiv,isexp
        integer get_right,get_left,isign0
        integer inls,inrs,inls2,inrs2

        isdiv=is_math2(anode,div)
        isexp=is_math2(anode,exp)

        if (.not.(isdiv.or.isexp)) then     !just a terminal node
           anum=anode
           if (iseq_neg(anum)) then
                qtmp=make_itneg(anum,2)
                isign=-isign
           endif
           anume=0
           aden=0
           adene=0
           return
        endif

c else, if ^, then break out base and exponent
        if (isexp) then
           call get_base_exp(anode,anum,anume,isign)
           aden=0
           adene=0
           return
        endif


c else, division. Check both num and den for ^

        inls=get_left(anode)
        call ferret_iopr(inls,inls2,isign0)
        isign=isign*isign0
        if (is_math2(inls2,exp)) then           !exponent
           call get_base_exp(inls2,anum,anume,isign)
        else
           anum=inls2
           anume=0
        endif

        inrs=get_right(anode)
        call ferret_iopr(inrs,inrs2,isign0)
        isign=isign*isign0
        if (is_math2(inrs2,exp)) then           !exponent
              call get_base_exp(inrs2,aden,adene,isign)
          else
              aden=inrs2
              adene=0
         endif

        return
        end

c----------------------------
c extract base and exponent, and correct sign
        subroutine get_base_exp(anode,anum,anume,isign)
        implicit none
        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer anum,anode,anume,isign,get_left,get_right,anum0,isign0
        logical iseq_neg,make_itneg,is_valu,qtmp
        real *8 get_val8,aval8


        anum0=get_left(anode)
        call ferret_iopr(anum0,anum,isign0)

c this is  a bit of ahack, since we may negate it below, but ...
        if (isign0.lt.0) qtmp=make_itneg(anum,2)  !move (-(a))^x into a

        anume=get_right(anode)

c we can extract neg from base iff exponent is a whole number
c if even, cancel - but do NOT change sign.  if odd, cancel - and Change sign
        if (iseq_neg(anum)) then   !see if we can extract -
           if (is_valu(anume)) then
              aval8=get_val8(anume)
              if (dmod(aval8,2.0).eq.0) then    !-x^(2*i), some i
                qtmp=make_itneg(anum,2)
              elseif (dmod(aval8,2.0).eq.1.0) then !-x(2*i+1),some i
                qtmp=make_itneg(anum,2)
                isign=-isign
             endif                      !dmode
           endif                !isvalu
        endif                  !iseqneg
        return
        end

c--------------------
c remove zeor nodes from list
        subroutine krunch_list(alist,inalist)
        implicit none
        integer max_nodelist
        parameter (max_nodelist=100)

        integer *2 alist(max_nodelist,2)

        integer inlist,igot,inalist

        inlist=0
        igot=0
        do while (igot.lt.inalist)

c first, find a non zero node

           igot=igot+1
           if (alist(igot,1).ne.0) then          !copy igot to inlist
                inlist=inlist+1
                if (inlist.ne.igot) then         !else, no need to copy
                    alist(inlist,1)=alist(igot,1)
                    alist(inlist,2)=alist(igot,2)
c                   alist(inlist,3)=alist(igot,3)
c                   alist(inlist,4)=alist(igot,4)
                endif
           endif
        enddo

        inalist=inlist
        return
        end

