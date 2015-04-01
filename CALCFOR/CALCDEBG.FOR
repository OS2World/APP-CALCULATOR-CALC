c @c for the public domain. D. Hellerstein,  ERS/RTD, 11/91
C=================================================
c The following a little debug utility that allows one to see what
c has happened to the calc stack.  It is included for the hell of it,
c as the ambitious programmer may find it somewhat useful.
c
C========================================================
c a  debug routine

        SUBROUTINE CALC_DEBUG(qout,iout)


C BASIC DATA STRUCTURE

        PARAMETER (maxnode=6000)
        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)
        PARAMETER (MAXVARI=100)

        character *10 afunk,ascird_2
        character *20 aa,ascird_2b

        common /calc_CALCDEBUG/NODED

        logical qout


 10     call down_line(3)

        PRINT *,'Note: Last analysis (or DeBug) used tree @ node #: ',
     1          noded

        print *,' (hit ESC to return to CALC)'
        afunk=ascird_2(
     1     ' Select function to display (ENTER to select a node):',
     2      iarg)
        if (iarg.gt.255 .or. iarg.eq.27) return

        call captaliz(afunk)
        if (afunk.eq.' ') then
           n1=valdef(' Select a node to display: ',float(noded))
           if (n1.le.0.or.n1.gt.maxnode) goto 10      !allow for backup
        else
            call find_func(afunk,fnum)                !FIND FUNCTION ID
            if (fnum.eq.0) then
                 print *,' No such function:',afunk
                 goto 10
            endif
            n1=calc_Get_FunkAddress(Fnum)
            print *,' Note: requested function starts at node # ',n1
        endif
        noded=n1                        !facilitate next call to debug

 2020   iopt=valdef(
     1    ' Option: 1=View tree, 2=View Nodes, 3=Tree stats',3.)
        if (iopt.lt.1.or.iopt.gt.3) goto 10

        if (iopt.eq.1) then
           call show_anode(n1,' (sub) Tree starting at node:',n1)
           call down_line(2)
           goto 10

        elseif (iopt.eq.3) then
           call tree_stats(n1,ntotal,ndepth,nprimary,nterminal)
           print *,'         Total # of nodes: ',ntotal
           print *,'            Depth of tree: ',ndepth
           print *,'   # of first level nodes: ',nprimary
           print *,'  # of values & variables: ',nterminal
           print *,' '
           goto 10
         endif

c else, show tree

        iper=valdef(' # of nodes to view per page ',15.)
        if (iper.lt.1) goto 10

        WRITE(6,13)
 13     FORMAT( 21x,' node#       OPR         VALUE     ::',
     1           '   LS     RS ')

        ijj=n1
        n1do=n1
 400    continue
        n2do=min(n1do+iper,maxnode)

c do 15 at a time
        do ij=n1do,n2do
              call print_node(ij,' ',6)
             if (qout)   call print_node(ij,' ',iout)

        END DO


        aa=ascird_2b(' more (PgUp,PgDn,ENTER:',iarg,' ')
        if (iarg.eq.18688) then
           n1do=max(1,n1do-iper)
           call down_line(1)
        elseif (iarg.eq.18432) then
           n1do=max(1,n1do-1)
        elseif (iarg.eq.20736)then
           n1do=min(maxnode,n1do+iper+1)
        elseif (iarg.eq.20480) then
           n1do=min(maxnode,n1do+1)
        elseif (iarg.eq.27) then
                goto 10
        else

           if (aa.eq.' ') goto 10    !if ENTER hit
           n1do=val(aa,ierr)
           if (ierr.gt.0) then
              goto 10
           else
              n1do=min(max(1,n1do),maxnode)
           endif
        endif

        goto 400

        END


c==========================================
c Write a line

        subroutine print_node(ij,acomm,iout)

        PARAMETER (maxnode=6000)
        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)
        PARAMETER (MAXVARI=100)

        character *(*) acomm

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP

        INTEGER BP,EP

        common /calc_PTYPE/BP,EP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_NODES/NODE,LS,RS,NIL
        common /calc_NODEVALUE/VALUE,OPR
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        CHARACTER *12 VARINAMES(MAXVARI)
        REAL *8 VariValues(maxvari)
        common /calc_VARIABLES/NVARI,VARINAMES,VariValues

       character *70 aline
        character *16 foo2,fname

        integer funkaddress(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        CHARACTER *12 FUNKargs(ntmpfunk)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        common /calc_CALCDEBUG/NODED


        CHARACTER *10 ATYPES(7),APARENS(2)
        DATA ATYPES/
     2   'IOPR','ADD','MINUS','MULT','DIV','EXP','MODOP'/
        DATA APARENS/' ( ',' ) '/

        aline(1:max(15,len_trim(acomm)))=acomm

            write(aline(16:20),91)ij            ! node #

            nopr=opr(ij)
            select case (iabs(nopr))          !display type of operator



            case(6)
                foo1='Eqend'
                foo2=' '
            case(1)
                foo1='Value'
                write(foo2,'(f12.5)')value(ij)
            case(2)
                foo1='MathOper'
                I11=VALUE(IJ)
                if (i11.gt.0) then
                   FOO2=ATYPES(iabs(I11))
                else
                   foo2(1:2)=' -'
                   foo2(3:16)=atypes(iabs(i11))
                endif
            case(3)
                foo1='Paren'
                I11=VALUE(IJ)
                FOO2=APARENS(iabs(I11))
            case(4)
                foo1='Function'
                I11=VALUE(IJ)
                if (i11.gt.0) then
                    FOO2=FUNKNAMES(iabs(I11))
                else
                   foo2(1:1)=' -'
                   foo2(3:16)=funknames(iabs(i11))
                endif
                if (i11.gt.npermfunk) then
                   if (funkaddress(i11-npermfunk).eq.0) then
                      foo2(15:16)='/d'
                   endif
                endif
            case(5)
                foo1='Variable'
                I11=VALUE(IJ)
                if (i11.gt.0) then
                    FOO2=VARINAMES(iabs(I11))
                else
                   foo2(1:2)=' -'
                   foo2(3:16)=varinames(iabs(i11))
                endif
            CASE(0)
                FOO1='unused '
                foo2=' '
            case default
                foo1='Error'
            end select

            write(aline(23:50),993)foo1,foo2    !store results

           fname=' '
           do m=1,ntmpfunk
                if (funkaddress(m).eq.ij) then
                   fname='( '//funknames(m+npermfunk)
                   exit
                endif
           enddo

            WRITE(iout,881)ALINE,NODE(IJ,LS),NODE(IJ,RS),fname   !output it


 993       format(a,2X,a)
 881       FORMAT(1X,A51,1x,2I4,2x,a)

 91         format(i4)

        return
        end

c========================

        subroutine show_anode(anode,prompt,iat)

        character *(*) prompt
        character *200 fstring

        integer anode

        call tree_string(fstring,anode,3,.false.,0)
        print *,prompt,', ',iat,', ', fstring(1:len_trim(fstring))

        return
        end


C--------
        SUBROUTINE PRINT_MULT(NUMLIST,INNUM,DENLIST,INDEN)
        INTEGER *2 NUMLIST(100,2),DENLIST(100,2)
        INTEGER ANODE,BNODE
        CHARACTER *180 FSTRING
        DO IJ=1,INNUM
          ANODE=NUMLIST(IJ,1)
          IF (ANODE.LE.0) THEN
                PRINT *,' NUM IJ = 0 @ ',IJ,', ',ANODE
                CYCLE
          ENDIF
          CALL       Tree_string(fstring,anode,4,.false.,0)
          PRINT *,' num: IJ,1) ',IJ,' , ',FSTRING(1:50)
          BNODE=NUMLIST(IJ,2)
          IF (BNODE.GT.0) THEN
             CALL       Tree_string(fstring,Bnode,4,.false.,0)
             PRINT *,'    ,2) ',FSTRING(1:50)
         ENDIF
        ENDDO
        DO IJ=1,INDEN
           ANODE=DENLIST(IJ,1)
           IF (ANODE.LE.0) THEN
                PRINT *,'DEN IJ = 0 @ ',IJ,ANODE
                CYCLE
           ENDIF
           CALL       Tree_string(fstring,anode,4,.false.,0)
           PRINT *,'den: IJ,1) ',IJ,' , ',FSTRING(1:50)
           BNODE=DENLIST(IJ,2)
           IF (BNODE.GT.0) THEN
              CALL       Tree_string(fstring,Bnode,4,.false.,0)
              PRINT *,'    ,2) ',FSTRING(1:50)
           ENDIF
        ENDDO
        RETURN
        END

c============================================
c subroutine to compute tree statistics.
c Total number of nodes (including ALL nodes
c Depth of tree  (# of nodes in longest chain)
c # of primary nodes (valu,func,vari, or paren nodes not nested in ( ) )

        subroutine tree_stats(base,
     1                        totnodes,maxdepth,primaries,terminals)
        implicit none

        integer maxnode
        PARAMETER (maxnode=6000)

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer LsNotdone,LsDone,LsNone,Lsfunk,lsstat
        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        real *8 opvalue

        integer parenct,wasnode,depth,totnodes,maxdepth,PRIMARIES
        integer terminals,inrs,inls


        Integer Base                    !the base of tree to be analyzed
        Integer Optype,CurrentNode,NewNode
        LOGICAL QOPR,QFUNK,IS_FUNC,IS_IOPR

        if (base.le.0 .or. base.gt.maxnode) then
           print *,' No tree at ',base
           return
        endif

c  calc_Push a nil to act as a brake
        Call calc_Pushd(nil,nil,nil,0)

c Get First Node.

        CurrentNode=base
        Call GET_NodeINFO(CURRENTNODE,OPTYPE,OPVALUE,INLS,INRS)

        parenct=0               !# of active parenthesis
        wasnode=nil             ! parent of currentnode  (not used )
        depth=0                 ! depth of currentnode
        totnodes=0              ! tot# of nodes encountered
        maxdepth=0              !maximum depth encountered
        PRIMARIES=0             ! # of nodes NOT nested in ( )
        TERMINALs=0              ! # of terminal (value or variable) nodes


c  -- the ENTRY point.
 100    Continue

         totnodes=totnodes+1          !total # of nodes

         depth=depth+1
         maxdepth=max(maxdepth,depth)

         IF (parenct.eq.0) PRIMARIES=PRIMARIES+1

c Case 1 :: IOPR or FUNK.
        qopr=is_iopr(currentnode)
        qfunk=is_func(currentnode)
        if (qopr.or.qfunk) then
          if (qopr) then
              parenct=parenct+1
             Call calc_Pushd(Lsnone,CurrentNode,wasnode,depth)
          elseif(qfunk) then
              Call calc_Pushd(Lsfunk,CurrentNode,wasnode,depth)
          endif

          CurrentNode=INRS            !now analyze RS argument.
          Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)

          goto 100
        endif

C Case 2:: VALU OR VARI (terminal node)

        If (OpType.eq.VALU.or.OPTYPE.eq.VARI) then
                TERMINALs=TERMINALs+1
                GOTO 200
        ENDIF

c  CASE 3:: we have a math oper. So, we need to get both left and right nodes.
c  Record this "state" on Stack.

        Call calc_Pushd(LsNotDone,CurrentNode,wasnode,depth)  !save left and

        CurrentNode=InLs                        !start with left side
        Call Get_NodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)

        Goto 100


C HERE After Succesful SUBTREE analysis, or if Node is simple (valu or VARI)

 200       continue                             !jump here if returning from subtree analysis

           call calc_popd(lsstat,newnode,wasnode,depth)

           IF (NewNode.eq.Nil) Then             !equation displayed


             RETURN                             !so Return
           endif

           IF (LsStat.eq.LsNotDone) then   !returning from Left node (of oper)


              Call Get_nodeInfo(Newnode,OPtype,OPvalue,InLS,InRS)

              Call calc_Pushd(LsDone,NewNode,wasnode,depth)  !ready to get RS

              wasnode=newnode
              CurrentNode=inrs

              Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)

              goto 100

           else                                 !returning to Right Node

              if (lsstat.eq.lsnone) then
                  parenct=parenct-1
              endif                     !else, lsfunk or lsdone
              goto 200                  !LS or RS subtree value for higher node

           endif                       !Return to LS or RS

        print *,' Error in tree_stats '
        stop

        End



