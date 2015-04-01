c'@c for the public domain, D. Hellerstein, ERS/RTD , 11/91

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
        PARAMETER (maxnode=6000)

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
        PARAMETER (maxnode=6000)

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

        SUBROUTINE calc_garbageman(forceit) !garbage_funk calls calc_garbageman

        IMPLICIT NONE
        INTEGER MAXNODE,MAXFUNK,NPERMFUNK,NTMPFUNK,maxstak2,forceit

        PARAMETER (maxnode=6000)
        PARAMETER (maxstak2=1750)
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


        if (forceit.eq.0) then
          if (freenodes.gt.(MAXNODE/2)) then
            return                        !dont bother
          endif
       else                 !do it in all but most silliest cases
          if (freenodes.ge. (0.99*maxnode)) then
            return                        !dont bother
          endif
        endif

        do ij=1,maxnode
           dirtynode(ij)=0
        end do

        if (nfunk.Le.npermfunk) then            !no functions to mark
           GOTO 200
        else
          do ij=npermfunk+1,nfunk
             nnode=funkaddress(ij-npermfunk)
             if (nnode.eq.0) cycle
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


        PARAMETER (maxnode=6000)

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

        PARAMETER (maxstak2=1750)

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


        PARAMETER (maxnode=6000)
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

        common   /cm_simp/
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra
        integer
     1     doiter,doparen,doneg,domath,dofunc,dosimple,doalgebra


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

c ANSI sequences used in print statements  for bold,block,blink and normal
        character *4 ablink,abold,ablock,anormal
        common /calc_screen/ablink,abold,ablock,anormal

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

        doiter=1                !assume total simplification
        doparen=1
        doneg=1
        domath=1
        dofunc=1
        dosimple=1
        doalgebra=1


        abold(1:1)=char(27)
        abold(2:2)='['
        abold(3:3)='1'
        abold(4:4)='m'
        anormal=abold
        ablock=abold
        anormal(3:3)='0'
        Ablock(3:3)='7'
        ablink=anormal
        ablink(3:3)='5'




        RETURN
        END



C=========================================================

        FUNCTION calc_GETFREE()

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=6000)

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

        PARAMETER (maxnode=6000)

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

        PARAMETER (maxnode=6000)

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

        PARAMETER (maxnode=6000)

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

        PARAMETER (maxnode=6000)

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

        PARAMETER (maxnode=6000)

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
        PARAMETER (maxnode=6000)

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

        PARAMETER (maxnode=6000)

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


C=================================================
c copy a node to a new node -- function value is address of new node
c (nil if couldn't make one )
C NODEADRESS - OLD NODE.  nNODE, NEW NODE (0=CREATE ONE)

        function copy_Node(nodeaddress,nnode)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=6000)

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

        if (nnode.le.0) then            !don't have a spot
            newnode=calc_getfree()   ! SO GET ONE
        else                            !got a particular spot
            newnode=nnode
        endif

        if (newnode.eq.nil) then        !no more space
           copy_node=nil
        else                    ! copy it
           if (nodeaddress.le.0 .or. nodeaddress.gt.maxnode) then
                call calc_error(' ! Bad NodeAddress: ')
           else
             Opr(newnode)       = Opr(NodeAddress)
             Value(newnode)     = Value(NodeAddress)
             Node(newnode,LS)   = node(nodeaddress,ls)
             NODE(newnode,RS)   = node(nodeaddress,rs)
           endif
           copy_Node=newnode
        endif

        return
        end


C===============================================================
C
C   Routine to return OPTYPE,OPVALUE,LS, and RS of a given NODE
c NOTE, sometimes we dont care about LS and RS.

        Subroutine GET_NodeINFO(NodeAddress,OPTYPE,OPVALUE,INLS,INRS)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=6000)
        PARAMETER (maxstak2=1750)

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
c change info in node nnode: nnode is the node
c ioptype is the type, opval8 (real *8) is the value, inls & inrs are ls and rs
       subroutine change_nodeinfo(nnode,ioptype,opval8,inls,inrs)

        PARAMETER (maxnode=6000)

        real *8 opval8

        integer *2 node(MAXNODE,2)              !4 POINTERS:
        INTEGER LS,RS
        INTEGER NIL                     !NIL POINTER
        common /calc_NODES/NODE,LS,RS,NIL

        REAL *8 VALUE(MAXNODE)
        integer *2 opr(MAXNODE)
        common /calc_NODEVALUE/VALUE,OPR

        IF (NNODE.LE.0 .OR. NNODE.GE.MAXNODE) THEN
           CALL CALC_ERROR(' ! Bad node # in change node ')
        else
           Opr(nnode)       = ioptype
           Value(nnode)     =  opval8
           Node(nnode,LS)   =   inls
           NODE(nnode,RS)   =   inrs
        endif
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

        Subroutine Tree_string(fstring,base,ndec,qe,iwp51,qdo_fmt)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=6000)
        PARAMETER (maxstak2=1750)
        parameter(NPERMFUNK=25)

        logical qdo_fmt,qe
        character *(*) fstring

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        real *8 vls,holdvalue
        real *8 opvalue,nil8

        character *12 get_variname,get_funkname
        character *30 charans

        common /calc_cmerror/nerr

        Integer Base                    !the base of tree to be analyzed
        Integer Optype,CurrentNode,NewNode


        Data LsNotdone,LsDone,LsNone,Lsfunk/10,20,30,40/

c if WP51 Style, use different function
        if (iwp51.eq.1) then
            call Tree_string_wp51(fstring,base,ndec,qe,qdo_fmt)
            return
        endif

c ELSE, CALC Style
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

            if (flagopv.lt.0)  then
                  call calc_put_out(Inum,'-',fstring,1)
            endif

            if (optype.eq.funk) then
               charans=get_funkname(ABS(flagopv))
               call calc_put_out(Inum,charans,fstring,1)
            else
                   call calc_put_out(Inum,'(',fstring,1)
            endif

            CurrentNode=INRS            !now analyze RS argument.

            Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
            flagopv=set_flagval(optype,opvalue  )

            goto 100
        endif

C Case 2:: VALU OR VARI

        If (OpType.eq.VALU.or.OPTYPE.eq.VARI) then

           if (flagopv.lt.0)
     1                 call calc_put_out(Inum,'-',fstring,0)
           IF (OpType.eq.VARI) then
                charans=GET_VariName(flagopv)
           else
                call calc_writenum(opvalue,charans,ndec,qe,nlen)
           endif

           call calc_put_out(Inum,charans,fstring,1)
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
              call calc_put_out(inum,charans,fstring,1)

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


                 call calc_PUT_OUT(Inum,')',fstring,1)  !end of argument
              endif

              goto 200                  !LS or RS subtree value for higher node

           endif                       !Return to LS or RS


        RETURN
        End

c============================================
c write a tree, WP51 style

        Subroutine Tree_string_wp51(fstring,base,ndec,qe,qdo_fmt)

C BASIC DATA STRUCTURE

        PARAMETER (maxnode=6000)
        PARAMETER (maxstak2=1750)
        parameter(NPERMFUNK=25)

        logical qdo_fmt,qe,is_notregfunc
        character *(*) fstring

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk

        real *8 opvalue

        character *12 get_variname,get_funkname,tmpchar
        character *30 charans,expand_apost

        integer parenct,wasnode,nodect,do_suppress,itmp
        logical qmatrix,qmatrix2


        common /calc_cmerror/nerr

        Integer Base                    !the base of tree to be analyzed
        Integer Optype,CurrentNode,NewNode

        integer totnodes,maxdepth,terminals,primaries
        integer inthis_set,inset_2
        INTEGER *2 GETCHASM,A2

        iset_8=8

        inum=1          !initialize string
        fstring='  '          ! ~ signals space after F(X)=
        call calc_put_out(Inum,' ~~ ',fstring,0)

c if lots of nodes, try breaking up - + into matrix form
        call tree_stats(base,
     1                        totnodes,maxdepth,primaries,terminals)

c           print *,'         Total # of nodes: ',ntotal
c           print *,'            Depth of tree: ',ndepth
c           print *,'   # of first level nodes: ',primaries
c           print *,'  # of values & variables: ',nterminal

        if (qdo_fmt.and.(totnodes.gt.30 .or. primaries.gt.10)) then
             qmatrix=.true.
             call calc_put_out(Inum,
     1          ' linespace 200 left\{  matrix {`  ',fstring,0)
             if (primaries.lt.5.or.primaries.lt.totnodes/5) then
                 qmatrix2=.true.
                 write(6,9191)
             else
                 qmatrix2=.false.
                 write(6,9192)
             endif
        else
             qmatrix=.false.
             qmatrix2=.false.
             write(6,9193)
        endif

 9191   format(' MATRIX used for first and second level terms.'\)
 9192   format(' MATRIX used for first level terms.'\)
 9193   format(' MATRIX not used.'\)
        DO IJ=1,200
            A2=GETCHASM()
            IF (A2.NE.0) EXIT
        ENDDO

c  calc_Push a nil to act as a brake
        Call calc_Pushd(nil,nil,nil,0)

c Get First Node.

        CurrentNode=Base
        Call GET_NodeINFO(CURRENTNODE,OPTYPE,OPVALUE,INLS,INRS)
        flagopv=set_flagval(optype,opvalue)
        parenct=0
        wasnode=nil
        nodect=0
        inthis_set=0
        inset_2=0

 100    Continue
        If (nerr.gt.0) return                   !check for error

        nodect=nodect+1    !depth of node (1=top node)


c Case 1 :: IOPR or FUNK.
c   Functions and () are treated similar, except function name is
c   displayed.
c   Regardless of type, we still need to display the subtree that will
c   that is the argument to this function or ().
c   NOTE:: User and perm functions are completely undifferentiated in
c   this routine.

        IF (optype.eq.funk.or.(optype.eq.OPER.and.
     1          abs(flagopv).eq.IOPR)) then   !use abs (watch out for neg functions)


c now display Funk(  or  (

            if (optype.eq.funk) then
               charans=get_funkname(ABS(flagopv))
               charans=expand_apost(charans,1)
               if (.not.(charans(1:4).eq.'SQRT'
     1                 .or.charans(1:3).eq.'ABS')) then
                 if (flagopv.lt.0)   then
                         call calc_put_out(Inum,'-',fstring,1)
                 endif
                 call calc_put_out(Inum,charans,fstring,1)
              else
                 if (flagopv.lt.0)   then
                         call calc_put_out(Inum,'-',fstring,1)
                 endif
                 if (charans(1:3).eq.'ABS') then
                     call calc_put_out(Inum,' ``left | `` ',fstring,1)
                 else
                      call calc_put_out(Inum,charans,fstring,1)
                      call calc_put_out(Inum,'{ ',fstring,0)
                 endif
              endif


            else                                ! Paren , (
                 parenct=parenct+1
                 if (flagopv.lt.0)   then
                        call calc_put_out(Inum,'-',fstring,1)
                 endif
                 itmp=do_suppress(wasnode)  !0=none, 1=yes,2=sqrt,3=abs
                 if (itmp.eq.0) then
                    if (parenct.eq.1
     1                .and. (is_notregfunc(wasnode)) ) then
                        if (nodect.eq.1) then
                           charans=' `` left [ ~~ ' !might change this later
                        else
                           charans=' ``left[ '
                        endif
                    else                         !wp, not 1st paren
                          charans=' ``left( '
                    endif
                    call calc_put_out(Inum,
     1                 charans(1:(len_trim(charans)+1)),fstring,1)

                endif

c first node in a level 1 ( ), and lots of them?
                if (parenct.eq.1.and.qmatrix2) then
                        inset_2=0
                        call calc_put_out(Inum,
     1                          ' `` left \{ matrix{ `` ',fstring,0)
                endif


            endif                       ! funk or (

            if (optype.eq.funk) then
               ll=lsfunk
            else
               ll=lsnone
            endif
c note: + wasnode signals CURRENTNODE IS RS OF WASNODE
c       -wasnode signals CURRENTNODE IS LS OF WASNODE
            Call calc_Pushd(Ll,CurrentNode,wasnode,nodect)

            wasnode=currentnode
            CurrentNode=INRS            !now analyze RS argument.

            Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
            flagopv=set_flagval(optype,opvalue  )

            goto 100
        endif

C Case 2:: VALU OR VARI (terminal node)

        If (OpType.eq.VALU.or.OPTYPE.eq.VARI) then

           if (optype.eq.valu)
     1           call calc_put_out(Inum,'{ ',fstring,0)
           if (flagopv.lt.0)  call calc_put_out(Inum,'-',fstring,0)

           IF (OpType.eq.VARI) then
                charans=GET_VariName(flagopv)
                if (charans.eq.'PI') charans='pi'   !use lc pi
                charans=expand_apost(charans,1)
           else
                call calc_writenum(opvalue,charans,ndec,qe,nlen)
           endif

           call calc_put_out(Inum,charans,fstring,1)
           if (optype.eq.valu)
     1            call calc_put_out(Inum,'} ',fstring,0)
           goto 200
        endif


c  CASE 3:: we have a math oper. So, we need to get both left and right nodes.
c  Record this "state" on Stack.
        itmp=do_suppress(currentnode)  ! { a on { a } / { b},{a}^{b}
        if (itmp.ne.0) then            ! { a on { a } / { b},{a}^{b}
           call calc_put_out(Inum,'{ ',fstring,0)
        endif

        Call calc_Pushd(LsNotDone,CurrentNode,wasnode,nodect)  !save left and

        wasnode=-currentnode
        CurrentNode=InLs                        !start with left side
        Call Get_NodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
        flagopv=set_flagval(optype,opvalue)

        Goto 100


C HERE After Succesful SUBTREE analysis, or if Node is simple (valu or VARI)

 200       continue                             !jump here if returning from subtree analysis
           if (nerr.gt.0) return                !check for error

           call calc_popd(lsstat,newnode,wasnode,nodect)

           IF (NewNode.eq.Nil) Then             !equation displayed
             if (qmatrix) then
                  call calc_put_out(inum,'  ```} right\}  ',fstring,0)
             endif
             call down_line(0)
             call del_to_end
             RETURN                             !so Return
           endif

           IF (LsStat.eq.LsNotDone) then   !returning from Left node (of oper)


c count operators
             boldit=0
             if (parenct.ge.0) then  !  used if qmatrix = true
                  if (.not.qmatrix2) then
                      inthis_set=inthis_Set+1     !# of -+in this line
                  elseif (parenct.eq.0) then
                       inthis_set=max(1,inthis_set)+max_set2
                        max_set2=0
                  endif
                 if (qmatrix.and.parenct.eq.0.and.inthis_set.gt.iset_8)
     1                    then
                     boldit=1
                  endif
             endif
             if (parenct.ge.1) then         !if qmatrix2=true
                  inset_2=inset_2+1     !# of -+in this line
                  max_set2=max(max_Set2,inset_2)
                  if (qmatrix2.and.parenct.eq.1.and.inset_2.gt.3)
     1                    then
                     boldit=1
                  endif
             endif

              Call Get_nodeInfo(Newnode,OPtype,OPvalue,InLS,InRS)
              flagopv=set_flagval(optype,opvalue)

              call CALC_oper_cvt(charans,flagopv)       !display the math operator
              if (charans.eq.'/') then                      !/
                      call calc_put_out(inum,'} OVER {',fstring,0)

c Break matrix line at - or + only.
c A nice addition would be to break at * when a long list of *
c WITH no - or + in it, appears. Perhaps later

              elseif (charans.eq.'-'.or.charans.eq.'+') then  !+ -

                  tmpchar=' `` '
                  if (qmatrix) tmpchar=' ~ '
                  call calc_put_out(inum,tmpchar(1:3),fstring,0)
                  if (boldit.eq.1) then       ! next stuff on next line,
                      call calc_put_out(inum,' ~~ BOLD ',fstring,0)
                 endif
                  call calc_put_out(inum,charans,fstring,1)
                  call calc_put_out(inum,tmpchar(1:3),fstring,0)

c both might be satisified, but only 1 will be acted on (since
c only one of the inthis_set variables will have been incremented.
c note we break a matrix ONLY at + or -, we allow other operators
c (at appropriatre level) to count towards the 3 operator line row width

                  if (qmatrix.and.parenct.eq.0) then  !
                      if (inthis_set.gt.iset_8) then       !new line
                          call calc_put_out(inum,'  #  `` ',fstring,0)
                          inthis_set=0
                      endif
                  endif
                  if (qmatrix2.and.parenct.eq.1) then
                      if (inset_2.gt.3) then       !new line
                          call calc_put_out(inum,' `#` ',fstring,0)
                          inset_2=0
                      endif
                  endif

              elseif (charans.eq.'^') then
                      call calc_put_out(inum,'``} ^ {``',fstring,0)
              else                                          ! * %
                       call calc_put_out(inum,charans,fstring,1)
              endif

              Call calc_Pushd(LsDone,NewNode,wasnode,nodect)  !ready to get RS

               wasnode=newnode
               CurrentNode=inrs

              Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
              flagopv=set_flagval(optype,opvalue)

              goto 100

           else                                 !returning to Right Node

              itmp=do_suppress(newnode)
              if (LsStat.eq.LsDone) then                !Returning to MATH OPER
                  if (itmp.ne.0) then
                     call calc_PUT_OUT(Inum,'}`` ',fstring,0) !end of
                  endif

              elseif (lsstat.eq.lsfunk) then   !perm or user functions
                  if (itmp.eq.2) then
                     call calc_PUT_OUT(Inum,'} ',fstring,0) !end of
                  elseif (itmp.eq.3) then
                     call calc_PUT_OUT(Inum,' ``right | `` ',fstring,0) !end of
                  endif


              else              !LsStat must be LsNone: )
                  parenct=parenct-1

                 if (parenct.eq.0.and.qmatrix2) then     !    end of matrix
                    call calc_put_out(inum,' `}` right\}`` ',fstring,0)
                 endif

                  itmp=do_suppress(wasnode)
                  if (itmp.eq.0) then

                    if (parenct.eq.0
     1                .and. (is_notregfunc(wasnode)) ) then
                        if (nodect.eq.1) then
                            charans='``right ~~]'    ! might change this later
                        else
                            charans=' ``right ] '
                        endif
                     else
                        charans=' ``RIGHT ) '
                      endif
                      call calc_PUT_OUT(Inum,
     1                    charans(1:(len_trim(charans)+1)),fstring,0) !end of
                   endif
              endif

              goto 200                  !LS or RS subtree value for higher node

           endif                       !Return to LS or RS

        print *,' bad spot in tree_string '
        RETURN
        End



c==================
c is this a paren attached to a function
        logical function is_notregfunc(anode)
        implicit none

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        integer anode,anode0
        logical is_func

        anode0=iabs(anode)
        if (anode0.eq.nil) then
           is_notregfunc=.true.
        else
            is_notregfunc=.not.is_func(anode0)
        endif
        return
        end



c==================================
c if wasnode is / or exp, or wasnode is Sqrt

        integer function do_suppress(anode0)
        implicit none

        integer anode,optype,inls,inrs,anode0
        real set_flagval,flagval
        real *8 opvalue
        logical is_math2,is_func,is_neg,qleft

        character *12 aname,get_funkname

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer nil                     !needed by calc_routines
        common /calc_cmnil/nil

        qleft=.false.
        if (anode0.lt.0) qleft=.true.
        anode=iabs(anode0)

        do_suppress=0             !assum no suppression
c        call print_node(anode,' wasnode ',6)

        if (anode.eq.nil) return

        if (is_math2(anode,div)) then
           if (.not.is_neg(anode)) then
                do_suppress=1
           endif
           return
        elseif  (is_math2(anode,exp)) then
c note: if its (a) of (a)^b , to NOT suppress, but if its (B) of (A)^(B), do
           if ((.not.is_neg(anode)) .and. (.not.qleft)) then
              do_suppress=1
           endif
           return
        endif

c else, check for sqrt and ans

        if (is_func(anode)) then
           Call GET_NodeINFO(aNODE,OPTYPE,OPVALUE,INLS,INRS)
           flagval=set_flagval(optype,opvalue)
           aname=get_funkname(ABS(flagval))
           if (aname(1:4).eq.'SQRT') then
                do_suppress=2
           elseif(aname(1:3).eq.'ABS') then
                do_suppress=3
           endif
        endif

        return
        end





c-----------------------------
c if wp51, convert ' to \'
        character *(*) function expand_apost(charans,iflag)
        implicit none
        character *(*) charans
        integer iflag,ilen,in,ij,jlen

c char(39) = '   !add a ' to name
c char(92)=\

        ilen=len(charans)
        jlen=len(expand_apost)

        if (iflag.ne.1.or.ilen.eq.0) then          !don't
             expand_apost=charans
             return
        endif
        expand_apost=' '
c       print *, 'expanding ',charans
        expand_apost(1:1)=charans(1:1)
        in=2
        do ij=2,ilen
           if (charans(ij:Ij).eq.char(39)) then    !expand '
             if (in.lt.jlen) then
                expand_apost(in:in)='\'
                in=in+1
             endif
             expand_apost(in:in)=char(39)
          else                                  !not a '
              expand_apost(in:in)=charans(ij:ij)
          endif
          in=in+1
          if (in.gt.ilen) exit
        enddo

        return
        end



C========================================================
C
C Fill a string. Inum is where to put string. Chara is substring.
c Fstring is string. Inum will be returned as old inum+messlen(substring)
c  Hence, inum is returned as next free space.

        SUBROUTINE calc_PUT_OUT(Inum,CHARA,fstring,trimit)

        CHARACTER *(*)CHARA,fstring
        integer trimit

        il=len(fstring)
        if (inum.gt.il) return          !cant fit any of substring

        if (trimit.ne.0) then
            IC=MESSLEN(CHARA)
        else
            ic=len(chara)
        endif
        IF (IC.EQ.0) IC=1       !A SPACE

        if (inum+ic.gt.il) ic=il-inum   !cant fit all of substring


        fstring(inum:inum+ic-1)=chara(1:IC)

        inum=inum+ic            !return next free space

        RETURN
        END



c=============================================================================

C=================================================
C DERIVATIVE ROUTINES
C=================================================
c
c 12/28/84 DMH
C=================================================
C
c Compute a derivative from a user defined function.
c both F FPRIMEVAR and FPRIME are supplied. The deriv function
c will be names FPrime.
c FPRIMEVAR is simply the name used for "user arguments".
C
C
C  Sample usage::
c       Define some function "F", using calc_Loadfunction
c       To get the derivative of F:
c           Call CALC_MakeDeriv(F,Fprime,Fprimevar,nerr)
c
c               F== Character variable containing NAME of function to get
c                       get derivative of
c               Fprime  == Character variable containing name to be
c                       GIVEN to the derivative.  In other words,
c                       FPRIME is a totally new function.  Therefore,
c                       one uses FPRIME just as one uses any other
c                       "user defined" function.
c               Fprimevar == Character variable for name of "argument".
c                       This name is used when function is displayed.
c               NERR  == Error flag. If value.

        Subroutine CALC_makederiv(AF,AFprime,Afprimevar,nerr2)

        implicit none

        integer npermfunk
        paramEter (NPERMFUNK=25)

        INTEGER CALC_DERIV
        REAL CALC_GET_FUNKADDRESS

        CHARACTER *(*) AF,AFPRIME,AFPRIMEVAR
        character *12 f,fprime,fprimevar
        integer nnode,Nfnode,nerr2,atmp,copy_tree

        INTEGER NERR
        common /calc_cmerror/nerr

        integer mvari
        real fnum,pnum

        nerr=0

        F=AF
        CALL CALC_CLEANUP_STRuNG(F,0)

        FPRIME=AFPRIME
        CALL CALC_CLEANUP_STRuNG(FPRIME,0)

        FPRIMEVAR=AFPRIMEVAR
        CALL CALC_CLEANUP_STRuNG(FPRIMEVAR,0)

        nerr2=1                 !assume an error

         call find_func(Fprime,pnum)
         call find_func(f,fnum)

         if (pnum.lE.npermfunk.AND.PNUM.GT.0) then     ! 0 SIGNIFIES NEW FUNK
           call Calc_Error(
     1       ' ! Attempt to redefine permanent function')
           return
         elseif (fnum.eq.0) then
             call Calc_Error(' No such function')
             return
         elseif (Fnum.le.npermfunk) then
             call calc_error(' Not A USER function ')
             return
         endif


        nnode=calc_get_funkaddress(FNUM)

        IF (NERR.GT.0) then
           call calc_error(' Undefined user function:'//f)
           return
        endif

        atmp=copy_tree(nnode)    !copy the tree first.

        Nfnode=calc_deriv(atmp)

c       print *,atmp,nfnode

        IF (NERR.GT.0) then
           return
        endif

        mvari=Pnum
           if (mvari.eq.0) then         !first time for this name
              call set_funk(Fprime,Nfnode,fprimevar)
           else                         !function name being replaced
              call fill_funk(mvari,fprime,fprimevar,Nfnode)
           endif

        nerr2=0

        return
        end


c------------------------------------------
c----------------------------------------------------
c
c In contrast to other modules of the CALC package, which "quasi-mimiced"
c recursive techniques, This module will seek to "Truly-mimic" recursive
c techniques.  The following is the recursive "program".
c
c
c Routine DEVIV(ANode)
c
C       IF ANODE IS value type then or vari type not equal to "function variable"
c               return a value node of 0
c       If anode is function variable,
c               return a value  node of 1
C
c       If Anode is IOPR type then
c               create IOPR node with RS --> to DERIV(rs)       !rs==right son
c
c       elseif ANode is Permanent function than !Dy/DX * D(f(y))/DY
c               Create node with:
c               Opr=Mult
c               Rs=Deriv(RS)
c               Ls=FunkDeriv(Funktype,RS)  !eg: D(sin(y))/Dy= Cos(y)
c
c      elseif Anode is OPER (math type) node:  !several possibilities
c             1) + or -
c               Create node with :
c                       Opr = Plus or minus
c                       Ls  = DERIV(LS)
c                       Rs  = DERIV(RS)
c            2) *
c               Create node with:
c                      Opr = Plus
c                        Rs = node with:
c                               Opr=*
c                               Rs=DERIV(RS)
c                               Ls=Ls
c                       Ls  = node with:
c                               Opr=*
c                               Rs=Rs
c                               Ls=DERIV(LS)
c           3) / or %   -- note that mod operation treated like division
c               Create node with:
c                    Opr = /
c                    LS = Node with:
c                          Opr = -
c                          LS = Node with:
c                             Opr = *
c                             LS = DERIV(LS)
c                             RS = Rs
c                          RS = Node with:
c                             Opr = *
c                             Ls = LS
c                             Rs = DERIV(RS)
c                   RS = Node with:
c                       Opr = *
c                       Rs = RS
c                       Ls = RS
c          4) ** (RS NOT a value or constant) (the general case)
c               Node:
c                 Opr = *
c                 Ls = node:
c                    Opr= +
c                    LS = Node:
c                          OPr = *
c                          Ls = DERIV(RS)
c                          Rs  = Node:
c                               Opr == LN function
c                               Rs =  Ls
c                               Ls = nil
c                    RS =  Node:
c                          OPr = *
c                          Ls = node:
c                              Opr=/
c                              Ls = RS
c                              Rs = Ls
c                          Rs  = DERIV(LS)
c                 RS  = Node:
c                       Opr = **
c                       Ls = Ls
c                       Rs = Rs
c       4a) **, with RS = value or constant
c          Node:
c           Opr= *
c           Ls = Deriv(LS)
c           RS = Node:
c               Opr = *
c               Ls = RS
c               Rs = Node:
c                  Opr = **
c                  Ls = Ls
c                  Rs = Node:
c                       Opr = -
c                       Ls = Rs
c                       Rs = 1
c
c  Currently: no support of deriv of user defined functions INSIDE of function.
c  Also, only take derivative of "argument", not of a constant.
c
c===========================================================================

c Implementation note:: due to necessity of skipping around (given value of
c  "PC", I must use more gotos than i would like (can not jump into the middle
c  of an IF...THEN ... ELSE structure.)

        integer function calc_DERIV(BaseNode)

        implicit none

        integer npermfunk
        paramEter (NPERMFUNK=25)

        integer basenode
        integer pc

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer nil

        integer currentnode,currentLS,currentRS,currentOPR,priornode
        integer calc_mathnode,tmpnode,calc_funkderiv,cd_oprn2
        integer make_lognode

        integer t3,t2,t1
        integer dls,drs,lnls

        logical is_math

        integer nerr
        COMMON  /calc_cmerror/nerr

c GET_NodeINFO(NodeAddress,OPTYPE,OPVALUE,INLS,INRS)
        integer optype
        real *8 opvalue
        real flagopv,set_flagval

        integer zeronode,onenode,twonode,tennode,negonenode
        common /globalnodes/zeronode,onenode,twonode,tennode,negonenode

        integer found_modop

        data nil/0/

        found_modop=0


        CALL CALC_GARBAGEMAN(0)         !make sure there is enough "memory"


        currentnode=basenode
        call calc_pushD(nil,nil,nil,nil)                !flag base of tree

c   Entry "DERIV(Anode)

 10     continue

        if (nerr.gt.0) return                   !error somewhere

        call get_nodeinfo(
     1        currentnode,optype,opvalue,CurrentLs,CurrentRs)
        flagopv=set_flagval(optype,opvalue)

c  "IF ANODE IS VALU OR NON-FUNCTION-ARG VARIABLE"
        if (.not.(optype.eq.valu.or.
     1       (optype.eq.vari.and.ABS(FLAGOPV).ne.1.)))
     1                  goto 8002

c    "RETURN A VALUE NODE OF 0
        currentnode=zeronode
        goto 5000

 8002   continue
        if (.not.(optype.eq.vari)) goto 8004
C          "ELSE RETURN A VALUE NODE OF 1 or -1 "
        if (flagopv.gt.0) then
            currentnode=onenode
        else
            currentnode=negonenode
        endif
        goto 5000

c "IF ANODE IS IOPR" THEN
 8004   continue
        If (.not.(Optype.eq.Oper.and.abs(flagopv).eq.Iopr)) goto 8010

c "CREATE IOPR NODE WITH RS=DERIV(RS)
        call calc_pushd(int(flagopv),nil,nil,110)       !ready for recursion
        currentnode=currentrs
        goto 10                                         !and do it

 110    continue                                !jump here after done with call
        currentnode=calc_mathnode(currentopr,nil,priornode)

        goto 5000                               !and "return from this entry"

c  "elseif NODE IS PERMANENT FUNCTION THAN"
 8010   continue
        If (.not.(optype.eq.funk)) goto 8020

c Superseded. Assume a function AB has deriv AB' (which can
c defined later if necessary
c        if (ABS(FLAGOPV).gt.npermfunk) then     !only permanent functions
c           call calc_error(                     !allowed
c    1  ' ! Deriv wont work if a User Defined function is referenced.')
c           return
c        endif

c "CREATE NODE WITH"

c    "IOPR = MULT"
        call calc_pushd(int(flagopv),nil,Currentrs,210) !ftype in "currentopr"

C    "RS = DERIV(RS)"
        currentnode=currentrs                           !in CurrentLs position on stack
        goto 10

 210    continue
c    "LS = FUNKDERIV(FunkType,RS)"
        tmpnode=Calc_funkDeriv(Currentopr,Currentrs)   !currentopr holds func identtifier
                                                !prior node holds "y" in "F(Y)"
        if (nerr.gt.0) return
        currentnode=calc_mathnode(mult,tmpnode,priornode)
        !DF(y)    Dy
        !----  * ----
        ! Dy      Dx

        goto 5000

c "elseif ANODE IS A MATH OPERATOR"
 8020   continue                        !by default, must be a math operator.

c  " 0) If Mod operator, error!
        if (flagopv.eq.modop) then
         call calc_error(' ! Can not take derivative of Mod operator')
           return
        endif

c   "1) + OR - "
        if (.not.(flagopv.eq.add.or.flagopv.eq.minus)) goto 8030

c        "NODE:"
c             "OPR = + OR - "
        call calc_pushd(int(flagopv),nil,currentrs,310)
c            "LS= DERIV(LS)"
        currentnode=currentls
        goto 10

 310    continue
        call calc_pushd(currentopr,priornode,nil,320)
c            "RS=DERIV(RS)"
        currentnode=currentrs
        goto 10

 320    continue

        CURRENTNODE=calc_mathnode(CURRENTOPR,currentls,priornode)
        GOTO 5000

 8030   continue
c  "2) * "
        if (.not.(flagopv.eq.mult)) goto 8040

c     "NODE:"
C         "OPR=+"
C         "LS= NODE:"
C            "OPR=*"
C            "LS=LS"
C            "RS=DERIV(RS)"
        call calc_pushd(add,currentls,currentrs,420)

        call calc_Pushd(mult,currentls,nil,410)
        currentnode=currentrs
        goto 10

 410    continue


        currentnode=calc_mathnode(mult,currentls,priornode)   !     db
                                                              !a *  --
        goto 5000                                             !     dx

 420    continue
        call calc_pushd(add,priornode,nil,440)

c           "RS=NODE:"
c             "OPR=MULT"
c             "LS=DERIV(LS)"
c             "RS=RS"
        call calc_pushd(mult,nil,currentrs,430)
        currentnode=currentls
        goto 10

 430    continue

        currentnode=calc_mathnode(mult,priornode,currentrs)   !da
                                                              !--   *   b
        goto 5000                                             !dx

 440    continue
        currentnode=calc_mathnode(add,currentls,priornode)  !a*db/dx + da/dx*b

        goto 5000


 8040   continue
c   " 3) /   "
        if (.not.(flagopv.eq.div)) goto 8050

c       "NODE:"
c           "OPR=/"
        call calc_pushd(div,currentls,currentrs,570)
c           "LS = NODE: "
c               "OPR = -"
        call calc_pushd(minus,currentls,currentrs,540)
c               "LS=NODE:"
c                  "OPR=*"
c                  "LS=DERIV(LS)"
c                  "RS=RS"
        call calc_pushd(mult,nil,currentrs,520)
        currentnode=currentls
        goto 10
 520    continue
        currentnode=calc_mathnode(mult,priornode,currentrs)
        goto 5000

 540    continue
c              "RS=NODE:"
c                   "OPR=*"
c                   "LS=LS"
c                   "RS=DERIV(RS)"
        call calc_pushd(minus,priornode,nil,550)
        call calc_pushd(mult,currentls,nil,544)
        currentnode=currentrs
        goto 10

 544    continue
        currentnode=calc_mathnode(mult,currentls,priornode)
        goto 5000
  550   continue
        currentnode=calc_mathnode(minus,currentls,priornode)
        goto 5000

 570    continue
c           "RS=NODE"
c               "OPR=EXP"
c               "LS=RS"
c               "RS=2"
        call calc_pushd(div,priornode,nil,580)
        if (is_Math(currentrs)) then
            currentnode=calc_mathnode(exp,cd_oprn2(currentrs),twonODE)
        else
            currentnode=calc_mathnode(exp,currentrs,twonODE)
        endif
        goto 5000

 580    continue
        currentnode=calc_mathnode(div,currentls,priornode)
        goto 5000

 8050   continue
C       "4) ** "
        if (.not.(flagopv.eq.exp)) goto 8060

C         "4a)RS IS VALU OR VARI" (BUT NOT IF VARI IS "X")
        call  get_nodeinfo(currentrs,optype,opvalue,t1,t2)
        IF (.NOT.(OPTYPE.EQ.VALU.OR.
     1       (OPTYPE.EQ.VARI.AND.OPVALUE.NE.1)))
     1                  GOTO 8055

c            "NODE:"
c              "OPR= * "
c              "LS = DERIV(LS)
        call calc_pushd(mult,currentls,currentrs,812)
        currentnode=currentls
        goto 10

 812    continue
c              "RS = NODE: "            ;note: no recursive call in this section
c                 "OPR=*"               ; We use several temporary variables
c                 "LS = RS"
c                 "RS =NODE:"
c                     "OPR = ** "
c                     "LS = LS"
c                     "RS = NODE"
c                       "OPR= - "
c                       "LS = RS "
c                       "RS = 1"
        t3=calc_mathnode(minus,currentrs,onenode)          !Rs-1
        t2=calc_mathnode(exp,currentls,t3)                 !ls**(rs-1)
        t1=calc_mathnode(mult,currentrs,t2)                !rs*[ls**(rs-1)]

        currentnode=calc_mathnode(mult,priornode,t1)    !dls/dx*[rs*[ls**(rs-1)]]
        goto 5000

 8055   continue                !To make life easy, I will get DERIV(LS)
c and DERIV(RS), and build subtree with this. Hence, not strictly recursive.

c       "4) ** , GENERAL CASE"
C       "
        call calc_pushd(exp,currentls,currentrs,940)
        call calc_pushd(1,currentls,currentrs,910)
        currentnode=currentls
        goto 10
 910    continue
        call calc_pushd(2,priornode,nil,920) !priornode=dls/dx
        currentnode=currentrs
        goto 10
 920    continue                        !currentls=dls/dx, priornode=drs/dx
        dls=currentls
        drs=priornode
        goto 5000
 940    continue                        !currentrs=RS,currentls=LS
        priornode=calc_mathnode(exp,currentls,currentrs)  !LS**RS
        lnls=make_lognode(currentls)    !node which is LN(LS)

c we now have enought parts to allow straight forward tree construction.
        t1=calc_mathnode(mult,lnls,drs) !drs/dx * ln(ls)
          t2=calc_mathnode(div,currentrs,currentls)  !rs/ls
          t3=calc_mathnode(mult,t2,dls)         !rs/ls * dls/dx
        tmpnode=calc_mathnode(add,t1,t3)   !drs/dx * ln(ls) + rs/ls * dls/dx

        currentnode=calc_mathnode(mult,tmpnode,priornode) !above * (a**b)
        goto 5000

 8060   continue

        call calc_error(' ! Unrecognized operator in Deriv routine ')
        return

C
c . . . . ..
c          Jump here to "return"  (to some "pc" location)
 5000   continue

        priornode=currentnode
        call calc_popd(currentopr,currentls,currentrs,pc)

        if (currentopr.eq.nil) then                     !DONE !!
         call get_nodeinfo(priornode,
     1                     optype,opvalue,CurrentLs,CurrentRs)
             IF (OPTYPE.EQ.OPER.AND.DABS(OPVALUE).EQ.IOPR) THEN
                  calc_Deriv=currentrs
               ELSE
                    calc_DERIV=PRIORNODE
              endif
            return
        endif

c else, continue recursion.

        if (pc.eq.110)goto 110
        if (pc.eq.210)goto 210
        if (pc.eq.310)goto 310
        if (pc.eq.320)goto 320
        if (pc.eq.410)goto 410
        if (pc.eq.420) goto 420
        if (Pc.eq.430)goto 430
        if (Pc.eq.440)goto 440
        if (pc.eq.570)goto 570
        if (pc.eq.540) goto 540
        if (Pc.eq.520)goto 520
        if (pc.eq.550)goto 550
        if (pc.eq.544)goto 544
        if (pc.eq.580)goto 580
        if (pc.eq.812)goto 812
        if (pc.eq.910)goto 910
        if (pc.eq.920)goto 920
        if (pc.eq.940)goto 940

        write(6,1234)pc
 1234   format (' unknown pc =',i3)
        stop

        end


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

        PARAMETER (maxnode=6000)

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

C          if (optypetmp.eq.oper.and.dabs(opvaltmp).eq.iopr) then
C             t2=t5
C             goto 12

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


c=========================
c calc_mathnode with () stripped
        integer function calc_mathnode2(anoper,i1,i2)
        implicit none

        integer anoper,i1,i2,get_right,calc_mathnode,itmp

        logical is_iopr

        itmp=calc_mathnode(anoper,i1,i2)
        if (is_iopr(itmp)) itmp=get_right(itmp)
        calc_mathnode2=itmp
        return
        end



c=========================
c  call calc_mathnode with rs a real *4 value

        integer function calc_mathnode4(anoper,ln,v4)

        implicit none

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI

        integer anoper,ln,n0,itmp,make_valnode,calc_mathnode2

        real v4
        real *8 v8

        v8=v4
        n0=make_valnode(v8,valu)
        itmp=calc_mathnode2(anoper,ln,n0)
        calc_mathnode4=itmp

        return
        end





C=================================================
C  Derivatives of trigonometric,etc functions.

        Integer function Calc_funkDeriv(Ftype,Y)

        implicit none

        INTEGER MAXFUNK,NPERMFUNK,NTMPFUNK

        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)


        integer ftype,y
        integer iftype,neg

c we want dF(y)/dy:: eg d(sin(y)) /dy = cos(y)

C       'SIN','COS',  'TAN','SINR','COSR',  'TANR',
C      'ARCSINR','ARCCOSR','ARCTANR','ARCSINR','ARCCOSR','ARCTANR',
C      'ABS','EXP','EXP10','LOG','LOG10',
C                      'INT','SQRT','RADTODEG','DEGTORAD',
c       sinh,cosh,tanh,factorial
c

        INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
        common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI


        INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
        common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP

        integer make_oprnode,make_funknode,make_user_funknode
        integer calc_buildmnode,cd_Oprn2,yy

        integer tmpnode,tmpnode2,tmp

        integer zero,one,two,ten,negone
        common /globalnodes/zero,one,two,ten,negone


        iftype=iabs(ftype)
        neg=sign(1.,float(ftype))

        if (iftype.eq.13.or.
     1       iftype.eq.18.or.iftype.eq.20.or.iftype.eq.21) then
            call calc_error(
     1     ' ! Derivative undefined: ABS,INT,DEGTORAD, or RADTODEG')
            return

        elseif (iftype.eq.25) then
            call calc_error(' ! Derivative undefined: FACT ')
            return

        elseif (iftype.eq.1.or.iftype.eq.4) then               !dsin=dcos
            tmpnode=make_funknode(float(iftype)+1.)
            call right_assign(tmpnode,y)

        elseif (iftype.eq.2 .or. iftype.eq.5) then     !dcos=-sin
            tmpnode=make_funknode(float(iftype)-1.)
            call right_assign(tmpnode,y)
            neg=-neg

        elseif (iftype.eq.3.or.iftype.eq.6) then               !dtan=1/cos**2
            tmpnode=make_funknode(float(iftype)-1.)             !cos
            call right_assign(tmpnode,y)
            tmpnode2=calc_buildmnode(exp,tmpnode,two)  !cos**2
            tmpnode=calc_buildmnode(div,one,tmpnode2)  !1/cos**2


        elseif(iftype.eq.7) then      !darcsin=1/sqrt[1-x**2]
            yy=make_funknode(21.)       !yy=degtorad(y)
            call right_assign(yy,y)
            tmpnode2=calc_buildmnode(exp,yy,two)        !y**2
            tmpnode2=calc_buildmnode(minus,one,tmpnode2)    !1-y**2
            tmpnode=make_funknode(19.)
            call right_assign(tmpnode,TMPNODE2) !sqrt(1-y**2)
            tmpnode=calc_buildmnode(div,one,cd_oprn2(tmpnode)) !1/[sqrt(1-y**2)]

        elseif (iftype.eq.8) then      !darccos=-1/sqrt(1-x**2)
            yy=make_funknode(21.)       !yy=degtorad(y)
            call right_assign(yy,y)
            tmpnode2=calc_buildmnode(exp,yy,two)        !y**2
            tmpnode2=calc_buildmnode(minus,one,tmpnode2)    !1-y**2
            tmpnode=make_funknode(19.)
            call right_assign(tmpnode,cd_oprn2(tmpnode2))               !sqrt(1-y**2)
            tmpnode=calc_buildmnode(div,negone,cd_oprn2(tmpnode)) !-1/[sqrt(1-y**2)]

        elseif (iftype.eq.9) then      !darctan=1/1+x**2
            yy=make_funknode(21.)       !yy=degtorad(y)
            call right_assign(yy,y)
            tmpnode2=calc_buildmnode(exp,yy,two)        !y**2
            tmpnode2=calc_buildmnode(add,one,tmpnode2)    !1+y**2
            tmpnode=calc_buildmnode(div,one,cd_oprn2(tmpnode2)) !1/[(1+y**2)]

        elseif(iftype.eq.10) then      !darcsin=1/sqrt[1-x**2]
            tmpnode2=calc_buildmnode(exp,y,two) !y**2
            tmpnode2=calc_buildmnode(minus,one,tmpnode2)    !1-y**2
            tmpnode=make_funknode(19.)
            call right_assign(tmpnode,cd_oprn2(tmpnode2))       !sqrt(1-y**2)
            tmpnode=calc_buildmnode(div,one,cd_oprn2(tmpnode)) !1/[sqrt(1-y**2)]

        elseif (iftype.eq.11) then     !darccos=-1/sqrt(1-x**2)
            tmpnode2=calc_buildmnode(exp,y,two) !y**2
            tmpnode2=calc_buildmnode(minus,one,tmpnode2)    !1-y**2
            tmpnode=make_funknode(19.)
            call right_assign(tmpnode,cd_oprn2(tmpnode2))               !sqrt(1-y**2)
            tmpnode=calc_buildmnode(div,negone,cd_oprn2(tmpnode)) !-1/[sqrt(1-y**2)

        elseif (iftype.eq.12) then     !darctan=1/1+x**2
            tmpnode2=calc_buildmnode(exp,y,two) !y**2
            tmpnode2=calc_buildmnode(add,one,tmpnode2)    !1+y**2
            tmpnode=calc_buildmnode(div,one,cd_oprn2(tmpnode2)) !1/(1+y**2)


        elseif (Iftype.eq.14) then                     !de**x=e**x
           tmpnode=make_funknode(14.)
           call right_assign(tmpnode,y)

        elseif (iftype.eq.15) then                     !d10**x=10**x * ln(10)
           tmpnode=make_funknode(15.)
           call right_assign(tmpnode,y)                 !exp10(y)
           tmpnode2=make_funknode(16.)
           call right_assign(tmpnode2,cd_oprn2(ten))            !ln(10)
           tmpnode=calc_buildmnode(mult,tmpnode,tmpnode2)     !exp10(y)*ln(10)

        elseif (iftype.eq.16)then              !dln=1/x
            tmpnode=calc_buildmnode(div,one,y)

        elseif (iftype.eq.17) then             !dlog10=1/(x*ln10)
           tmpnode2=make_funknode(16.)
           call right_assign(tmpnode2,cd_oprn2(ten))            !ln(10)
           tmpnode=cd_oprn2(calc_buildmnode(mult,y,tmpnode2))   ![x*ln(10)]
           tmpnode=calc_buildmnode(div,one,tmpnode)     !1/[x*ln(10)]

        elseif (Iftype.eq.19) then             !dsqrt=1/[2*sqrt(y)]
           tmpnode=make_funknode(19.)
           call right_assign(tmpnode,y)         !sqrt(y)
           tmpnode2=CD_OPRN2(calc_buildmnode(mult,two,tmpnode))   ![2*sqrt(y)]
           tmpnode=calc_buildmnode(div,one,tmpnode2)    !1/[2*sqrt(y)]


        elseif (iftype.eq.22) then             !dsinh=cosh
            tmpnode=make_funknode(23.)
            call right_assign(tmpnode,y)

        elseif (iftype.eq.23) then             !dcosh=sinh
            tmpnode=make_funknode(22.)
            call right_assign(tmpnode,y)

        elseif (iftype.eq.24) then             !dtanh=1/cosh**2
           tmpnode2=make_funknode(23.)
           call right_assign(tmpnode2,y)
           tmpnode=calc_buildmnode(exp,tmpnode2,two)
           tmpnode=calc_buildmnode(div,one,tmpnode)

        elseif (iftype.gt.npermfunk) then       !a non permanent function
           tmpnode=make_user_funknode(iftype)
           if (tmpnode.eq.0) then
                call calc_error(' ! Referencing undefined derivative')
                return
           endif
           call right_assign(tmpnode,y)

        endif

        tmp=make_oprnode(float(iopr*neg))
        call right_assign(tmp,tmpnode)
        calc_funkderiv=tmp

        return
        end


c================================
c return pointer to F', if iftype points to F, and if F' exists.  Else, 0

      integer function make_user_funknode(iftype)

        IMPLICIT NONE
        INTEGER MAXFUNK,NPERMFUNK,NTMPFUNK

        PARAMETER (MAXFUNK=59)
        parameter(NPERMFUNK=25)
        parameter(ntmpfunk=maxfunk-npermfunk)

        integer funkaddress(ntmpfunk),nfunk
        character *12 funkargs(ntmpfunk)
        CHARACTER *12 FUNKNAMES(MAXFUNK)
        common /calc_FUNCTIONS/NFUNK,FUNKNAMES,funkaddress,funkargs

        integer iftype,ii,iat,make_funknode
        real aval
        character *12 aname,aname2

         make_user_funknode=0           !assume failure

        if (iftype.le.npermfunk) then
           return
        endif

        iat=funkaddress(iftype-npermfunk)     !see if its been deleted
        if (iat.le.0) then
           return
        endif

c get the name of this function
        aname=funknames(iftype)
        ii=len_trim(aname)
        if (ii.le.0) return

        aname2=aname(1:min(11,ii))//char(39)   !add a ' to name
        call  FIND_FUNC(aname2,AVAL)     !AVAL POINTS TO FUNCTION

        if (aval.eq.0) then
            make_user_funknode=0
        else
            make_user_funknode=make_funknode(aval)
        endif

        return
        end

c
c       Subroutine ree_string(fstring,base,ndec,qe,iwp51)
c
c BASIC DATA STRUCTURE
c
c       PARAMETER (maxnode=6000)
c       PARAMETER (maxstak2=1750)
c       parameter(NPERMFUNK=25)
c
c       logical qe
c       character *(*) fstring
c
c       integer *2 node(MAXNODE,2)              !4 POINTERS:
c       INTEGER LS,RS
c       INTEGER NIL                     !NIL POINTER
c       common /calc_NODES/NODE,LS,RS,NIL
c
c       REAL *8 VALUE(MAXNODE)
c       integer *2 opr(MAXNODE)
c       common /calc_NODEVALUE/VALUE,OPR
c
c       INTEGER EQEND,VALU,OPER,PAREN,FUNK,VARI
c       common /calc_OPRS/EQEND,VALU,OPER,PAREN,FUNK,VARI
c
c       INTEGER IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
c       common /calc_OPTYPE/ IOPR,ADD,MINUS,MULT,DIV,EXP,MODOP
c
c       INTEGER BP,EP
c       common /calc_PTYPE/BP,EP
c
c       INTEGER *2 STKNODE(MAXSTAK2)
c       integer *2 LsStats(MAXSTAK2)
c       REAL *8 VALL(MAXSTAK2)          !holds value
c       REAL VALR(MAXSTAK2)             !could be integer *2
c       INTEGER NSTACK2
c       common /calc_STACK2/NSTACK2,VALL,LsStats,STKNODE,VALR
c
c       common /calc_cmls/LsNotdone,LsDone,LsNone,Lsfunk
c
c       real *8 vls,holdvalue
c       real *8 opvalue,nil8
c       integer suppress_paren
c
c       character *12 get_variname,get_funkname
c       character *30 charans,expand_apost
c
c       common /calc_cmerror/nerr
c
c       integer ls_fromdiv
c       Integer Base                    !the base of tree to be analyzed
c       Integer Optype,CurrentNode,NewNode
c
c
c       Data LsNotdone,LsDone,LsNone,Lsfunk/10,20,30,40/
c
c
c       inum=1          !initialize string
c       fstring=' '
c
c  calc_Push a nil to act as a brake
c       nil8=nil
c       Call calc_PushS(nil8,LsNone,nil,float(nil))
c
c Get First Node.
c
c       CurrentNode=Base
c
c       Call GET_NodeINFO(CURRENTNODE,OPTYPE,OPVALUE,INLS,INRS)
c       flagopv=set_flagval(optype,opvalue)
c
c
c100    Continue
c       If (nerr.gt.0) return                   !check for error
c
c Case 1 :: IOPR or FUNK.
c   Functions and () are treated similar, except function name is
c   displayed.
c   Regardless of type, we still need to display the subtree that will
c   that is the argument to this function or ().
c   NOTE:: User and perm functions are completely undifferentiated in
c   this routine.
c
c       IF (optype.eq.funk.or.(optype.eq.OPER.and.
c    1          abs(flagopv).eq.IOPR)) then   !use abs (watch out for neg functions)
c           Vls=0                       !signal used by user functions
c           Vrs=Inrs                    !Address of function or IOPR argument
c
c           if (optype.eq.funk) then
c              ll=lsfunk
c           else
c              ll=lsnone
c           endif
c           Call calc_PushS(Vls,Ll,CurrentNode,Vrs)
c
c now display Funk(  or  (
c
c           if (iwp51.ne.1 .and. flagopv.lt.0)  then
c                 call calc_put_out(Inum,'-',fstring,1)
c           endif
c
c           if (optype.eq.funk) then
c              charans=get_funkname(ABS(flagopv))
c              charans=expand_apost(charans,iwp51)
c              if (iwp51.eq.1) then
c                  call calc_put_out(Inum,' func { ',fstring,0)
c                  if (flagopv.lt.0)   then
c                       call calc_put_out(Inum,'-',fstring,1)
c                  endif
c              endif
c              call calc_put_out(Inum,charans,fstring,1)
c              if (iwp51.eq.1)
c    1              call calc_put_out(Inum,'} ',fstring,0)
c           else
c              if (iwp51.ne.1) then
c                  call calc_put_out(Inum,'(',fstring,1)
c              else
c                  call calc_put_out(Inum,' left( ',fstring,0)
c              endif
c           endif
c
c
c           CurrentNode=INRS            !now analyze RS argument.
c
c           Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
c           flagopv=set_flagval(optype,opvalue  )
c
c           goto 100
c       endif
c
c Case 2:: VALU OR VARI
c
c       If (OpType.eq.VALU.or.OPTYPE.eq.VARI) then
c
c          if (iwp51.eq.1) then
c              call calc_put_out(Inum,'{ ',fstring,0)
c              if (flagopv.lt.0)
c    1                 call calc_put_out(Inum,'-',fstring,0)
c          else
c              if (flagopv.lt.0)
c    1                 call calc_put_out(Inum,'-',fstring,0)
c          endif
c          IF (OpType.eq.VARI) then
c               charans=GET_VariName(flagopv)
c               charans=expand_apost(charans,iwp51)
c          else
c               call calc_writenum(opvalue,charans,ndec,qe,nlen)
c          endif
c
c          call calc_put_out(Inum,charans,fstring,1)
c          if (iwp51.eq.1)  call calc_put_out(Inum,'} ',fstring,0)
c          goto 200
c       endif
c
c
c  CASE 3:: we have a math oper. So, we need to get both left and right nodes.
c  Record this "state" on Stack.
c       vls=inls
c       vrs=inrs
c       Call calc_PushS(vls,LsNotDone,CurrentNode,vrs)  !save left and right nodes
c       CurrentNode=InLs                        !start with left side
c       Call Get_NodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
c       flagopv=set_flagval(optype,opvalue)
c
c       Goto 100
c
c
c HERE After Succesful SUBTREE analysis, or if Node is simple (valu or VARI)
c
c200       continue                             !jump here if returning from subtree analysis
c          if (nerr.gt.0) return                !check for error
c          Call calc_popS(VLS,LsStat,NewNode,VRS)  !vrs only used to hold address
c
c          IF (NewNode.eq.Nil) Then             !equation displayed
c            RETURN                             !so Return
c          endif
c
c          IF (LsStat.eq.LsNotDone) then   !returning from Left node (of oper)
c
c
c             Call Get_nodeInfo(Newnode,OPtype,OPvalue,InLS,InRS)
c             flagopv=set_flagval(optype,opvalue)
c
c             call CALC_oper_cvt(charans,flagopv)       !display the math operator
c             if (iwp51.eq.1) then
c                 if (charans.eq.'/') then                      !/
c                     call calc_put_out(inum,' OVER ',fstring,0)
c                 elseif (charans.eq.'-'.or.charans.eq.'+') then  !+ -
c                     call calc_put_out(inum,' `` ',fstring,0)
c                     call calc_put_out(inum,charans,fstring,1)
c                     call calc_put_out(inum,' `` ',fstring,0)
c                 else                                          ! * % ^
c                      call calc_put_out(inum,charans,fstring,1)
c                 endif
c
c             else
c                call calc_put_out(inum,charans,fstring,1)
c             endif
c
c             Call calc_PushS(Holdvalue,LsDone,NewNode,Vrs)  !ready to get RS
c             CurrentNode=Vrs
c
c             Call Get_nodeInfo(CurrentNode,OPtype,OPvalue,InLS,InRS)
c             flagopv=set_flagval(optype,opvalue)
c
c             goto 100
c
c          else                                 !returning to Right Node
c
c             if (LsStat.eq.LsDone) then                !Returning to MATH OPER
c               continue                        !do nothing
c             elseif (lsstat.eq.lsfunk) then   !perm or user functions
c               continue
c             else              !LsStat must be LsNone: )
c               if (iwp51.ne.1) then
c                  call calc_PUT_OUT(Inum,')',fstring,1)  !end of argument
c               else
c                  call calc_PUT_OUT(Inum,' right) ',fstring,0) !end of
c               endif
c             endif
c
c             goto 200                  !LS or RS subtree value for higher node
c
c          endif                       !Return to LS or RS
c
c
c       RETURN
c       End
c
