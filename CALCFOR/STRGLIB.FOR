c @c for the public domain, D. Hellerstein, ERS/RTD 11/91
C  THIS FILE CONTAINS SEVERAL ROUTINES TO HELP WITH FORTRAN I/O
C
C  Note -- care should be taken when mixing  these routines with
c          normal fortran i/o.  Specifically, one has to be wary of
c          overwriting.
c
c
c
C     WRITELN(ASTRING)   : WRITES A LINE TO TERMINAL
c             Writeln is LF,string.--so cursor stays at end of string.
c
C     WRTSTRG(ASTRING)   : WRITES A STRING TO TERMINAL, NO RETURN
c                       Wrtstrg WILL overwrite normal Fortran sentences,
c               since it suppresses trailing LF (normal fortran I/O
c                                                is LF,string,CR)
c      CRLF             : send a CRLF
C
C
C    NOTE-- WRITELN,WRTSTRG, AND CRLF CAN BE FREELY MIXED.
C
C     VALUERD(PROMPT)          :  FUNCTION THAT READS AND RETURNS A
C                               A REAL  NUMBER.  PROMPT IS DISPLAYED
C                               PRIOR TO ACCEPTING INPUT. CAN ALSO BE
C                               USED TO PICK UP AN INTEGER
c   valimit (prompt,amin,default,amax) :
c               similar to above, but will not accept values
c                       outside of amin -- amax range.  Also,
c                       <CR> will return default as value.
C
c   VALDEF and VALDEF8 (PROMPT,DEFAULT) = valdef8(prompt,default)
C      Similar to above, but will display " (<CR>=Default_value) ?" after
c               prompt. ValDef8 gets/returns real *8.
C
c   VAL(Astring,Qerror)
c               Convert string to real number. Qerror=true
c               if bad string.
C
C  EG.          VALU = VALUERD(' A PROMPT:')
c               VALL = VALIMIT (' A PROMPT : ',10.,100.,1000.)
C               AVAL=VAL('1224.3',QERROR)
C
C  EG.          INTEGERVAR = VALUERD(' A PROMPT:')
C
c     Strg(ANS,CharAns,NDEC,QE,NLEN)  !ala basic
c
c        Ans== Real *8 number to be converted
c        CharAns == Where to put string (should be *24 to avoid most errors)
c        Ndec   == #  digits to left of decimal. If 0, use integer format.
c        Qe     ==  True if use exponential notation
c        Nlen   == Returned, the number of significant characters in CharAns.
c                       Nlen = 0 if error.
c
c Note:: if Ans is too big (given Ndec) to be written using real
c  format( assuming qe=false) then CharAns will be written using
c  exponential format.
c
c
C     ASCIRD(PROMPT)              : AS ABOVE, FOR A STRING
C                               KLEN, IF INCLUDED, IS LENGTH OF STRING ENTRY
C     CVTUCASE(LETTER)      : RETURNS  THE UPPER CASE OF LETTER
C
C     writeREAL(PROMPT,NVALS,VAL1,..,VALN))
C               : ROUTINE TO WRITE A REAL NUMBER (W/RETURN)
C                 NVALS -- # OF VALUES TO PRINT
C                 VAL1 .. VALN  -- THE VALUES.  (UP TO 10 VALUES)
C                 VALUES ARE WRITTEN WITH F FORMAT.
C
C    READREAL(PROMPT,NVALS,VAL1..VALN)
C       AS ABOVE, EXCEPT READ IN VALUES.  DECIMAL POINT NOT
C       NEEDED, ERROR CAUSES REDISPLAY OF PROMPT.
C
C     writeINTeGeR(PROMPT,NVALS,IVAL1,...,IVALN))            : AS ABOVE
C     READinteger(PROMPT,NVALS,iVAL1..,iVALN)
C
C     Values(Astring,RealArray,nsubs,ierr)
c               Convert a string into an array of up to 10 values
c               (uses PARSE_STRING). RealArray has 10 values. ierr gt 0
c               if error condition. Nsubs is # of values specified (unspecified
c               values returned as 0.0)
C
C     TABEXPAND(ALINE)           :ALINE IS < *140 CHARACTER
C          Will return aline without tabs, but with spaces to pad the line.
C
c       MESSLEN(aline)          !integer function. End of significant characters
C
C       OPENREAD and OPENWRITE.
c
c       Parse_string(ifoo1,SUBS)
c               Parse string (Ifoo1, max *75) into character array
c               subs(max of 10 substrings). Commas are dividers,
c               as are spaces.  If character other than space
c               is to be secondary divider, change ISPC in
c               /CMPARSE_space/.  If Comma is not desired as
c               primary divider, set ICOMMA in /CMparse_comma/
c               Number of strings specified (including empty),
c               returned in NSUBS.
c
c also in here are the following routines  (at end of file)
c get_commandline
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        include 'fgraph.fi'


c=====----------------------------------------------------------

C  GENERAL READ VALUE FUNCTION -- WITH ERROR TRAPPING,ETC.


        real    function valuerd(amess)

        parameter (valmiss=10.0**38)

        character *(*) amess
        CHARACTER *40 ANS

 42       Write(6,44)AMESS
 44     format(' ',a,'?'\)


 10     read(5,45)ANS
 45     FORMAT(A40)

        if (ichar(ans).eq.5) then
           call godos('CALC')
           goto 42
        endif

        if (ans.eq.'?') then
              write(6,678)
 678     format(
     1   ' Enter a value.'/
     1   '   Special values are: ',
     1         ' E= 2.7182   PI=3.1413  '/
     1   '    Any entry format may be used (real,integer,exponential)'/
     1   '    Note: For temporary exit to DOS, Enter ^E ' /
     1    '$  REENTER::')
            goto 10
        endif


        TT=VAL(ANS,IERR)
        IF (IERR.GT.0) THEN
            WRITE(6,66)
 66     FORMAT('$ %BAD INPUT -- REENTER :: ')
            GOTO 10
        ENDIF
        VALUERD=TT
        RETURN
        END

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
        character *4 a4,a4a

        a4(1:1)=char(27)
           a4(2:2)='['
           a4(3:3)='1'
           a4(4:4)='m'
           a4a=a4
        a4a(3:3)='0'

 42     continue
        IF (ABS(DEFVAL).LT.100000) THEN
           IDEFVAL=DEFVAL
           IF (IDEFVAL.EQ.DEFVAL) THEN
                Write(6,4401)a4,AMESS,Idefval,a4a
           ELSE
                Write(6,44)a4,AMESS,defval,a4a
           ENDIF
        ELSE
                Write(6,44)a4,AMESS,defval,a4a
        ENDIF
 44     format(1x,a,1x,a,'(<CR>=',f11.4, ') ?',a\)
 4401   format(1x,a,1x,a,'(<CR>=',I7,') ?',a\)

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
     1   ' REENTER::'\)
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
          write(6,555)a4,ALOW,ALIMIT,DEFVAL,a4a
          GOTO 10
        else
           VALIMIT=aval
           RETURN
        END IF

 555    FORMAT(1x,a,' LOWEST & HIGHEST VALUES ALLOWED ARE=',
     1          F15.7,',',F15.7/
     1         ' :: Please ReEnter  (<CR>=',F15.7,') ? ',a\)

        end



c-----------------------------------------------------------------
        FUNCTION VALdef(Amess,DEFVAL)

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
              write(6,678)
 678     format(
     1   ' Enter a value.'/
     1   '    Special values are: ',
     1        'E= 2.7182   PI=3.1413 '/
     1   '    Any entry format may be used (real,integer,exponential)'/
     1   '    Note: For temporary exit to DOS, Enter ^E ' /
     1   '$ REENTER::')
            goto 10
        endif

        IF (ANS.EQ.' ')THEN
           valdef=defval
           RETURN
        ENDIF

        TT=VAL(ANS,iERR)
        IF (iERR.ne.0) THEN
            WRITE(6,66)
 66     FORMAT('$ %BAD INPUT -- REENTER :: ')
             GOTO 10
        ENDIF

        VALDEF=TT
        RETURN

        END

c-----------------------------------------------------------------
        real *8 FUNCTION VALdef8(Amess,DEFVAL)   ! same as valdef

        character *(*) amess
        real *8 defval
        tmp=defval
        aa=valdef(amess,tmp)
        valdef8=aa
        return
        end

c------------------------------
c read value, recognize ALT-H & F1

        real  function valdef_2(amess,defval,ihelp)

        parameter (valmiss=10.0**38)

        character *(*) amess
        CHARACTER *40 ANS,ascird_2a
        character *80 tmp

        ihelp=0

 42     continue
        if (defval.ge.valmiss) then
           write(tmp,1411)amess
        else
           Write(tmp,44)AMESS,defval
        endif
 44     format(a,'(<CR>=',f11.4, ') ?'\)
 1411     format(a,'(<CR>=Missing Value) ?'\)
 45     FORMAT(A40)

 10     ans=ascird_2a(tmp(1:len_trim(tmp)+3),iarg)
        if (iarg.eq.8960.OR.IARG.EQ.15104) then
           call down_line(1)
           ihelp=1
           valdef_2=defval
           return
        elseif (iarg.gt.255.or.ans.eq.'?') then
              write(6,678)
              goto 10
        elseIF (iarg.lt.32)THEN
           valdef_2=defval
           RETURN
        ENDIF


 678     format(
     1   ' Enter a value.'/
     1   '    Special values are: ',
     1        'E= 2.7182   PI=3.1413   '/
     1   '    Any entry format may be used (real,integer,exponential)'/
     1   '    Note: For temporary exit to DOS, Enter ^E ' /
     1   '    For further help, enter ALT-H '/
     1   '$ REENTER::')

c possibly a valid number
        TT=VAL(ANS,IERR)
        IF (IERR.GT.0) THEN
            WRITE(6,66)
 66     FORMAT('$ %BAD INPUT -- REENTER :: '/)
             GOTO 10
        ENDIF

        valdef_2=TT
        RETURN

        END



c-----------------------------------------------------------------
C NOTE: Q In format statement returns number of character remaining to
c be read.


        CHARACTER *(*) FUNCTION ASCIRD(PROMPT)

        character *(*) prompt
        CHARACTER *80 ANS

 42     Write(6,44)prompt
 44     format(' ',a,'?'\)

 10     read(5,45)ANS
 45     FORMAT(A80)

        if (ichar(ans).eq.5) then
           call godos('CALC')
           goto 42
        endif

        ascird=ans
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


c SUPERSEDED BY ASCIRD_2a  !!

        character *(*) function ascird_2(prompt,iarg)

           character *(*) prompt
           character *80  aword,ascird_2a

           aword=ascird_2a(prompt,iarg)
           ascird_2=aword
        return
        end

c============================
c This is SUPERSEDED by ASCIRD_2b, (SOME CURSOR KEYS IMPLEMENTED)

        character *(*) function ascird_2A(prompt,iarg)
        character *(*) prompt
        character *80  aword,ascird_2b

        aword=ascird_2b(prompt,iarg,' ')
        call down_line(1)
        ascird_2a=aword
        return
        end



c============================
c This supersedes ASCIRD_2a, (oldline serves as starting value )

        character *(*) function ascird_2b(prompt,iarg,oldline)
        character *(*) prompt,oldline
        character *80  aword
        character *1 a1,a2,bk,blank,cc(80)
        integer *2 ain2,getchasm,ain3
        character *4 a4,a4a,abell,afull,aempty

        INSDO=0                 ! NON-INSERT MODE

        a4(1:1)=char(27)
           a4(2:2)='['
           a4(3:3)='1'
           a4(4:4)='m'
           a4a=a4
        a4a(3:3)='0'
        bk=char(8)
        blank=char(32)
        abell=char(7)
        afull=char(219)
        aempty=char(179)

        ithlet=1

        do ij=1,80                      ! initialize
          cc(ij)=' '
        enddo

        leftonline=76-len(prompt)
c if prompt is large
        if (leftonline.eq.' ') then
            oldline=' '
        endif

c info on prior line
        if (oldline.ne.' ') then
           nletold=min(len_trim(oldline),leftonline)
           do ij=1,nletold
               cc(ij)=oldline(ij:ij)
           enddo
        else
           nletold=0
        endif


c if prior line to be used  (nb: jump here on ^E)
 42     continue

        Write(6,44)a4,prompt,a4a
 44         format(1x,a4,a,'?',a4,' '\)

        call del_to_end

        if (nletold.gt.0) then        !got an old line
            do ij=1,nletold             ! write it
                write(6,411)cc(ij)
            enddo
            if (iarg.gt.0) then
              ithlet=1
              do ij=1,nletold             ! move back to start of line
                 write(6,411)bk
              enddo
            else                        !dont back up
                ithlet=nletold+1
            endif
         endif

        nlets=nletold

 411               format(a1,\)         ! used to move in line

c keep entering characters
        do while (1.lt.2)                  !infinite loop, exit via EXIT

c get 1 character
        ain2=0
        do while (ain2.eq.0)
            ain2=getchasm()
            if (ain2.eq.29440) ain2=40
            if (ain2.eq.29696) ain2=41
        enddo

c check special cases: f1 alt-h CR LF
            if (ain2.eq.15104.or.ain2.eq.8960
     1          .or. ain2.eq.13 .or. ain2.eq.10) then  !F1 alt-h, cr lf
                exit
            endif
c esc
            if (ain2.eq.27) then                ! esc
                write(6,4113)a4,a4a
 4113           format(a1,' <ESC> ',a1\)
               nlets=0
               exit
            endif
c ^E
           if (ain2.eq.5) then
                call godos('CALC')
                nletold=nlets
                goto 42
           endif


C ins KEY
           IF (AIN2.EQ.20992) THEN
                A2=CC(ITHLET)
                INSDO=1-INSDO                   !TOGGLE IT
                if (insdo.eq.1) then
                    write(6,411)afull
                else
                    write(6,411)aempty
                endif
                ain3=0
                ifoo=0
                do while (ain3.eq.0 .and. ifoo.lt.255)
                     ain3=getchasm()
                     ifoo=ifoo+1
                enddo
                WRITE(6,411)BK
                WRITE(6,411)A2
                WRITE(6,411)BK
                cycle
           ENDIF


c no room left in line, so jump unless it's bs,left arrow, or home.
           if ( (nlets.ge.80) .and. (.not.
     1       (ain2.eq.8 .or. ain2.eq.19200 .or. ain2.eq.18176))) then
                cycle
           endif

c BACKSPACE
            if (ain2.eq.8) then
               if (ithlet.gt.1) then
                  ithlet=ithlet-1
                  cc(ithlet)=' '
                  write(6,411)bk
                  write(6,411)blank
                  write(6,411)bk
                  if (ithlet.eq.nlets) nlets=nlets-1
                  cycle
               else
                   cycle
               endif
            endif

C DEL key
           IF (AIN2.EQ.21248) THEN
                if (ithlet.gt.nlets) then       !nothing to delete
                    cycle
                else
                   itmp=ithlet+1
                   ibackdo=0
                   do while (itmp.le.nlets+1)
                       write(6,411)cc(itmp)
                       cc(itmp-1)=cc(itmp)
                       itmp=itmp+1
                       ibackdo=ibackdo+1
                   enddo
                   do itmp=1,ibackdo
                       write(6,411)bk
                   enddo
                   nlets=nlets-1
                   cycle
              endif
            endif

c left arrow (backup)
            if (ain2.eq.19200  ) then
               if (ithlet.gt.1) then
                   ithlet=ithlet-1
                   write(6,411)bk
                   cycle
             else
                   cycle
             endif
            endif

c end
            if (ain2.eq.20224  ) then
               if (ithlet.gt.nlets) then
                  cycle  !already at end
               endif

               do while (ithlet.le.nlets)
                   write(6,411)cc(ithlet)
                   ithlet=ithlet+1
               enddo
               cycle
            endif

c tab
            if (ain2.eq.9  ) then
               if (ithlet.ge.nlets) then
                  cycle  !already at end
               endif

               itmp=int(float(ithlet)/8.0)+1
               itmp=(8*itmp)+1
               itmp=min(itmp,nlets)
               do while (ithlet.le.itmp)
                   write(6,411)cc(ithlet)
                   ithlet=ithlet+1
               enddo
               cycle
            endif

c home
            if (ain2.eq.18176  ) then
               a2=cc(ithlet)
               if (ithlet.gt.1) then
                  do while (ithlet.gt.1)
                      ithlet=ithlet-1
                      write(6,411)bk
                   enddo
                   cycle
             else
                   cycle
             endif
            endif

c right arrow (forward)
            if (ain2.eq.19712) then
                   write(6,411)cc(ithlet)
                   ithlet=ithlet+1
                   cycle
            endif

C else, keep accepting characters?
            if (ain2.le.255) then
                if (insdo.eq.0.or.nlets.lt.ithlet) then    ! replace mode
                    if (ithlet.gt.nlets) nlets=ithlet
                    cc(ithlet)=char(ain2)
                    write(6,411)cc(ithlet)
                    ithlet=ithlet+1
               else                            ! insert mode
                     itmp=ithlet
                     a1=char(ain2)
                     do while (itmp.le.nlets)   ! insert-copy
                        a2=cc(itmp)
                        cc(itmp)=a1
                        itmp=itmp+1
                        a1=a2
                     enddo
                     nlets=nlets+1
                     cc(nlets)=a1
c now write out cc
                     itmp=ithlet
                     nbackdo=0
                     do while (itmp.le.nlets)
                        write(6,411)cc(itmp)
                        nbackdo=nbackdo+1
                        itmp=itmp+1
                     enddo
                     do itmp=1,nbackdo-1
                        write(6,411)bk
                     enddo
                     ithlet=ithlet+1
                endif

            else                                ! all other non-ascii=ENTER
                exit
            endif

          end do                                ! end of infinite loop

c put together word, and exit
 1000   continue
        a1=char(13)
        write(6,411)a1

c Do NOT give a new line !
c       a1=char(10)
c       write(6,411)a1

        aword=' '
        if (nlets.eq.0) then
                 ASCIRD_2b=aword
         else
              do 68 ijj=1,min(nlets,80)
                   aword(ijj:ijj)=cc(ijj)
 68             continue
              ASCIRD_2b=aword
         endif
         iarg=ain2
         return

        end

c============================
c delete to end of line
        subroutine del_to_end

        character *3 a3del

        a3del(1:1)=char(27)
        a3del(2:2)='['
        a3del(3:3)='K'
        write(6,511)a3del
 511    format(a3\)

        return

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

        do 100 ij=1,len(sent)
            let=ichar(sent(ij:IJ))
            if (let.ge.97.and.let.le.122) sent(ij:ij)=char(let-32)
 100    continue
        return
        end

C=================================================================
C alternate name of FUNCTION TO CONVERT FROM Lower TO upper CASE

        CHARACTER *1 FUNCTION CVTUCASE(LETTER)

        CHARACTER *1 LETTER

        call captaliz(letter)
        cvtucase=letter

        RETURN
        END




C-----------------------------------------------------------------
C Routine to open a sequential data file, with 'UNKNOWN' status,
C
        character *(*) function
     1          OPENWRITE(PROMPT,UNITNUMBER,DEFEXT,NERR)


        CHARACTER *(*) PROMPT
        CHARACTER *(*) DEFEXT
        INTEGER UNITNUMBER

        character *60 ascird_2a,filename

        filename=ascird_2a(prompt,iarg)
        if (iarg.eq.27.or.iarg.gt.255) then
          goto 900
        elseif (filename.eq.' ') then
          goto 900
        endif

        CALL CHKEXTENSION(FILENAME,DEFEXT)

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

C-----------------------------------------------------------------
C Routine to open a sequential data file, with 'READ' status,
C
        character *(*) function OPENREAD(PROMPT,UNITNUMBER,NERR)

        CHARACTER *(*) PROMPT
        INTEGER UNITNUMBER

        character *60 ascird_2a,filename


10      continue
        filename=ascird_2a(prompt,iarg)
        if (iarg.eq.27.or.iarg.gt.255) then
          goto 900
        elseif (filename.eq.' ') then
          goto 900
        endif


        OPEN(UNIT=UNITNUMBER,FILE=FILENAME,ACCESS='SEQUENTIAL',
     1         err=890,status='OLD',mode='READ')

        openread=filename
        NERR=0
        RETURN

  890   continue
        print *,' No such file: Reenter. '
        goto 10

 900    continue
        openread=' '
        NERR=1
        RETURN

        END

C============================================================
C
C - - - - - - - - - - - - - - - - - - - - -- - - -  - - -- - - - -
C
C  ROUTINE TO SEARCH FOR EXTENSION IN FILE NAME.
C IF NO EXTENSION FOUND (NO . AT END OF FILE NAME), ADD
C  'DEFEXT' TO FILE NAME.

        SUBROUTINE CHKEXTENSION(FILEI,DEFEXT)
        CHARACTER *(*) FILEI
        CHARACTER *(*) DEFEXT


        LENFILE=LEN(FILEI)

        DO I=LENFILE,1,-1
           IS=I
           IF (FILEI(I:I).EQ.']'.OR.FILEI(I:I).EQ.':') GOTO 100
        END DO

        IS=0


 100    CONTINUE

        IS=IS+1

        DO II=IS,LENFILE
          IF (FILEI(II:II).EQ.'.') GOTO 200
        END DO

C  HERE IF NO EXTENSION FOUND

        DO II=LENFILE,IS,-1
           ILAST=II
           IF(FILEI(II:II).NE.' ') GOTO 140             !LAST CHARACTER
        END DO

 140    IF (ILAST+4.LE.LENFILE) FILEI(ILAST+1:ILAST+4)=DEFEXT

 200    CONTINUE
        RETURN
        END


c==========================================================

        INTEGER FUNCTION MESSLEN(ALINE)

        CHARACTER *(*) ALINE

        LL=len_trim(aline)
        messlen=ll
        return
        END


c====================================
c Get arguments from the command line.  If none given, then
c display prompt and get from user.

        character *(*) function get_commandline(prompt,kommand)


        character *(*)prompt

        integer *2 i2a,i2b
        character *70 aline,choice,ascird_2b

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
 10            choice=ascird_2b(prompt,iarg,' ')
               if (iarg.gt.255) then
                  kommand=3
                  choice=' '
               elseif (iarg.eq.27) then
                   choice=' '
                   kommand=2
              endif
              call down_line(1)
        endif


        call captaliz(choice)
        get_commandline=choice

        return

        end

c====================================
c get a prior command, depending on what was last command retrieved
c (signaled in nat)

        subroutine pop_ahistory(strung,imove)
        parameter (len_history=20)
        character *(*) strung
        character *80 A_history(len_history)
        common /cm_a_history/nat,ngot,a_history

        if (ngot.eq.0) then           !nothing entered yet
           strung=' '
           return
        endif

c else, get from history list
        ido=nat+imove
        ido=max(min(len_history,ido),1)
        if (ido.le.ngot) then
           strung=a_history(ido)
        else
           strung=' '
        endif
        nat=ido
        return

        end

c------------------------------
c add to bottom of list, removing oldest if necessary
        subroutine push_ahistory(strung)

        parameter (len_history=20)
        character *(*) strung
        character *80 A_history(len_history)
        common /cm_a_history/nat,ngot,a_history

        if (ngot.lt.len_history) then            !room on list
           ngot=ngot+1
           nat=ngot+1
           a_history(ngot)=strung
        else
           do ij=2,len_history
             a_history(ij-1)=a_history(ij)
           enddo
           nat=len_history              ! force history to bottom of list
           a_history(len_history)=strung
        endif

        return
        end


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

c-------------------
c find next legit character, starting at nchar+1.  If
c none, or nchar > len(foo), return nchar=len(foo)+1, achar= ' '

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

c===========================
C FIND NEXT NON-CHAR CHAR IN STRING
        FUNCTION NEXT_NON_CHAR(IAT0,PSTRUNG,ILEN)
        CHARACTER *(*)PSTRUNG
        CHARACTER *1 AC

        IAT=IAT0+1
        DO WHILE (IAT.LE.ILEN)
          AC=PSTRUNG(IAT:IAT)
          IF (AC.EQ.' '.OR.AC.EQ.';'.OR.AC.EQ.':') THEN
             EXIT
          ENDIF
          IAT=IAT+1
        ENDDO

        NEXT_NON_CHAR=IAT       !IAT>ILEN, OR EXIT
        RETURN
        END

c===========================
c read a line from a file
          character *(*) function get_from_file(in_unit)

          character *230 aline          !MAX FROM A LINE IN A FILE


          read(in_unit,33,end=200,err=200)aline
 33     format(a)
          get_from_file=aline
          return

 200    continue
        in_unit=5
        get_from_file=' '
        return

        end





c------------------------------------
c init=1 = clear screen
         subroutine print_at(irow,icol,ifron,iback,amess,init)
        include 'fgraph.fd'
         record /rccoord/s
         character *(*)amess
         integer *2 a2,g2
         integer rr,cc
        if (init.eq.1) then
            a2=setvideomode($textc80)
           CALL clearscreen( $GCLEARSCREEN )
        endif
        nn=setbkcolor(iback)
        a2=ifron
        rr=irow
        cc=icol
        g2=settextcolor(a2)
        call settextposition(rr,cc,s)
        if (len_trim(amess).gt.0) then
           call outtext(amess(1:len_trim(amess)))
        endif
        return
        end

c
c----------------------
C      SUBROUTINE graphicsmode(maxx4,maxy4,MODE)
C
C      INCLUDE  'FGRAPH.FD'
C
C      INTEGER*2            dummy, maxx, maxy
C      RECORD /videoconfig/ myscreen
C
CC
CC     Find graphics mode.
CC
C      CALL getvideoconfig( myscreen )
C      SELECT CASE( myscreen.adapter )
C         CASE( $CGA )
C            dummy = setvideomode( $HRESBW )
C         CASE( $OCGA )
C            dummy = setvideomode( $ORESCOLOR )
C         CASE( $EGA, $OEGA )
C            IF( myscreen.monitor .EQ. $MONO ) THEN
C                dummy = setvideomode( $ERESNOCOLOR )
C            ELSE
C                dummy = setvideomode( $ERESCOLOR )
C            END IF
C         CASE( $VGA, $OVGA, $MCGA )
C            dummy = setvideomode( $VRES2COLOR )
C         CASE( $HGC )
C            dummy = setvideomode ( $HERCMONO )
C         CASE DEFAULT
C            dummy = 0
C      END SELECT
C
C
C      IF( dummy .EQ. 0 ) then
C           print *,' Warning:  cannot set graphics mode'
C           maxx=0
C           maxY=0
C      ELSE
CC
CC  Determine the minimum and maximum dimensions.
CC
C        CALL getvideoconfig( myscreen )
C        maxx = myscreen.numxpixels - 1
C        maxy = myscreen.numypixels - 1
C
C      endif
C
C      maxx4=maxx
C      maxy4=maxy
C      mode=dummy
C
C      RETURN
C
C      END
C
C---------------------------------------------------------------
C
C ROUTINE TO EXPAND TABS IN A LINES.
C RETURNS ALINE WITHOUT ANY TABS.
C
C       SUBROUTINE TABEXPAND(ALINE)
C
C       CHARACTER *(*) ALINE
C       CHARACTER *140 ALINE2           !MAX OF 140 CHARACTER LONG LINE
C       CHARACTER *1 TAB,C1,CHAR
C       INTEGER FINDTAB
C
C       LL=MIN(LEN(ALINE),140)          !THIS CHUNK DETERMINES MESSAGE LENGTH
C       do ij=LL,1,-1
C         nlen=ij
C         if (Aline(ij:ij).gt.' ') goto 10
C       end do
C       MESSLEN=0
C10     CONTINUE
C       MESSLEN=NLEN
C
C       TAB=CHAR(9)
C       ALINE2=' '
C
C WE WANT TO COPY INTO ALINE2 ONE CHARACTER AT A TIME. IF
C  WE FIND A TAB, WE WANT TO FILL FROM "LOGICAL" TAB LOCATION
C  TO NEXT TAB STOP WITH SPACES.
C
C       NEWPOS=1
C       OLDPOS=1                !START AT BEGINNING
C
C       DO IJ=1,NLEN
C         C1=ALINE(IJ:IJ)
C         IF (C1.NE.TAB) THEN
C            ALINE2(NEWPOS:NEWPOS)=C1
C            NEWPOS=NEWPOS+1
C         ELSE
C            ITABSPOT=FINDTAB(NEWPOS)           !FIND NEXT TAB AFTER CURRENT NEWPOS
C            IF (NEWPOS+1.GE.ITABSPOT) THEN
C               ALINE2(NEWPOS:NEWPOS)=' '
C               NEWPOS=NEWPOS+1
C               GOTO 90
C            END IF
C            DO IJJ=NEWPOS,ITABSPOT-1
C               ALINE2(IJJ:IJJ)=' '
C            END DO
C            NEWPOS=ITABSPOT
C         END IF
C
C90       CONTINUE
C         IF (NEWPOS.GE.140) GOTO 100
C
C       END DO
C
C100    CONTINUE
C
C       ALINE=ALINE2
C       RETURN
C       END
C
CCC
C
C       INTEGER FUNCTION FINDTAB(IAT)
C
C       DATA TABPOS /9,17,25,33,41,49,57,65,73/         !VT100 TAB POSITIONS
C
C       IF (IAT.GE.140) THEN
C          FINDTAB=140
C          RETURN
C       END IF
C
C       IF (IAT.LT.9) THEN
C          FINDTAB=9
C          RETURN
C       END IF
C
C       IAT2=IAT-9
C       ITAB=(IAT2/8)+1
C
C       FINDTAB=9+(ITAB*8)
C
C       END
C
C
C-------------------------------------------------
