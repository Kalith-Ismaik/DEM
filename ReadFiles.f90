!THIS SUBROUTINE IS DEDICATED TO READ THE INPUT FILES FOR SIMULATIONS.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE ReadFiles()
        INCLUDE 'common.h'
    
!       READ INPUT CO-ORDINATE AND VELOCITY FILE

        REWIND 11
        READ(11,*) NAN,TIME
        If(NAN.gt.LPMX) then
            write(* ,*) '  NAN=',NAN,' is greater than LPMX=',LPMX, &
                ' Program stops.' 
        stop
        Endif
        READ(11,*) XL,YL,ZL,XCENTR,YCENTR,ZCENTR
        READ(11,*) (KTYPE(J),J=1,NAN)
        READ(11,*) (KHIST(J),XD(1,J),XD(2,J),XD(3,J),J=1,NAN)
        READ(11,*) (Q1D(1,J),Q1D(2,J),Q1D(3,J),J=1,NAN)
        READ(11,*) (RAD(J),ZMASS(J),J=1,NAN)
        READ(11,*) (SK(J),SN(J),TS(J),VS(J),J=1,NAN)
 
!       READ INPUT DATA
        REWIND 12
        READ(12,100) NSTEP,NEWTAB,NEPRT,NWRITE,NPER,NTYPE, &
             KFLAG,LFLAG,KEYBS,LIDX,LIDY,LIDZ,KBOUND
        READ(12,200) QTEM
        READ(12,200) QPRESS
        READ(12,200) DELTA
        READ(12,200) RSKIN

100     FORMAT (I10)
200     FORMAT (D15.5)
        RETURN

    END SUBROUTINE ReadFiles