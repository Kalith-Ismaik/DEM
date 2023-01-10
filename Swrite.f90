!WRITE OUTPUT FILES FOR SURTHER ANALYSIS.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE Swrite()
        INCLUDE 'common.h'
        CHARACTER*5 chtime

        ITIME=ANINT(TIME)
        IF (ITIME.LT.1000) Then
           WRITE(chtime, FMT='(I3.3)') ITIME
        ELSEIF (ITIME.LT.10000) Then
           WRITE(chtime, FMT='(I4.4)') ITIME
        ELSEIF (ITIME.LT.100000) Then
           WRITE(chtime, FMT='(I5.5)') ITIME
        ELSE
           PRINT *, "Simulation is too long... Time=",Time," ps."
           STOP
        ENDIF

        CALL DFT()

        OPEN(UNIT=969,FILE='./'//DRNAME(1:LDR)//'/time'//trim(chtime)//'.d')
        REWIND 969

        WRITE(969,220)(XD(1,J),XD(2,J),XD(3,J),DIAM(J),DAV(J),KTYPE(J),J = 1,NAN)
220     FORMAT(5(F10.4,1x),I2)

        CLOSE (UNIT = 969)

        RETURN
    END SUBROUTINE Swrite