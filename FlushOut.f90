!FLUSH OUT THE BUFFER OF IUNIT TO FORCE PRINTING/WRITINGTO A FILE; HELPFUL FOR DEBUGGING.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE FlushOut(iunit)

        CHARACTER(LEN=255)   :: filename
        LOGICAL              :: ex, nmd
        INTEGER, INTENT(IN)  :: iunit
        INTEGER              :: ios

!       Acquiring the name of external file
        INQUIRE (UNIT=iunit, IOSTAT=ios, ERR=905, EXIST=ex, NAMED=nmd, NAME=filename)

        IF (ex .AND. nmd) THEN

!          Close unit to flush buffer
           CLOSE (UNIT=iunit, IOSTAT=ios, ERR=905)

!          Reconnect to external file
           OPEN (UNIT=iunit, IOSTAT=ios, ERR=905, STATUS='unknown', POSITION='APPEND', FILE=filename)
        ENDIF

905     CONTINUE
        IF (ios.GT.0) WRITE(*,*) ' FLUSH: UNIT=',iunit,', IOSTAT=',ios
        RETURN
    END SUBROUTINE FlushOut