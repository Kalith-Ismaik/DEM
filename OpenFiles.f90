!THIS SUBROUTINE IS DEDICATED TO OPEN AND VERIFY THE INPUT FILES FOR SIMULATIONS.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.
    
    SUBROUTINE OpenFiles(Dname)
    
        INCLUDE 'common.h'

        CHARACTER*4   ::  str
        CHARACTER*14  ::  str1
        CHARACTER*14  ::  str2
        CHARACTER*8   ::  Dname
        INTEGER       ::  iunit

        OPEN (UNIT = 10 , FILE = Dname)

        opener_loop: DO 

            READ(10,*,END = 1005) str,iunit,str1,str2

            IF (str(1:1).eq.'#') CYCLE opener_loop
            IF (str(1:1).eq.'*') THEN
                ldr=leng(str1)
                IF(ldr.eq.0) Then
                    WRITE(*,*) 'Wrong name of directory',str1
                    STOP
                ENDIF
                drname(1:ldr)=str1(1:ldr)
                CYCLE opener_loop
            ENDIF

            lf1=leng(str1)
            IF (lf1.eq.0) Then
                WRITE(*,*) 'Wrong format in the file ',openf
                STOP
            ENDIF

            lf2=leng(str2)
            OPEN (iunit,file=str1,status=str2,err=255)
            CYCLE opener_loop
255         WRITE(*,*) 'Error while reading file ',str1
            STOP
1005        EXIT opener_loop

        END DO opener_loop

    RETURN

    END SUBROUTINE OpenFiles