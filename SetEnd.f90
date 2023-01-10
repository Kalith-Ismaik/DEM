!THIS SUBROUTINE CLEANS UP AFTER THE DEM timestep LOOP
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE SetEnd()
        INCLUDE 'common.h' 

!       Since we multiplied velocities by timestep DELTA
!       at the beginning of the run (in SetInit.f) we have
!       to divide by DELTA here to get real velocities
        
        DO I=1,NAN3
           Q1(I)=Q1(I)/DELTA
        ENDDO
        
        DO I=1,NAN
           R1(I)=R1(I)/DELTA
        ENDDO
        
        RETURN
    END SUBROUTINE SetEnd