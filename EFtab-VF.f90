!THIS SUBROUTINE IS CREATING THE TABLE OF ENERGY AND FORCE BETWEEN TWO PARTICLES.
!DUE TO INTERPARTICLE INTERACTION IS DEFINED BY VISCOUS FORCE.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE FVF()
    
        INCLUDE 'common.h'

        VF(1:NPOTS) = 0.0

        DO I=1,NPOTS

            DO J=1,NPOTS

                IJ     = IJINDEX(I,J)    
                VF(IJ) = 4.0 * PI * DELTA * VSI(IJ) 

            ENDDO

        ENDDO

        RETURN

    END SUBROUTINE FVF