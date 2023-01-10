!THIS SUBROUTINE IS CREATING THE TABLE OF ENERGY AND FORCE BETWEEN TWO PARTICLES.
!DUE TO INTERPARTICLE ATTRACTION CORRESPONDING TO GRAVITATIONAL FORCE.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE FGA()

        INCLUDE 'common.h'

        REAL*8 Mi,Mj

        Mi = 0.0
        Mj = 0.0

        DX = ((MAXVAL(SZINDEX))-(MINVAL(SZINDEX)))/NT
        
        Mi = Mi + MINVAL(SZINDEX)
        Mj = Mj + MINVAL(SZINDEX)
        DO I=1,NT

            DO J=1,NT
                
                FT(I,J) = (Mi*Mj*GB*(OMTOMICM**2))/(GKTOPICG**2)
                Mj = Mj + DX

            ENDDO

            Mi = Mi + DX

        ENDDO
        
        RETURN
    
    END SUBROUTINE FGA