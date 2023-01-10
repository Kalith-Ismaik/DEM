!THIS SUBROUTINE IS EXPRESSING THE FORCE IMPARTING ON THE PARTICLES DUE TO GRAVITY.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE Gravity()
        INCLUDE 'common.h'
 
       C = (GA*OMTOMICM)/(STOMS**2)
!        C = (GA*OMTOMICM)

        FG(1:NAN) = 0.0
        GRAV_loop: DO I=1,NAN

            H = XD(3,I)

            FZ = ZMASS(I) * C
            PZ = ZMASS(I) * C * H

            FD(3,I)=FD(3,I)-FZ               ! z component of force
            POT(I) =POT(I) -PZ

            P = FZ/H
!           Static Portion of Stress Tensor
            STEN(I,3,3)=STEN(I,3,3)+P*H*H
    
        END DO GRAV_loop
        
        RETURN

    END SUBROUTINE Gravity