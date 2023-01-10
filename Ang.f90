!FUNCTION ANG PROVIDES THE FRICTION ANGLE BETWEEN TWO PARTICLES.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    FUNCTION Ang(I,J,D)

        REAL*8, DIMENSION(3) :: I,J,A,B,M,K
        REAL*8 :: mm, nn, alpha, DEG

        
        M = (I(1:3) + J(1:3))/2

        K(1:3) = M(1:3)
        K(3)   = 0.0

        A(1:3) = I(1:3) - M(1:3)
        B(1:3) = K(1:3) - M(1:3)

        mm = A(1)**2 + A(2)**2 + A(3)**2
        nn = B(1)**2 + B(2)**2 + B(3)**2

        mm = SQRT(mm)
        nn = SQRT(nn)

        alpha = ((A(1)*B(1)) + (A(2)*B(2)) + (A(3)*B(3)))

        alpha = alpha/mm*nn

        DEG   = ACOS(alpha)

    END FUNCTION Ang