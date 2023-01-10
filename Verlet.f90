!DIFFERENT VERSIONS OF VERLET ALGORITHM.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

!VELOCITY VERLET ALGORITHM.
!NO NEED OF STARTER ALGORITHM.
 
    SUBROUTINE VelVer()

        INCLUDE 'common.h'

        QIN(1:NAN)=0.0d0
        Q1(1:NAN3)=0.0d0

        FO(1:NAN3) = F(1:NAN3) 

        CALL Forces()
        
        VV_loop: DO I=1, NAN3
            IF(KRIGID(I).EQ.1) CYCLE VV_loop
            I3=(I-0.5)/3.+1.

            X(I)    = X(I) + (DELTA)*Q1(I) + ((DELTA**2)*F(I))/(2*ZMASS(I3))
            Q1(I)   = Q1(I) + (F(I) + FO(I))*(DELTA/(2*ZMASS(I3)))
            QA(I)   = Q1(I) 
            QIN(I3) = QIN(I3) + ((Q1(I)**2)*ZMASS(I3))/2 

        END DO VV_loop

        RETURN 

    END SUBROUTINE VelVer

!---------------------------------------------------------------------------------------
!BHEEMAN SCHOFIELD ALGORITHM.
!IN NEED OF STARTER ALGORITHM.

    SUBROUTINE Bheem()

        INCLUDE 'common.h'

        QIN(1:NAN)=0.0d0
        Q1(1:NAN3)=0.0d0

        FO(1:NAN3)  = FO1(1:NAN3)
        FO1(1:NAN3) = F(1:NAN3) 

        CALL Forces()
        
        VV_loop: DO I=1, NAN3
            IF(KRIGID(I).EQ.1) CYCLE VV_loop
            I3=(I-0.5)/3.+1.

            X(I)    = X(I) + (DELTA)*Q1(I) + ((DELTA**2)*(4*F(I) - FO(I)))/(6*ZMASS(I3))
            Q1(I)   = Q1(I) + (2*F(I) + 5*FO1(I) - FO(I))*(DELTA/(6*ZMASS(I3)))
            QIN(I3) = QIN(I3) + ((Q1(I)**2)*ZMASS(I3))/2 

        END DO VV_loop

        RETURN 

    END SUBROUTINE Bheem