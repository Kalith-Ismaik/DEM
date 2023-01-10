!THIS SUBROUTINE PROVIDES THE CUTOFF DISTANCE FOR DIFFERENT TYPES OF INTERACTING PARTICLES.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE CutOff()
        INCLUDE 'common.h'

        REAL*8 :: FF
        INTEGER*8 :: II

        TVAL = 1D-06

        C = (GB * OMTOMICM)/(GKTOPICG*(STOMS**2))

        CC = 0.0
        DT = 5D-14

        DO I=1,NTYPE

            DO J=1, NTYPE
                
                DO K=1,100000

                    CC = CC + DT
                    FF = C * XMASS(I) * XMASS(J) / (CC**2)
                    
                    IF(FF.GT.TVAL) II = K   

                END DO

                IJ     = IJINDEX(I,J)
                RM(IJ) = II*DT

                II = 0.0
                CC = 0.0

            END DO

        END DO

        RETURN

    END SUBROUTINE CutOff