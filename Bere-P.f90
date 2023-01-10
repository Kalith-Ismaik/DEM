!THIS SUBROUTINE IS IMPLEMENTING BERENDSEN BAROSTAT.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

!   Implementing Berendsen barostat for keeping constant P
!   At each time step coordinates are scaled by the factor Dzeta.
!   TPRESS is external pressure in GPa

    SUBROUTINE BERE_P(TPRESS)
        INCLUDE 'common.h'

!       pbeta can be increased from 0.001 to 0.1 or even 5.0 for fast 
!       P ajustment in a large particle system

        REAL*8, PARAMETER :: pbeta=0.001d0

        DZETA=1.0d0-pbeta*DELTA*(QPRESS-TPRESS*1.0d-09)

        ZL=ZL*DZETA
        YL=YL*DZETA
        XL=XL*DZETA

        ZLHALF=ZLHALF*DZETA
        YLHALF=YLHALF*DZETA
        XLHALF=XLHALF*DZETA
        
        XD(1,1:NAN)=(XD(1,1:NAN)-XCENTR)*DZETA+XCENTR
        XD(2,1:NAN)=(XD(2,1:NAN)-YCENTR)*DZETA+YCENTR
        XD(3,1:NAN)=(XD(3,1:NAN)-ZCENTR)*DZETA+ZCENTR
        
        RETURN
    
    END SUBROUTINE BERE_P