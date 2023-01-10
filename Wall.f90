!THIS SUBROUTINE IS DEDICATED TO EXCERT FORCE ON PARTICLES IN THE SYSTEM AS A BOUNDARY WALL.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE Wall()

        INCLUDE 'common.h'

        KRIGID(1:NAN3) = 0
        W_loop: DO I=1,NAN

           I3=I+I+I
           
           IF(LIDX.EQ.2) THEN
              DX =X(I3-2)-XCENTR-RAD(I)*XMDR
              DXX=X(I3-2)-RAD(I)*XMDR
              IF(DX.EQ.XLHALF.OR.DX.EQ.-XLHALF) KRIGID(I3-2) = 1
              IF(DX.GT.XLHALF.OR.DX.LT.-XLHALF) F(I3-2) = -SK(I)*DXX
           ENDIF
           
           IF(LIDZ.EQ.2) THEN
              DZ =X(I3)-ZCENTR-RAD(I)*XMDR
              DZZ=X(I3)-RAD(I)*XMDR
              IF(DZ.EQ.ZLHALF.OR.DZ.EQ.-ZLHALF) KRIGID(I3) = 1
              IF(DZ.GT.ZLHALF.OR.DZ.LT.-ZLHALF) F(I3) = -SK(I)*DZZ
           ENDIF
           
           IF(LIDY.EQ.2) THEN
              DY =X(I3-1)-YCENTR-RAD(I)*XMDR
              DYY=X(I3-1)-RAD(I)*XMDR 
              IF(DY.EQ.YLHALF.OR.DY.EQ.-YLHALF) KRIGID(I3-1) = 1
              IF(DY.GT.YLHALF.OR.DY.LT.-YLHALF) F(I3-1) = -SK(I)*DYY
           ENDIF

        END DO W_loop
        
        RETURN
    
    END SUBROUTINE Wall