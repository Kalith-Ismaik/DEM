!THIS SUBROUTINE GATHERS PARTICLES TO THE COMPUTATIONAL CELL TIME TO TIME. 
!IT HELPS TO GATHER THE PARTICLES DIFFUSE OUTSIDE OF THE COMPUTATIONAL CELLS.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE Gather()

        INCLUDE 'common.h'
    
        G_loop: DO I=1,NAN

           I3=I+I+I
           
           IF(LIDX.EQ.1) THEN
              DX=X(I3-2)-XCENTR 
              IF(DX.GT.XLHALF) X(I3-2)=X(I3-2)-XL
              IF(DX.LT.-XLHALF) X(I3-2)=X(I3-2)+XL
           ENDIF
           
           IF(LIDZ.EQ.1) THEN
              DZ=X(I3)-ZCENTR 
              IF(DZ.GT.ZLHALF) X(I3)=X(I3)-ZL 
              IF(DZ.LT.-ZLHALF) X(I3)=X(I3)+ZL 
           ENDIF
           
           IF(LIDY.EQ.1) THEN
              DY=X(I3-1)-YCENTR 
              IF(DY.GT.YLHALF) X(I3-1)=X(I3-1)-YL
              IF(DY.LT.-YLHALF) X(I3-1)=X(I3-1)+YL
           ENDIF

        END DO G_loop
        
        RETURN
    
    END SUBROUTINE Gather