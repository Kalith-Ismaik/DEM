!THIS SUBROUTINE IS ESTIMATING THE ENERGY AND FORCE BETWEEN TWO PARTICLES.
!DUE TO INTERPARTICLE INTERACTION IS DEFINED BY VISCOUS FORCE.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.
    
    SUBROUTINE FFVF()
        INCLUDE 'common.h'

        REAL*8,DIMENSION(3) :: Q
        INTEGER*8           :: I3,J3,K3,L3

        LOGICAL :: keySTOP=.FALSE.

        outer_loop: DO I=1,NAN
           I3=I+I+I

           IF (NNG(I).gt.MAXNNB) THEN
              print *, ' TOO MANY NEIGHBOURS: ',NNG(I),' > ', maxnnb
              STOP
           ENDIF
        
           ktypei=KTYPE(I)
        
           inner_loop: DO K=1,NNG(I)
        
                J=NNNG(K,I)
                J3=J+J+J
                DX=XD(1,J)-XD(1,I)
                DY=XD(2,J)-XD(2,I)
                DZ=XD(3,J)-XD(3,I)

!               Periodicity *************
                IF(LIDX.EQ.1) THEN
                   IF(DX.GT.XLHALF) DX=DX-XL
                   IF(DX.LT.-XLHALF) DX=DX+XL
                ENDIF
                IF(LIDZ.EQ.1) THEN
                   IF(DZ.GT.ZLHALF) DZ=DZ-ZL
                   IF(DZ.LT.-ZLHALF) DZ=DZ+ZL
                ENDIF
                IF(LIDY.EQ.1) THEN
                   IF(DY.GT.YLHALF) DY=DY-YL
                   IF(DY.LT.-YLHALF) DY=DY+YL
                ENDIF
!               End of periodicity ******

                IJ = ijindex(KTYPEI, KTYPE(J))

                RR=DX*DX+DY*DY+DZ*DZ
                RD=SQRT(RR)

                RD=(RAD(I)+RAD(J))-RD

                IF (RD.LE.0) CYCLE inner_loop

                K3 = J3-2
                L3 = I3-2
                
                DO L=1, 3
                  Q(L) = QA(K3) - QA(L3)
                  K3 = K3+1
                  L3 = L3+1
                ENDDO

                !Q(1:3) = 0
                
                FI = -VF(IJ)*Q(1)
                FJ = -VF(IJ)*Q(2)
                FK = -VF(IJ)*Q(3)

                PX=FI/RD
                PY=FJ/RD
                PZ=FK/RD

!               Static Portion of Stress Tensor
                STEN(I,1,1)=STEN(I,1,1)+PX*DX*DX
                STEN(I,2,2)=STEN(I,2,2)+PY*DY*DY
                STEN(I,3,3)=STEN(I,3,3)+PZ*DZ*DZ

                STEN(J,1,1)=STEN(J,1,1)-PX*DX*DX
                STEN(J,2,2)=STEN(J,2,2)-PY*DY*DY
                STEN(J,3,3)=STEN(J,3,3)-PZ*DZ*DZ

                DX=PX*DX                          ! x component of force
                FD(1,I)=FD(1,I)+DX
                FD(1,J)=FD(1,J)-DX
                DZ=PY*DZ                          ! z component of force
                FD(3,I)=FD(3,I)+DZ
                FD(3,J)=FD(3,J)-DZ
                DY=PZ*DY                          ! y component of force
                FD(2,I)=FD(2,I)+DY
                FD(2,J)=FD(2,J)-DY

            END DO inner_loop
        
        END DO outer_loop
        
        IF(keySTOP) STOP

        RETURN

    END SUBROUTINE FFVF