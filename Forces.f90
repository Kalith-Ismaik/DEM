!THIS SUBROUTINE IS EXPRESSING THE FORCE IMPARTING ON THE PARTICLES IN THE SYSTEM.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, DECEMBER-2022, NUS SINGAPORE.

    SUBROUTINE Forces()

        INCLUDE 'common.h'

        F(1:NAN3)=0.0d0
        POT(1:NAN)=0.0d0
        STEN(1:NAN,1:3,1:3)=0.0d0

        CALL Gravity()

        CALL FFGA()

        CALL FFVF()

        CALL FFSF()
        
        CALL Wall()

        Outer_loop: DO I=1,NAN

           I3=I+I+I 

           IF (NNG(I).gt.MAXNNB) THEN
              print *, ' TOO MANY NEIGHBOURS: ',NNG(I),' > ', maxnnb
              STOP
           ENDIF
        
           ktypei=KTYPE(I)
        
           Inner_loop: DO K=1,NNG(I)

                J=NNNG(K,I)
        
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

                IJ = IJINDEX(KTYPEI, KTYPE(J))

                RR=DX*DX+DY*DY+DZ*DZ
                RD=SQRT(RR)

                RD=(RAD(I)+RAD(J))-RD

                IF(RD.LE.0) CYCLE inner_loop

                CALL ANG(XD(1:3,I),XD(1:3,J),RD)

                FIJ = TAN(DEG)

                FI = FIJ*F(I3-2) + TSI(IJ)*RD/3
                FJ = FIJ*F(I3-1) + TSI(IJ)*RD/3
                FK = FIJ*F(I3) + TSI(IJ)*RD/3

                FI = FI/RD
                FJ = FJ/RD
                FK = FK/RD

                IF(F(I3-2).GE.FI) F(I3-2)=F(I3-2)-FI*DX
                IF(F(I3-1).GE.FJ) F(I3-1)=F(I3-1)-FJ*DY
                IF(F(I3).GE.FK) F(I3)=F(I3)-FK*DZ

                FI = FIJ*F(J3-2) + TSI(IJ)*RD/3
                FJ = FIJ*F(J3-1) + TSI(IJ)*RD/3
                FK = FIJ*F(J3) + TSI(IJ)*RD/3

                FI = FI/RD
                FJ = FJ/RD
                FK = FK/RD

                IF(F(J3-2).GE.FI) F(J3-2)=F(J3-2)+FI*DX
                IF(F(J3-1).GE.FJ) F(J3-1)=F(J3-1)+FJ*DY
                IF(F(J3).GE.FK) F(J3)=F(J3)+FK*DZ

           ENDDO Inner_loop

!          Estimating the volume of each particles
!          STEN is the static portion of stress tensor x particle volume
           VOL(I) = (4*PI*(RAD(I)**3))/3
           STEN(I,1:3,1:3)=STEN(I,1:3,1:3)*VOL(I)

        ENDDO Outer_loop

        RETURN
    
    END SUBROUTINE Forces