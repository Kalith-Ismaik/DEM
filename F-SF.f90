!THIS SUBROUTINE IS ESTIMATING THE ENERGY AND FORCE BETWEEN TWO PARTICLES.
!DUE TO INTERPARTICLE INTERACTION IS DEFINED BY HOOKES LAW.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.
 
   SUBROUTINE FFSF()
      INCLUDE 'common.h'
 
      REAL*8 :: LN,KN,LNI,KNI,MIJ,RIJ

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

            DX=XD(1,J)-XD(1,I)
            DY=XD(2,J)-XD(2,I)
            DZ=XD(3,J)-XD(3,I)

!           Periodicity *************
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
!           End of periodicity ******

            IJ = IJINDEX(KTYPEI, KTYPE(J))

            RR=DX*DX+DY*DY+DZ*DZ
            RD=SQRT(RR)

!            LIM = 0.5*XMDR*(R(I)+R(J))
!            IF(RD.GT.LIM) CYCLE inner_loop

            RD=(RAD(I)+RAD(J))-RD

            IF(RD.LE.0) CYCLE inner_loop

            VD = (DIST(K,I) - RD)/DELTA

            KN  = (1/SK(I)) + (1/SK(J))

            MIJ = 1/ZMASS(I) + 1/ZMASS(J)
            RIJ = 1/RAD(I)   + 1/RAD(J)

            TENS = (1/TS(I)) + (1/TS(J))
            TENS = TENS * RD

            LN = 1/(SN(I)*2*(SQRT(SK(I)*ZMASS(I)))) + 1/(SN(J)*2*(SQRT(SK(I)*ZMASS(J))))

            FI = KN*RD - LN*VD*MIJ
            FI = SQRT(RD*RIJ)*FI

            U  = KN*(RD**2)/2

            DIST(K,I) = RD

!           Potential energy (give half the potential to each particle)
            PENER=.5*U
            POT(I)=POT(I)+PENER
            POT(J)=POT(J)+PENER
!           End of potential energy

            P=FI/RD

            IF(FI.EQ.0.0) P=0

            DX = (RAD(I)+RAD(J)) - DX 
            DY = (RAD(I)+RAD(J)) - DY 
            DZ = (RAD(I)+RAD(J)) - DZ 

!           Static Portion of Stress Tensor
            STEN(I,1,1)=STEN(I,1,1)+P*DX*DX
            STEN(I,2,2)=STEN(I,2,2)+P*DY*DY
            STEN(I,3,3)=STEN(I,3,3)+P*DZ*DZ
              
            STEN(J,1,1)=STEN(J,1,1)-P*DX*DX
            STEN(J,2,2)=STEN(J,2,2)-P*DY*DY
            STEN(J,3,3)=STEN(J,3,3)-P*DZ*DZ

            IF(FI.LE.TENS) P=0
            FI = FI - TENS
                
            DX=P*DX                          ! x component of force
            FD(1,I)=FD(1,I)+DX
            FD(1,J)=FD(1,J)-DX
            DZ=P*DZ                          ! z component of force
            FD(3,I)=FD(3,I)+DZ
            FD(3,J)=FD(3,J)-DZ
            DY=P*DY                          ! y component of force
            FD(2,I)=FD(2,I)+DY
            FD(2,J)=FD(2,J)-DY

         END DO inner_loop
        
      END DO outer_loop
        
      IF(keySTOP) STOP

      RETURN

   END SUBROUTINE FFSF