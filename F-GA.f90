!THIS SUBROUTINE IS CREATING THE TABLE OF ENERGY AND FORCE BETWEEN TWO PARTICLES.
!DUE TO INTERPARTICLE ATTRACTION CORRESPONDING TO GRAVITATIONAL FORCE.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.
 
   SUBROUTINE FFGA()
      INCLUDE 'common.h'

      LOGICAL :: keySTOP=.FALSE.

      outer_loop: DO I=1,NAN

         IF (NNG(I).GT.MAXNNB) THEN
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

!           DIST(K,I)=RD

            RD=RD-(RAD(I)+RAD(J))

            IF (RD.GE.RM(IJ)) CYCLE inner_loop

            IF (RD.LE.RM(IJ)/NT) CYCLE inner_loop

!           If your run crashes, you can try to uncomment this part
!           IF (RD.LE.RM(IJ)/NT) THEN
!              Print *, 'Molecules ',I,' and ',J,' are too close.', &
!                 'Program will stop.'
!              keySTOP=.TRUE.
!           ENDIF

            P = NT/ZMASS(I)
            Q = NT/ZMASS(J)
            IJ = P
            KF = Q

!           Potential energy (give half the potential to each particle)
            PENER=.5*(FT(IJ,KF)/RD)
            POT(I)=POT(I)+PENER
            POT(J)=POT(J)+PENER
!           End of potential energy

            P=FT(IJ,KF)/(RD**2)
            P=P/RD

!           Static Portion of Stress Tensor
            STEN(I,1,1)=STEN(I,1,1)+P*DX*DX
            STEN(I,2,2)=STEN(I,2,2)+P*DY*DY
            STEN(I,3,3)=STEN(I,3,3)+P*DZ*DZ
              
            STEN(J,1,1)=STEN(J,1,1)-P*DX*DX
            STEN(J,2,2)=STEN(J,2,2)-P*DY*DY
            STEN(J,3,3)=STEN(J,3,3)-P*DZ*DZ

            DX=P*DX                          ! x component of force
            FD(1,I)=FD(1,I)+DX
            FD(1,J)=FD(1,J)-DX
            DZ=P*DZ                          ! z component of force
            FD(3,I)=FD(3,I)+DZ
            FD(3,J)=FD(3,J)-DZ
            DY=P*DY                          ! y component of force
            FD(2,I)=FD(2,I)+DY
            FD(2,J)=FD(2,J)-DY
        
         End do inner_loop
        
      End do outer_loop
        
      IF(keySTOP) STOP

      RETURN

   END SUBROUTINE FFGA