!THIS SUBROUTINE PROVIDES THE NEIGHBOURS LIST OF ALL PARTICLES IN THE SYSTEM.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.
 
    SUBROUTINE NbList()

        INCLUDE 'common.h'
 
        NNG(1:NAN)= 0          !Contains the number of neighbors of particle I
    
        OUTER_loop: DO I=1,NAN 
    
            ktypei=KTYPE(I)

            INNER_loop: DO J=1,NAN      

                IF(I.EQ.J) cycle INNER_loop

                IJ = IJINDEX(KTYPEI, KTYPE(J))

                Rij = RLIST(ij)+RAD(i)+RAD(j)
                R2ij = Rij * Rij

                DZ=XD(3,I)-XD(3,J)
                IF(LIDZ.EQ.1) THEN
                    IF(DZ.GT.ZLHALF) DZ=DZ-ZL
                    IF(DZ.LT.-ZLHALF) DZ=DZ+ZL
                ENDIF
              
                IF (ABS(DZ).LT.Rij) THEN
                    DX=XD(1,I)-XD(1,J) 
                    IF(LIDX.EQ.1) THEN
                        IF(DX.GT.XLHALF) DX=DX-XL
                        IF(DX.LT.-XLHALF) DX=DX+XL
                    ENDIF
                 
                    IF (ABS(DX).LT.Rij) THEN
                        DY=XD(2,I)-XD(2,J)  
                        IF(LIDY.EQ.1) THEN
                            IF(DY.GT.YLHALF) DY=DY-YL
                            IF(DY.LT.-YLHALF) DY=DY+YL
                        ENDIF
                    
                        IF(ABS(DY).LT.Rij) THEN

                            RR=DX*DX+DY*DY+DZ*DZ
                            RD=SQRT(RR)

                            IF (RR.LT.R2ij) THEN

!                               We've found a neighbor close enough to be on the list
                                NNG(I)=NNG(I)+1  ! increase the count of neighbors
                                NNNG(NNG(I),I)=J ! add the neighbor of atom I
                                DIST(NNG(I),I)=RD
                          
                            END IF
 
                        END IF

                    END IF
                    
                END IF

            ENDDO INNER_loop

        ENDDO OUTER_loop

        RETURN
    
    END SUBROUTINE NbList