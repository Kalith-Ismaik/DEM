!THIS SUBROUTINE IS EXPRESSING THE PRESSURE IMPARTING ON THE PARTICLES IN THE SYSTEM.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, DECEMBER-2022, NUS SINGAPORE.

    SUBROUTINE Pressure(TPRESS)
    
        INCLUDE 'common.h'

        DIMENSION AtPress(LPMX),VAtPress(LPMX),TAtPress(LPMX)

        SPress=0.0d0
        VPress=0.0d0

!       Calculating virial portion first
!       Min Zhou claims that only this part makes sense (Cauchy stress)
!       Calculate Trace to obtain local particle pressure
!       The sign is oposite (should multiply by -1 if stresses are of interest)

        outer_loop: DO I=1,NAN
           AtPress(i)=(STEN(I,1,1)+STEN(I,2,2)+STEN(I,3,3))/3.0d0
           SPress=SPress+AtPress(I)
        End do outer_loop

!       Adding the velocity portion
!       This is the kinetic portion of the particle pressure [in program units]

        VAtPress(1:NAN)=0.0d0
        i_loop: DO i=1,NAN
           m_loop: DO M=1,3
              VAtPress(i)=VAtPress(i)+XMASS(KTYPE(I))*Q1D(M,I)*Q1D(M,I)/DELTA/DELTA/3.0d0
           END DO m_loop
           VPress=VPress+VAtPress(I)
        END DO i_loop

!       Convert Units [Pr.En.units/Ang^3] --> [J/m^3] == [Pa]
        SPress=SPress/(XLREAL*YLREAL*ZLREAL*1.0d-18)
        VPress=VPress/(XLREAL*YLREAL*ZLREAL*1.0d-18)
        TPress=SPress+VPress

!       The following approach to pressure calculation
!       gives the same result as the above one for free BC,
!       but does not work for periodic BC.
!       Calculating virial portion first

        If(.FALSE.) Then

           SPress2=0.0d0
           
           DO I=1,NAN
              SPress2=SPress2+(FD(1,I)*XD(1,I)+FD(2,I)*XD(2,I)+FD(3,I)*XD(3,I))/3.0d0
           ENDDO

!          Convert Units [Pr.En.units/Ang^3] --> [J/m^3] == [Pa]
           SPress2=SPress2/(XLREAL*YLREAL*ZLREAL*1.0d-18)

!          Adding Ideal Portion (QINT is already in eV)
           VPress2=QINT*2.0d0/NDIM/(XLREAL*YLREAL*ZLREAL*1.0d-18)
           TPress2=SPress2+VPress2
           
           print '(3(A,E11.4,A,X))', & 
                "SPress2=",SPress2,"Pa","VPress2=",VPress2,"Pa","TPress2=",TPress2,"Pa"
        EndIf

        RETURN
    
    END SUBROUTINE Pressure