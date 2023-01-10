!THIS SUBROUTINE IS EXPRESSING THE DENSITY OF THE PARTICLES IN THE SYSTEM.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE Density()

        INCLUDE 'common.h'

        DENSNIN(1:NTYPE) = 0.0d0
        DENSMIN(1:NTYPE) = 0.0d0

!       Density calculation
        
        VOLUME=(XLREAL*1.0D-08)*(YLREAL*1.0D-08)*(ZLREAL*1.0D-08) !In cm3
        
        TMASS=0.0d0
        PMASS=0.0d0
        
        DO J=1,NTYPE

           TMASS=TMASS+KTT(J)*XMASS(J)       ! Total mass in picogram

           PMASS=PMASS+KTT(J)*XMASS(J)       ! Mass of total particle 'i' in picogram
           DENSMIN(J)=PMASS*1.0D+03/VOLUME      ! gr/cm3 
           DENSNIN(J)=KTT(J)/VOLUME             ! molecules/cm3

        ENDDO

        DENSM=TMASS*1.0D+03/VOLUME           ! gr/cm3
        DENSN=NAN/VOLUME                     ! molecules/cm3

        RETURN
       
    END SUBROUTINE Density