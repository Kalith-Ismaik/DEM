!THIS SUBROUTINE SETS THE INITIAL PARAMETERS FOR DEM SIMULATION.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE SetInit()
        INCLUDE 'common.h'

!       Number of types of particle pairs
        NPOTS=NTYPE*(NTYPE+1)/2 
  
!       Counting particles of different types
        KTT(1:NTYPE)=0
        DO I=1,NAN
           DO J=1,NTYPE
              IF(KTYPE(I).EQ.J) KTT(J)=KTT(J)+1
              IF(KTYPE(I).EQ.J) XMASS(J)=ZMASS(I)
              IF(KTYPE(I).EQ.J) SKK(J)=SK(I)
              IF(KTYPE(I).EQ.J) VVS(J)=VS(I)
              IF(KTYPE(I).EQ.J) TTS(J)=TS(I)
           ENDDO
        ENDDO

        DIAM(1:NAN) = RAD(1:NAN) * 2
        
        KTOT=0
        DO J=1,NTYPE
           KTOT=KTOT+KTT(J)
        ENDDO
        IF(KTOT.NE.NAN) Then
           Print *,'KTYPE(I), NTYPE, and NAN do not match, Program stops' 
           STOP
        ENDIF

!       Define the real geometry of the system for correct pressure definition
        XXMIN = XD(1,1)
        XXMAX = XXMIN
        YYMIN = XD(2,1)
        YYMAX = YYMIN
        ZZMIN = XD(3,1)
        ZZMAX = ZZMIN
        DO I=2,NAN 
           IF(XXMIN.GE.XD(1,I)) XXMIN = XD(1,I)
           IF(XXMAX.LE.XD(1,I)) XXMAX = XD(1,I)
           IF(YYMIN.GE.XD(2,I)) YYMIN = XD(2,I)
           IF(YYMAX.LE.XD(2,I)) YYMAX = XD(2,I)
           IF(ZZMIN.GE.XD(3,I)) ZZMIN = XD(3,I)
           IF(ZZMAX.LE.XD(3,I)) ZZMAX = XD(3,I)
        ENDDO
        IF(LIDZ.EQ.0) THEN
           ZLREAL = ZZMAX - ZZMIN
        ELSE
           ZLREAL = ZL
        ENDIF
        IF(LIDX.EQ.0) THEN
           XLREAL = XXMAX - XXMIN
        ELSE
           XLREAL = XL
        ENDIF
        IF(LIDY.EQ.0) THEN
           YLREAL = YYMAX - YYMIN
        ELSE
           YLREAL = YL
        ENDIF

!       Define the dimensions of the system.
        NO1=NAN-1
        NAN3=NAN+NAN+NAN
        XLHALF=XL*0.5d0
        YLHALF=YL*0.5d0
        ZLHALF=ZL*0.5d0

!       DXR(I) is used to pick a value of force or energy that corresponds 
!       to a given interparticle distance from the table:
        DO I=1,NPOTS
           DXR(I)=RM(I)
           RLIST(I) = RM(I)+RSKIN
           RLIST2(I) = RLIST(I)*RLIST(I)
        ENDDO

!       Let's assume that we have up to 3 types of atoms
!       interacting via up to 6 pair potentials
        IJINDEX(1,1)=1
        IJINDEX(1,2)=3
        IJINDEX(1,3)=6
        IJINDEX(2,1)=3
        IJINDEX(2,2)=2
        IJINDEX(2,3)=5
        IJINDEX(3,1)=6
        IJINDEX(3,2)=5
        IJINDEX(3,3)=4

!       Assumption of tensile strength,viscousity and size of material complex
        DO I=1,NTYPE
           DO J=1,NTYPE
               IJ = IJINDEX(I,J)
               TSI(IJ) = 1/TTS(I) + 1/TTS(J)
               VSI(IJ) = 1/VVS(I) + 1/VVS(J)
               SKI(IJ) = 1/SKK(I) + 1/SKK(J)
           END DO
           SZINDEX(I,1)=XMASS(I)/2
           SZINDEX(I,2)=XMASS(I)*2
        END DO

        DIST(MAXNNB,NAN) = 0.0

!       Defining boundary and stochastic layers
        NRIGID=0
        Nstoch=0 
        DO I=1,NAN
           VW(I)=0.0d0
           KRIGID(I)=0
           IF(KHIST(I).EQ.3) KRIGID(I)=KBOUND
           IF(KHIST(I).EQ.3) NRIGID=NRIGID+1
           IF(KHIST(I).EQ.3.AND.KBOUND.EQ.1) Q1D(1:3,I) = 0.0d0
           IF(KHIST(I).EQ.2) Nstoch=Nstoch+1
        ENDDO

!       It is convenient to multiply velocities by timestep DELTA
!       when a predictor-corrector algorithm is used.
        DO I=1,NAN3
           Q1(I)=Q1(I)*DELTA
           Q2(I)=0.0d0
           Q3(I)=0.0d0
           Q4(I)=0.0d0
           Q5(I)=0.0d0
        ENDDO
        
        DO I=1,NAN
           R1(I)=R1(I)*DELTA
           R2(I)=0.0d0
           R3(I)=0.0d0
           R4(I)=0.0d0
           R5(I)=0.0d0
        ENDDO
        
        IF (NDIM.EQ.2) Q1D(3,1:NAN)=0.0d0
        
!       G1 is used in Nordsieck Integration method
        GMscale=32.0d0
        DO J=1,NAN
           G1(J)=0.5d0*DELTA*DELTA/ZMASS(J)
           G1R(J)=G1(J)/GMscale
        ENDDO
        
!       Nordsieck predictor-corrector coefficients
        C1=3.d0/20.d0
        C2=251.d0/360.d0
        C3=11.d0/18.d0
        C4=1.d0/6.d0
        C5=1.d0/60.d0

    END SUBROUTINE SetInit