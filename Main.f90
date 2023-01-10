!THIS PROGRAM IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    PROGRAM DEM

        INCLUDE 'common.h'

        INTEGER       :: S

        CHARACTER*8   :: openf

!       OPEN AND READ THE NAMELIST DATAFILE
        openf = "dem.rc"

        CALL OpenFiles(openf)

        CALL ReadFiles()

        CALL WriteInit()

        CALL FlushOut(14)

        CALL Gather()

        CALL SetInit()

        CALL Density(DENSM,DENSN)

        CALL CutOff()

        CALL CPU_TIME(timestart) ! For simulation time calculation

        CALL NbList()

        CALL FGA()

        CALL FVF()

!        XO1(1:NAN3) = X(1:NAN3)
!        FO1(1:NAN3) = F(1:NAN3)
!        CALL VelVer()

        QA(1:NAN3) = 0.0

        OPEN(UNIT = 100,FILE = 'energy.out')

        WRITE(14,*) ' Step   Energy      Kinetic    Potential  T-tot T-stoch  Pressure' 

        S=0
        Main_loop: DO S=1,NSTEP

            TIME=TIME+DELTA

            CALL VelVer()

!            CALL Nord5()

!           Gathering all particles to the initial cell
            IF (MOD(S,NPER).EQ.0) CALL Gather()
           
            IF (MOD(S,NEWTAB).EQ.0) Then
                CALL NbList()
            ENDIF

!           Writing data for future analysis
            IF(MOD(S,NWRITE).EQ.0) CALL Swrite()

!           Printing output information
            IF (MOD(S,NEPRT).EQ.0.OR.(S.EQ.1)) THEN
                CALL Temper(ENT,QINT,POTT,TEMPTR,TEMPST)
                CALL Pressure(TPRESS)
                WRITE(14,10) S,ENT,QINT,POTT,TEMPTR,TEMPST,TPRESS
                WRITE(100,11)TIME,ENT+ENINT,POTT+POTINT, &
                  QINT+QININT,ENT+ENINT+EOUT,EOUT,TEMPTR,TPRESS
                print *,"Time =",TIME,", Total Energy =",ENT+ENINT+EOUT
                call flush(6)
            ENDIF

!           Berendsen barostat for constant pressure simulation
            IF (LFLAG.EQ.1) THEN
               CALL Pressure(TPRESS)
               CALL Bere_P(TPRESS)
            ENDIF

        END DO Main_loop

        CALL FlushOut(14)
        CALL FlushOut(100) 

        CALL CPU_TIME(timeend) ! For simulation time calculation

        print *,"Total CPU time =",timeend-timestart

        CALL SetEnd()

        CALL Vel()
        
        CALL Gather()

        CALL WriteEnd()

10      FORMAT(I7,3(1X,E14.7),1X,F5.0,1X,F5.0,1X,E14.7)
11      FORMAT(8(1x,E14.7))

        STOP
         
    END PROGRAM DEM