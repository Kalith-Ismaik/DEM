!THIS SUBROUTINE WRITES INITIAL INPUT PARAMETERS FOR THE SIMULATION
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

      SUBROUTINE WriteInit()
        INCLUDE 'common.h' 

        WRITE(14,15) NSTEP,DELTA,KFLAG,LFLAG,KEYBS,LIDX,LIDY, &
             LIDZ,KBOUND,QTEM,QPRESS,NEWTAB,NEPRT,NWRITE,NPER,NT,RSKIN
15      FORMAT( 6X,'  -----> DEM RUN:   '/ &
             'NSTEP=',I7,' (Number of steps);'/ &
             'DELTA=',D15.5,' (Integration time step in [psec]);',/, &  	
             'KFLAG=',I1,' (1-Quench,2-Vel,3-Heating,4-GLEQ,5-Bere_T);',/, &
             'LFLAG=',I1,' ((1-Berendsen Const. pressure);',/, &
             'KEYBS=',I1,' (0-p.p.,1-BS,2-rigid sph.,3-BS+Polymer,4-EAM);',/, &
             'LIDX=',I1,' (1-periodicity in X direct.,0-free boundaries);'/ &
             'LIDY=',I1,' (1-periodicity in Y direct.,0-free boundaries);'/ &
             'LIDZ=',I1,' (1-Z PBC,0-Z free or boundary layer);'/ &
             'KBOUND=',I2,' (0-Free,1-rigid,2-non-reflecting);'/ &
             'QTEM=',D15.5,' (Temperature used by VEL, HEATING or GLEQ,[K]);'/ &
             'QPRESS=',D15.5,' (Pressure used by Bere_P, [GPa]);'/ &
             'NEWTAB=',I3,' (Step of neighbours list renewal);'/ &
             'NEPRT=',I4,' (Step of printing output information);'/ &
             'NWRITE=',I5,' (Step of writing output information);'/ &
             'NPER=',I4,' (Step of gathering molecules to the comp. cell);'/ &
             'NT=',I4,' (Number of points in the energy & force tables);'/ &
             'RSkin=',D15.5,' (Skin depth in neighbour list calculation [A]);'/)
        
        WRITE(14,16) XL,YL,ZL,NAN,NTYPE
16      FORMAT( 6X,'  -----> MATERIAL:   '/ &
             'XL=',D11.5,' (X size of the computational cell [A]);'/ &
             'YL=',D11.5,' (Y size of the computational cell [A]);'/ &
             'ZL=',D11.5,' (Z size of the computational cell [A]);'/ &
             'NAN=',I7,' (Number of particles in the computational cell);'/ &
             'NTYPE=',I2,'(Number of particle types);'/)
        
        RETURN

      END SUBROUTINE WriteInit
