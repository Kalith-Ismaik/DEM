      Parameter (LPMX = 10000)           !Maximum number of particles
      
      Parameter (LPMX3 = LPMX*3)

      Parameter (KTMX = 3)               !Maximum number of particle types
 
      Parameter (KTMX2 = KTMX*2)

      Parameter (KPMX = KTMX*(KTMX+1)/2) !Maximum number of potential types

      Parameter (MAXNNB = 400)           !Maximum number of neighbour pairs

      Parameter (ID   = 3)               !Dimension of system

      Parameter (NT = 2000)              !Points in the energy & force tables

      Parameter (XMDR = 0.5)              !Maximum deformation ratio

!---- Fundamental constants ----------------------------------------------

      Parameter (GA = 9.80665)           !Std. acc. due to gravity in earth (unit = m*(s**(-2)))

      Parameter (GB = 6.6743D-11)        !Gravitational Constant (unit = N*(m**2)*(kg**(-2))

      Parameter (PI = 3.1415926535897932D+00) !PI

      Parameter (BK = 8.617385D-05)       !Boltzman constant, eV/K

!---- Transfer to program units ------------------------------------------
      
      Parameter (GTOPICG = 1D+12)         !Conversion factor from gram to picogram

      Parameter (GKTOPICG = 1D+15)        !Conversion factor from kilogram to picogram

      Parameter (OMTOMICM = 1D+06)        !Conversion factor from meter to micron

      Parameter (STOMS = 1D+03 )          !Conversion factor from sec to milli.sec

!--------------------------------------------------------------------------