      IMPLICIT REAL*8(A-H,O-Z)
      character*14 drname
      LOGICAL FullList
      INCLUDE 'parameters.h'

!     Coordinates and velocities
      COMMON/XXX/ X(LPMX3),Q1(LPMX3),F(LPMX3),XO1(LPMX3),XO2(LPMX3)
      COMMON/RRR/ R(LPMX),R1(LPMX)
 
!     Forces
      COMMON/FFF/ FG(LPMX),FO(LPMX3),FO1(LPMX3)

!     Variables used to define names of input/output files
      COMMON/dr1/ LDR
      COMMON/dr2/ DRNAME

!     Coordinates of the center of the computational cell
      COMMON/CENTR/ XCENTR,YCENTR,ZCENTR

!     Characteristics of the particles
      COMMON/KTP/ KTYPE(LPMX),KRIGID(LPMX3),KHIST(LPMX),KTT(KTMX)

!     Diameter(micron) and Masses(picogram) of the particles
      COMMON/MSS/ DIAM(LPMX),RAD(LPMX),ZMASS(LPMX),XMASS(KTMX2),G1(LPMX),G1R(LPMX),GMscale

!     Volume of the particles
      COMMON/VLL/ VOL(LPMX)

!     Density of the particles
      COMMON/DEN/ DENSN,DENSM,DENSNIN(KTMX),DENSMIN(KTMX),DAV(LPMX),TDEF(LPMX)
 
!     Angle of friction
      COMMON/ADEG/ DEG
      
!     Spring and Damping constant parameters of the particles
      COMMON/NKLN/ SK(LPMX),SN(LPMX),TS(LPMX),VS(LPMX),SKK(KTMX),TTS(KTMX),VVS(KTMX)
 
!     Index that defines type of the pair potential for two particles
      COMMON/IND/ IJINDEX(KTMX,KTMX)

!     Index that defines the pair potential for two particles
      COMMON/VISC/ VF(KPMX),TF(KPMX),VSI(KPMX),TSI(KPMX),SKI(KPMX)

!     Index that defines type of the minima and maxima of particle radius
      COMMON/SZS/ SZINDEX(KTMX,2)
!     Sizes of the computational cell
      COMMON/CELL/ XL,YL,ZL,XLHALF,YLHALF,ZLHALF,XLREAL,YLREAL,ZLREAL

!     Neighbor lists arrays
      COMMON/NBR/ NNG(LPMX),NNNG(MAXNNB,LPMX),DIST(MAXNNB,LPMX)

!     It is convenient to have coordinates, velocities, and forces
!     in both two and one dimensional arrays
      DIMENSION XD(3,LPMX), Q1D(3,LPMX), Q2D(3,LPMX), Q3D(3,LPMX), Q4D(3,LPMX), & 
      Q5D(3,LPMX), QAD(3,LPMX), FD(3,LPMX)
      EQUIVALENCE (X(1),XD(1,1)), (Q2(1),Q2D(1,1)), (Q3(1),Q3D(1,1)), &
      (Q4(1),Q4D(1,1)), (Q5(1),Q5D(1,1)), (QA(1),QAD(1,1)),(F(1),FD(1,1))

!     Input parameters
      COMMON/NNN/ NAN,NAN3,NO1,NSTEP,NEWTAB,NEPRT,NWRITE,NPER,NTYPE, &
      KFLAG,LFLAG,KEYBS,LIDX,LIDY,LIDZ,KBOUND,NPOTS,NRIGID,Nstoch
      
      COMMON/DDD/ QTEM,QPRESS,DELTA,RSKIN,TIME

!     Cutoff distances for pair potentials and neighbor lists
      COMMON/CUT/ DXR(KPMX),RM(KPMX),RLIST(KPMX),RLIST2(KPMX)

!     Force and Energy tables for pair potentials
      COMMON/TTT/ FT(NT,NT)

!     Energies per atom (total, potential, and kinetic)
      COMMON/ENY/ EN(LPMX),POT(LPMX),QIN(LPMX),VW(LPMX)

!     Pressure and stress
      COMMON/PRST/ STEN(LPMX,3,3)

!     Higher time derivatives of the coordinates and parameters for
!     Nordsieck integrator
      COMMON/NORD/ Q2(LPMX3),Q3(LPMX3),Q4(LPMX3),Q5(LPMX3),QA(LPMX3), &
      R2(LPMX),R3(LPMX),R4(LPMX),R5(LPMX),C1,C2,C3,C4,C5