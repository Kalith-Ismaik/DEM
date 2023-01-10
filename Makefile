# The makefile compiles (type make) and makes an executable called mse-bs

EXEPATH = .

EXE = $(EXEPATH)/dem.x

F90 = gfortran

OBJS = Main.o OpenFiles.o ReadFiles.o WriteInit.o Gather.o SetInit.o CutOff.o NbList.o Forces.o DFT.o Ang.o Wall.o Gravity.o EFtab-GA.o EFtab-VF.o F-VF.o F-GA.o F-SF.o Nord5.o Verlet.o Swrite.o SetEnd.o Temper.o Pressure.o Vel.o Bere-P.o Density.o FlushOut.o WriteEnd.o Leng.o

# compile and load
default:
	@echo " "
	@echo "Compiling Code MD"
	@echo "Version 2.0"
	@echo "FORTRAN 90"
	$(MAKE) $(EXE)

$(EXE):	$(OBJS)
	$(F90) $(F90FLAGS) $(LDFLAGS) -o $(EXE)  $(OBJS)

.SUFFIXES: .f90 .o
.f90.o:
	$(F90) $(F90FLAGS) -c $*.f90


clean:
	rm -f *.o
	rm -f dem.x