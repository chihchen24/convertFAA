# Makefile for H3Main (Level 1 to 2 processor)

# These are set in the run_make script now
FFLAGS = $(ForFLAGS)

LINKFLAGS = -O2 

INCS = -I$(netCDFInc)

OBJS = $(OBJ)

MAIN = $(SOURCEPATH)

$(GOAL): $(OBJS) $(LIB_FILES)
	$(FC) -o $(GOAL) $(LINKFLAGS) $(OBJS) $(LIBS)

CreateFAAData.o: $(MAIN)/CreateFAAData.F90 handleErr.o convertFAA.o FAARoutines.o
	$(FC) $(FFLAGS) -I$(MAIN) $(MAIN)/CreateFAAData.F90 $(INCS)

handleErr.o: $(MAIN)/handleErr.F90
	$(FC) $(FFLAGS) $(MAIN)/handleErr.F90 $(INCS)

convertFAA.o: $(MAIN)/convertFAA.F90 FAARoutines.o
	$(FC) $(FFLAGS) $(MAIN)/convertFAA.F90 $(INCS)

FAARoutines.o: $(MAIN)/FAARoutines.F90 handleErr.o
	$(FC) $(FFLAGS) $(MAIN)/FAARoutines.F90 $(INCS)
