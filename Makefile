#  NEED BUFRLIB v11-3-0 https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php
#  and tables https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/decoders/decod_shared/fix/

# Linux gfortran
# F77     = gfortran
# F90    = gfortran -ffree-form  -ffree-line-length-none
# CC     = gcc -DUNDERSCORE
# FFLAGS = -O2 -g
# CFLAGS = -O2 -g
# LIB = -L/home/wrf/src/BUFRLIB/lib -lbufr_gcc

# Linux pgf90
F77    = pgf90
F90    = pgf90 
CC     = pgcc -DUNDERSCORE
FFLAGS = -O2 -g
CFLAGS = -O2 -g 
LDFLAGS = -Mnomain
LIB = -L/home/wrf/src/BUFRLIB/lib -lbufr

# AIX xlf90
#F77 = xlf90 -qfixed
#F90 = xlf90
#LIB = -L/sya/u/wrf/libs/BUFRLIB/lib -lbufr
#FFLAGS = -g

TARGET =  read_bufr read_h8amv_bufr read_amdar_bufr debufr

.SUFFIXES:
.SUFFIXES:	.f90 .o .c .f

default:	$(TARGET)

read_bufr:	read_bufr.f90
	$(F90) $(FFLAGS) -o $@ read_bufr.f90  $(LIB)

read_h8amv_bufr:   read_h8amv_bufr.f90
	$(F90) -o $@ $@.f90 $(LIB)

read_amdar_bufr:  read_amdar_bufr.f90
	$(F90) -o $@ $@.f90 $(LIB)

debufr:  module.o fdebufr.o debufr.o
	$(F90) $(LDFLAGS) -o debufr module.o debufr.o fdebufr.o $(LIB)

clean:
	rm -f $(TARGET)
	rm -f *.o *.mod
.f90.o:
	$(F90) $(FFLAGS) -c $<
.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(F77) $(FFLAGS) -c $<

