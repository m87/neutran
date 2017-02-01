
all:
	gfortran -c ./modules/functions.f95


clean:
	rm ./modules/*.o ./modules/*.mod *.o *.mod
