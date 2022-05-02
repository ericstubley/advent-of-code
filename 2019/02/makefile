LIBDIR = .:/Users/eric/Dropbox/current/advent-of-code/2019/libraries/
FLAGS = -O2 -funbox-strict-fields



main: puzzle
	ghc $(FLAGS) -i$(LIBDIR) -o main_2019_02.bin main_2019_02.hs

test: puzzle
	ghc $(FLAGS) -i$(LIBDIR) -o test_2019_02.bin test_2019_02.hs

puzzle: libraries puzzle_2019_02.o

libraries: ../libraries/cookie.o ../libraries/automation.o ../libraries/parsing.o ../libraries/grid.o ../libraries/intcode.o

%.o : %.hs
	ghc -c $(FLAGS) -i$(LIBDIR) -o $@ $<

clean:
	rm *.o *.hi *.bin

libclean:
	rm ../libraries/*.o ../libraries/*.hi