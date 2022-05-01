LIBDIR = .:/Users/eric/Dropbox/current/advent-of-code/2019/libraries/
FLAGS = -O2



main: puzzle
	ghc $(FLAGS) -i$(LIBDIR) -o main_2019_06.bin main_2019_06.hs

test: puzzle
	ghc $(FLAGS) -i$(LIBDIR) -o test_2019_06.bin test_2019_06.hs

puzzle: libraries puzzle_2019_06.o

libraries: ../libraries/cookie.o ../libraries/automation.o ../libraries/parsing.o ../libraries/grid.o

%.o : %.hs
	ghc -c $(FLAGS) -i$(LIBDIR) -o $@ $<

clean:
	rm *.o *.hi *.bin

libclean:
	rm ../libraries/*.o ../libraries/*.hi