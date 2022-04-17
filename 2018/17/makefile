run:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_17.hs
runtest:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_17.hs
main: puzzle
	ghc -O2 -o main_2018_17.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_17.hs
test: puzzle
	ghc -O2 -o test_2018_17.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_17.hs
puzzle:
	ghc -O2 -i/Users/eric/Dropbox/current/advent-of-code/automation/ puzzle_2018_17.hs
clean:
	rm *.o *.hi *.bin
