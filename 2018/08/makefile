run:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_08.hs
runtest:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_08.hs
main: puzzle
	ghc -o main_2018_08.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_08.hs
test: puzzle
	ghc -o test_2018_08.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_08.hs
puzzle:
	ghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ puzzle_2018_08.hs
clean:
	rm *.o *.hi *.bin
