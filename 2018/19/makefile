run:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_19.hs
runtest:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_19.hs
main: puzzle
	ghc -O2 -o main_2018_19.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_19.hs
test: puzzle
	ghc -O2 -o test_2018_19.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_19.hs
puzzle:
	ghc -O2 -i/Users/eric/Dropbox/current/advent-of-code/automation/ puzzle_2018_19.hs
clean:
	rm *.o *.hi *.bin
