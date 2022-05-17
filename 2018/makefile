run:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_XX.hs
runtest:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_XX.hs
main: puzzle
	ghc -o main_2018_XX.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_XX.hs
test: puzzle
	ghc -o test_2018_XX.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_XX.hs
puzzle:
	ghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ puzzle_2018_XX.hs
clean:
	rm *.o *.hi *.bin
