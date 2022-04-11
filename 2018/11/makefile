run:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_11.hs
runtest:
	runghc -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_11.hs
main: puzzle
	ghc -funbox-strict-fields -o main_2018_11.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ main_2018_11.hs
test: puzzle
	ghc -funbox-strict-fields -o test_2018_11.bin -i/Users/eric/Dropbox/current/advent-of-code/automation/ test_2018_11.hs
puzzle:
	ghc -funbox-strict-fields -i/Users/eric/Dropbox/current/advent-of-code/automation/ puzzle_2018_11.hs
clean:
	rm *.o *.hi *.bin
