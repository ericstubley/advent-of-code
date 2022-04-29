# advent-of-code
My repo for Advent of Code solutions: https://adventofcode.com

Nothing special about these solutions. I expect that most of them will not be prettified once I get the answer, so read at your own risk. If for some reason anyone ever looks at this and wants to chat about the solutions, don't hesitate to reach out! ericdavidstubley@gmail.com is the best place to reach me.

I'm trying to learn or focus on a specific thing each year of problems that I do:

2021: (Dec 2021) 
- first Advent of Code I participated in
- solved in python
- mostly stuck to the soft goals of having solutions run in under 1 second and not submitting more than a single wrong answer per problem
- expand your repertoire of vi commands for efficiency
- overall goal was to just regain fluency with coding

2015: (Jan 2022)
- plan to solve using python
- keep the < 1s runtime and no more than a single wrong answer goals
- keep paying attention to efficient vi usage
- goal: write pythonically
- goal: develop some good testing habits; write tests before writing big chunks of code
- goal: use a proper debugger rather than print statements

2016: (Feb 2022)
- plan to solve using python; next month try for haskell
- last month's goals now feel automatic
- goal: build up the knowledge database rather than constant googling
- goal: build up a library of reusable code, rather than re-writing the file parsing every day
- goal: spend a bit of time cleaning up the solution every day before committing

2017: (Mar 2022)
- plan to solve using python; language strategy at this point is to stick with python to maintain/increase fluency while interviewing. After you get a job think about a functional statically typed language
- debugger has fallen out of use; bad on one hand, but don't feel too bad about it since you haven't needed anything serious since your bugs have been straightforward thanks to testing
- project: sit down and set up some automated puzzle/input fetching and submitting; this project will be great for expanding your coding horizons
- goal: don't be afraid of refactoring
- goal: avoid code duplication (started doing this towards end of 2016)
- goal: avoid googling, just code
- keep up many other goals (runtime, few mistakes, testing)
- experiment with new approaches (i.e. the time you passed functions rather than strings around)

2018: (Apr 2022)
- learning new things from doing these in python has plateaued; with a few minutes thinking you basically come up with the fully formed solution
- you made automation happen for 2017 in shorter order than you though it would take
- given all that: let's start working in Haskell, including setting up a testing and automation framework from the start
- regular goals: correct answer first try, essentially instant runtimes
- over than that, focus on reading about Haskell and learning new concepts from other peoples solutions (reddit and some repos to look at)

2019: (May 2022)
- Haskell is the language of choice for this year!
- Focus this year on building reusable tools. This is the year which has an intcode computer, which should definitely be built into its own library. Parsing deserves its own library, with an actual system other than pure thought for detecting and correcting parsing errors. 2d grids are another area where you should built out a library of useful tools.
- Also focus on project management. Your 2018 system was very ad hoc, and it would be nice for example to have a consistent sensible set of automatic imports.
- When you need lenses, use the microlens library; it's much easier and has better docs than the full lens library.
- Pay attention to not googling; workflow should be to try and remember first, then try to puzzle it out with ghci, then google. This loop shouldn't take more than a minute, but a bit of effort will go a long way to increasing recall.