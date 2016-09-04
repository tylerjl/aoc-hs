# adventofcode [![Build Status](https://travis-ci.org/tylerjl/adventofcode.svg?branch=master)](https://travis-ci.org/tylerjl/adventofcode)

Solutions to advent of code exercises.

## Usage

First, install [stack](http://docs.haskellstack.org/en/stable/README.html).

Run the test suite to confirm the code conforms to the problem set examples:

    stack test

If everything looks good, go ahead and either install the solver binary and run it against problem input (in my case, I've committed my problem input, so I can do this):

    stack install
    adventofcode 1 dist/resources/day1

Alternatively, you can build the source and run `Main.sh` standalone against problem set input:

    stack build
    stack runhaskell app/Main.sh 1 dist/resources/day1
