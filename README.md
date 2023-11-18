# aoc-hs [![Build Status](https://github.com/tylerjl/aoc-hs/actions/workflows/main.yml/badge.svg)](https://github.com/tylerjl/aoc-hs/actions)

Solutions to Advent of Code exercises.

## Setup

I'm using [nix flakes][flakes] to handle dependencies now, so install the requisite software to make sure that `nix flake` works for you.
Optionally, I recommend [direnv][] as well to make the process even easier.

Clone this repository, enter the directory, and

```console
$ direnv allow
```

Or, if you like living as a caveman:

```console
$ nix develop
```

You'll have a bit of setup happen.
Once that's done, the dev environment should be functional with all dependencies present.

[direnv]: https://github.com/direnv/direnv
[flakes]: https://nixos.wiki/wiki/Flakes#Flake_schema

## Usage

Run the test suite to confirm the code conforms to the problem set examples:

    make test

If everything looks good, go ahead and either install the solver binary and run it against problem input (in my case, I've committed my problem input, so I can do this):

    cabal run adventofcode -- 2015 1 dist/resources/2015/day1
