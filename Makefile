.PHONY: build
build:
	cabal new-build --enable-coverage

.PHONY: test
test:
	cabal new-test --enable-coverage adventofcode:test:adventofcode-test
	$(eval MIX=$(shell find dist-newstyle -type d -path '*dyn/mix/adventofcode*' | grep -v inplace))
	$(eval TIX=$(shell find dist-newstyle -type f -path '*adventofcode*adventofcode*.tix' | grep -v test))
	hpc report --hpcdir=$(MIX) $(TIX) \
		| sed '/^  /d' \
		| awk '{ p += $$1; } END { t = p/NR; if (t <= 70) { print t; exit 1 } else { print t " OK"; } }'

.PHONY: benchmarks
benchmarks:
	nix build '.#adventofcode:bench:criterion'
	./result/bin/criterion --output docs/benchmarks/index.html

.PHONY: coverage
coverage: test
	rm -rf docs/coverage/*
	find dist-newstyle -type d -path '*hpc/*/html/adventofcode*' \
		| grep -v test | grep -v inplace \
		| xargs -I{} cp -r {}/. docs/coverage/
	ln -s hpc_index.html docs/coverage/index.html

.PHONY: docs
docs:
	rm -rf docs/haddock/*
	cabal new-haddock
	find dist-newstyle -type d -path '*html/adventofcode' \
		| xargs -I{} cp -r {}/. docs/haddock/
