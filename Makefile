.PHONY: build
build:
	cabal new-build --enable-coverage

.PHONY: test
test:
	cabal new-test --enable-coverage adventofcode:test:adventofcode-test

.PHONY: benchmarks
benchmarks:
	cabal bench --enable-coverage \
		--benchmark-options '--output docs/benchmarks/index.html' \
		&& xdg-open docs/benchmarks/index.html

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
