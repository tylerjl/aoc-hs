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
coverage:
	rm -rf docs/coverage/*
	stack hpc report \
		$$(stack path --local-install-root)/hpc/adventofcode/adventofcode-test/adventofcode-test.tix \
		--destdir docs/coverage
	ln -s hpc_index.html docs/coverage/index.html

.PHONY: docs
docs:
	rm -rf docs/haddock/*
	cabal new-haddock
	find dist-newstyle -type d -path '*html/adventofcode' \
		| xargs -I{} cp -r {}/. docs/haddock/
