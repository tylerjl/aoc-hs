.PHONY: build
build:
	stack build --coverage

.PHONY: test
test:
	stack test --coverage adventofcode:test:adventofcode-test

.PHONY: benchmarks
benchmarks:
	stack bench \
		--benchmark-arguments '--output docs/benchmarks/index.html' \
		&& open docs/benchmarks/index.html

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
	cp -r $$(stack path --local-doc-root)/adventofcode-*/* docs/haddock
