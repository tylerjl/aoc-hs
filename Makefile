.PHONY: docs
docs:
	rm -rf docs/*
	cp -r $$(stack path --local-doc-root)/* docs/
