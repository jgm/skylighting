XMLS=$(wildcard skylighting-core/xml/*.xml)
ALL=skylighting/src/Skylighting/Syntax.hs quick

all: $(ALL)

quick:
	stack install --system-ghc --test --flag "skylighting:executable" --test-arguments '--hide-successes $(TESTARGS)'

test:
	stack test --system-ghc --test-arguments '--hide-successes $(TESTARGS)'

bench:
	stack bench --system-ghc --flag 'skylighting:executable'

format: skylighting-format skylighting-core-format

skylighting-core-format:
	stylish-haskell -i -c .stylish-haskell \
	      skylighting-core/bin/*.hs \
	      skylighting-core/test/test-skylighting.hs \
	      skylighting-core/benchmark/benchmark.hs \
	      skylighting-core/Setup.hs \
	      skylighting-core/src/Skylighting/*.hs \
	      skylighting-core/src/Skylighting/Format/*.hs

skylighting-format:
	stylish-haskell -i -c .stylish-haskell \
	      skylighting/bin/*.hs \
	      skylighting/Setup.hs \
	      skylighting/src/Skylighting.hs

bootstrap: skylighting/src/Skylighting/Syntax.hs

skylighting/src/Skylighting/Syntax.hs: $(XMLS)
	stack install --system-ghc --flag "skylighting-core:executable" skylighting-core
	-rm -rf skylighting/src/Skylighting/Syntax skylighting/src/Skylighting/Syntax.hs
	cd skylighting && skylighting-extract ../skylighting-core/xml/*.xml
	stack install --system-ghc --flag "skylighting:executable" --test --test-arguments \
	    '--hide-successes $(TESTARGS)'

syntax-highlighting:
	git clone https://github.com/KDE/syntax-highlighting

update-xml: syntax-highlighting
	cd syntax-highlighting; \
	git pull; \
	cd ../skylighting-core/xml; \
	for x in *.xml; do cp ../../syntax-highlighting/data/syntax/$$x ./; done ; \
	for x in *.xml.patch; do patch < $$x; done

clean:
	stack clean

.PHONY: all update-xml quick clean test format

