#XMLS=haskell.xml cmake.xml diff.xml hamlet.xml alert.xml modelines.xml c.xml doxygen.xml
XMLS=$(wildcard xml/*.xml)

quick:
	stack install --test --flag "skylighting:executable" --test-arguments '--hide-successes $(TESTARGS)'

test:
	stack test --test-arguments '--hide-successes'

bench:
	stack bench --flag 'skylighting:executable'

format:
	stylish-haskell -i -c .stylish-haskell \
	      bin/*.hs test/test-skylighting.hs \
	      src/Skylighting/*.hs src/Skylighting/Format/*.hs src/Skylighting.hs

bootstrap: $(XMLS)
	-rm -rf src/Skylighting/Syntax
	stack install --flag 'skylighting:bootstrap'
	skylighting-extract $(XMLS)
	stack install --test --flag 'skylighting:executable'

syntax-highlighting:
	git clone https://github.com/KDE/syntax-highlighting

update-xml: syntax-highlighting
	cd syntax-highlighting; \
	git pull; \
	cd ../xml; \
	for x in *.xml; do cp ../syntax-highlighting/data/syntax/$$x ./; done ; \
	for x in *.xml.patch; do patch < $$x; done

clean:
	stack clean

.PHONY: all update-xml quick clean test format

