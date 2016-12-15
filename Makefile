DEFS=$(wildcard xml/*.xml)

quick:
	stack install --test --fast

all:
	cp Skylighting/Syntax.hs.bootstrap Skylighting/Syntax.hs
	stack install --test --fast
	skylighting-extract $(DEFS) > Skylighting/Syntax.hs
	stack install --test --fast

syntax-highlighting:
	git clone https://github.com/KDE/syntax-highlighting

update-xml: syntax-highlighting
	cp syntax-highlighting/data/syntax/*.xml xml/

.PHONY: all update-xml quick

