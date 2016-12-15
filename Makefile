DEFS=$(wildcard xml/*.xml)

all: Skylighting/Syntax.hs
	stack install --test --fast

syntax-highlighting:
	git clone https://github.com/KDE/syntax-highlighting

update-xml: syntax-highlighting
	cp syntax-highlighting/data/syntax/*.xml xml/

Skylighting/Syntax.hs: $(DEFS)
	skylighting-extract $^ > $@; \

.PHONY: all update-xml

