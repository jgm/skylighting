#XMLS=haskell.xml cmake.xml diff.xml javascript.xml
XMLS=abc.xml actionscript.xml ada.xml agda.xml alert.xml alert_indent.xml apache.xml asn1.xml asp.xml ats.xml awk.xml bash.xml bibtex.xml boo.xml c.xml changelog.xml clojure.xml cmake.xml coffee.xml coldfusion.xml commonlisp.xml cpp.xml cs.xml css.xml curry.xml d.xml diff.xml djangotemplate.xml dockerfile.xml dot.xml doxygen.xml doxygenlua.xml dtd.xml eiffel.xml elixir.xml email.xml erlang.xml fasm.xml fortran.xml fsharp.xml gcc.xml glsl.xml gnuassembler.xml go.xml hamlet.xml haskell.xml haxe.xml html.xml idris.xml ini.xml isocpp.xml java.xml javadoc.xml javascript.xml json.xml jsp.xml julia.xml kotlin.xml latex.xml lex.xml lilypond.xml literate-curry.xml literate-haskell.xml llvm.xml lua.xml m4.xml makefile.xml mandoc.xml markdown.xml mathematica.xml matlab.xml maxima.xml mediawiki.xml metafont.xml mips.xml modelines.xml modula-2.xml modula-3.xml monobasic.xml nasm.xml noweb.xml objectivec.xml objectivecpp.xml ocaml.xml octave.xml opencl.xml pascal.xml perl.xml php.xml pike.xml postscript.xml prolog.xml pure.xml python.xml r.xml relaxng.xml relaxngcompact.xml rest.xml rhtml.xml roff.xml ruby.xml rust.xml scala.xml scheme.xml sci.xml sed.xml sgml.xml sql-mysql.xml sql-postgresql.xml sql.xml tcl.xml tcsh.xml texinfo.xml verilog.xml vhdl.xml xml.xml xorg.xml xslt.xml xul.xml yacc.xml yaml.xml zsh.xml
# DEFS=$(wildcard xml/*.xml)
DEFS=$(patsubst %,xml/%,$(XMLS))

quick:
	stack install --test --fast

all: $(DEFS)
	-rm -rf Skylighting/Syntax
	cp Skylighting/Syntax.hs.bootstrap Skylighting/Syntax.hs
	sed -i '' -e '/Syntax\.Syntax_/d' skylighting.cabal
	stack install --test --fast
	skylighting-extract $(DEFS)
	stack install --test --fast

syntax-highlighting:
	git clone https://github.com/KDE/syntax-highlighting

update-xml: syntax-highlighting
	cp syntax-highlighting/data/syntax/*.xml xml/

.PHONY: all update-xml quick

