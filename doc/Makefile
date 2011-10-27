ASCIIDOC=asciidoc
TEXI2PDF=texi2pdf
MAKEINFO=makeinfo

DOC_MAN1=$(patsubst %.txt,%.1,$(wildcard *.txt))
DOC_HTML=$(patsubst %.txt,%.html,$(wildcard *.txt))

doc: docs
docs: manual manpages

manpages: $(DOC_MAN1) $(DOC_HTML)

%.1: %.txt
	mkdir -p man1
	a2x -D man1 -d manpage -f manpage -L $<

%.html: %.txt
	$(ASCIIDOC) $<

manual: galosh.pdf galosh.info

galosh.pdf: galosh.texi
	$(TEXI2PDF) $< --clean --output $@

galosh.info: galosh.texi
	$(MAKEINFO) $< --output $@
