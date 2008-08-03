EMACS=emacs
GZIP=gzip
ALLSOURCE=$(wildcard *.el)
ALLCOMPILED=$(wildcard *.elc)
SPECIAL=
SOURCE=$(filter-out $(SPECIAL),$(ALLSOURCE))
TARGET=$(patsubst %.el,%.elc,$(SOURCE))

VERSION=0.1

DESTDIR=
PREFIX=$(DESTDIR)/usr/local
INFODIR=$(PREFIX)/info
MAN1DIR=$(PREFIX)/share/man/man1
SITELISP=$(PREFIX)/share/emacs/site-lisp/etest

INSTALLINFO = /usr/sbin/install-info --info-dir=$(INFODIR)

.PHONY: all install deb-install clean
.PRECIOUS: %.elc %.info %.html
all: $(TARGET) etest.info

%.elc: %.el
	@$(EMACS) --eval "(add-to-list 'load-path \".\")" \
	          -q --no-site-file \
	          -batch \
		  -f batch-byte-compile $<

%.info: %.texinfo
	makeinfo --no-split $<

%.html: %.texinfo etest-style.css
	makeinfo --css-include etest-style.css --html --no-split $<

install:
	test -d $(SITELISP) || mkdir -p $(SITELISP)
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 644 $(ALLSOURCE) $(SITELISP)
	install -m 644 $(ALLCOMPILED) $(SITELISP)
	install -m 0644 etest.info $(INFODIR)/etest
	for p in $(MAN1PAGES) ; do $(GZIP) -9c $$p > $(MAN1DIR)/$$p.gz ; done
	$(INSTALLINFO) etest.info

dist:
	tar -C .. -cvzf etest-$(VERSION).tar.gz etest # fix "etest" assumption

html: etest.html

remove-info:
	$(INSTALLINFO) --remove etest.info

deb-install:
	install -m 644 $(ALLSOURCE) $(SITELISP)

ChangeLog:
	git log > $@

clean:
	-rm -f *~ *.elc etest.info etest.html
