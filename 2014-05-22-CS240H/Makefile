DESTDIR=.

MDFILE := ghc
L ?= $(MDFILE)

all: $(L).html $(L)-slides.html
.PHONY: all

$(L).html: $(L).md
	@test -f $<
	pandoc --smart -s -t html -o $@ $<
#	pandoc --data-dir=./pandoc -s -t html -o $@ $<

$(L)-slides.html: $(L).md $(wildcard ./pandoc/slidy/*)
	@test -f $<
	pandoc -V slidy-url=./pandoc/slidy \
		--self-contained --smart -s -t slidy -o $@ $<

CONFIG := $(dir $(PWD))config
$(CONFIG):
	mkdir -p $(CONFIG)
CONFIGENV = XDG_CONFIG_HOME="$(CONFIG)" \
		XDG_DATA_HOME="$(CONFIG)/share" \
		HOME="$(CONFIG)"

show: $(L)-slides.html $(CONFIG)
	export $(CONFIGENV); \
	if command -v xdg-open 2> /dev/null; then \
		xdg-open $< & \
	elif command -v open 2> /dev/null; then \
		open $< & \
	else \
		firefox $< & \
	fi
.PHONY: show

chromium:
	mkdir -p "$(CONFIG)/share/applications"
	$(CONFIGENV) xdg-mime default chromium.desktop \
		text/html application/xml
.PHONY: chromium

INSTALL_FILES := $(L).md $(L).html $(L)-slides.html \
	$(wildcard *.svg) $(EXTRA_INSTALL)
install: $(L).md $(L).html $(L)-slides.html
	cp $(INSTALL_FILES) $(DESTDIR)/
	-git add $(patsubst %, $(DESTDIR)/%, $(INSTALL_FILES))
.PHONY: install

clean:
	rm -f $(L).html $(L)-slides.html *.o *.hi *~ \
		$(patsubst %.hs,%,$(wildcard *.hs))
.PHONY: clean
