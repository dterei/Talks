MDFILE := ghc
L ?= $(MDFILE)

all: $(L).html $(L)-slides.html
.PHONY: all

$(L).html: $(L).md
	@test -f $<
	pandoc -s -t html -o $@ $<

$(L)-slides.html: $(L).md $(wildcard ./slidy/*)
	@test -f $<
	pandoc -V slidy-url=./slidy \
		--self-contained -s -t slidy -o $@ $<

clean:
	rm -f $(L).html $(L)-slides.html *.o *.hi *~ \
		$(patsubst %.hs,%,$(wildcard *.hs))
.PHONY: clean

