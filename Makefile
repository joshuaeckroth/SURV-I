
GHCFLAGS = -O -auto-all -isrc/abducer -outputdir build
CVLDPATH = LD_LIBRARY_PATH=/home/josh/bin/installations/opencv/lib
ABDUCER_SRCS = $(wildcard src/abducer/*.hs) $(wildcard src/abducer/*/*.hs)

all: gui abducer api

gui: Makefile.gui
	make -f Makefile.gui

Makefile.gui: surv-1.pro
	qmake -o Makefile.gui surv-1.pro

abducer:
	mkdir -p build/WrappedInts
	mkdir -p build/Reasoner
	ghc $(GHCFLAGS) -o build/abducer --make src/abducer/abducer.hs 

runsurv:
	$(CVLDPATH) build/runsurv

debug:
	$(CVLDPATH) gdb --args build/runsurv

clean:
	rm -Rvf build/*

.PHONY : api
api:
	for file in $(SRCS); \
		do mkdir -p api/`dirname $$file`; \
		HsColour -html -anchor $$file > api/`dirname $$file`/`basename $$file .hs`.html; \
	done

	haddock -o api -h --title="SURV-I" \
		--source-module="file:///home/josh/research/surv-1/api/src/%{MODULE/.//}.html" \
		--source-entity="file:///home/josh/research/surv-1/api/src/%{MODULE/.//}.html#%N" \
		$(ABDUCER_SRCS)
