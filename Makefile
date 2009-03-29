
GHCFLAGS = -O -auto-all -isrc/abducer -outputdir build
CVLDPATH = LD_LIBRARY_PATH=`pwd`/libs/opencv/lib
ABDUCER_SRCS = $(wildcard src/abducer/*.hs) $(wildcard src/abducer/*/*.hs)

all: gui abducer api

gui: Makefile.gui
	make -f Makefile.gui

Makefile.gui: surv-1.pro
	qmake -o Makefile.gui surv-1.pro

abducer:
	ghc $(GHCFLAGS) -o build/abducer --make src/abducer/abducer.hs 

runsurv:
	$(CVLDPATH) build/runsurv

debug:
	$(CVLDPATH) gdb --args build/runsurv

clean:
	make -f Makefile.gui clean
	hmake $(HMAKEFLAGS) -realclean abducer

.PHONY : api
api:
	for file in $(SRCS); \
		do mkdir -p api/`dirname $$file`; \
		HsColour -html -anchor $$file > api/`dirname $$file`/`basename $$file .hs`.html; \
	done

	haddock -o api -h --title="SURV-I" \
		--source-module="file:///home/josh/school/surveillance/surv-1/api/src/%{MODULE/.//}.html" \
		--source-entity="file:///home/josh/school/surveillance/surv-1/api/src/%{MODULE/.//}.html#%N" \
		$(ABDUCER_SRCS)
