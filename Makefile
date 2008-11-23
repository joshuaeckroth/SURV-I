
LDFLAGS = -L/usr/X11R6/lib -lX11 -lm
CFLAGS = -g -Wall -ansi
GHCFLAGS = #-prof -auto-all
HMAKEFLAGS = -package HaXml -package containers -package gtk -package array -dbuild -isrc
SRCS = $(wildcard src/*.hs) $(wildcard src/*/*.hs) \

all: decode player/player abducer api

decode: decode.c
	$(CC) $(CFLAGS) decode.c $(LDFLAGS) -o $@

player/player: player/player.cpp
	$(CXX) $(CFLAGS) -Iplayer/xmlsp-1.0 player/player.cpp player/xmlsp-1.0/xmlsp.cpp player/xmlsp-1.0/xmlsp_dom.cpp \
		$(LDFLAGS) -o $@

abducer:
	hmake $(HMAKEFLAGS) $(GHCFLAGS) abducer

clean:
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
		$(SRCS)
