
LDFLAGS = -L/usr/X11R6/lib -lX11 -lm
CFLAGS = -g -Wall -ansi
GHCFLAGS = -O -prof -auto-all
HMAKEFLAGS = -package HaXml -package containers -package array -package bytestring -dbuild -isrc
SRCS = $(wildcard src/*.hs) $(wildcard src/*/*.hs) \

all: decoder/decoder player/player abducer api

decoder/decoder: decoder/decoder.c
	$(CC) $(CFLAGS) decoder/decoder.c $(LDFLAGS) -o $@

player/player: player/player.cpp
	$(CXX) $(CFLAGS) -Iplayer/xmlsp-1.0 player/player.cpp player/xmlsp-1.0/xmlsp.cpp player/xmlsp-1.0/xmlsp_dom.cpp \
		$(LDFLAGS) -o $@

abducer:
	hmake $(HMAKEFLAGS) $(GHCFLAGS) abducer

decode-videos:
	decoder/decoder 0 ../videos/plse1.ppm > acquisitions-camera-0.xml
	decoder/decoder 1 ../videos/plsw1-6fps.ppm > acquisitions-camera-1.xml

test-tiny:
	build/abducer tmp.xml | xmllint --format -

test-tiny-prof:
	build/abducer +RTS -p -RTS tmp.xml | xmllint --format -

test-small:
	build/abducer tmp2.xml | xmllint --format -

test-small-prof:
	build/abducer +RTS -p -RTS tmp2.xml | xmllint --format -

test-large:
	build/abducer acquisitions.xml | xmllint --format -

test-large-prof:
	build/abducer +RTS -p -RTS acquisitions.xml | xmllint --format -

test-visual:
	build/abducer acquisitions-camera-0.xml acquisitions-camera-1.xml | player/player ../videos/plse1.ppm ../videos/plsw1-6fps.ppm

clean:
	rm -Rvf decoder/*.o decoder/decoder player/*.o player/player
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
		$(SRCS) \
		#&> /dev/null
