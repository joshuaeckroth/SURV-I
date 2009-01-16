
LDFLAGS = -L/usr/X11R6/lib -lX11 -lm
CFLAGS = -g -Wall -ansi
GHCFLAGS = -O -prof -auto-all
HMAKEFLAGS = -package HaXml -package containers -package array -package bytestring -dbuild -isrc
CVCFLAGS = -lcv -lcvaux -lml -lhighgui -lcxcore -L`pwd`/libs/opencv/lib -I`pwd`/libs/opencv/include/opencv
CVLDPATH = LD_LIBRARY_PATH=`pwd`/libs/opencv/lib
SRCS = $(wildcard src/*.hs) $(wildcard src/*/*.hs)

all: decoder2/calibrate decoder2/decoder player/player abducer api

decoder/decoder: decoder/decoder.c
	$(CC) $(CFLAGS) decoder/decoder.c $(LDFLAGS) -o $@

decoder2/calibrate: decoder2/calibrate.cpp
	$(CXX) $(CFLAGS) $(CVCFLAGS) -o decoder2/calibrate decoder2/calibrate.cpp

decoder2/calibrate3d: decoder2/calibrate3d.cpp
	$(CXX) $(CFLAGS) $(CVCFLAGS) -o decoder2/calibrate3d decoder2/calibrate3d.cpp

decoder2/decoder: decoder2/decoder.cpp
	$(CXX) $(CFLAGS) $(CVCFLAGS) -o decoder2/decoder decoder2/decoder.cpp

player/player: player/player.cpp
	$(CXX) $(CFLAGS) -Iplayer/xmlsp-1.0 player/player.cpp player/xmlsp-1.0/xmlsp.cpp player/xmlsp-1.0/xmlsp_dom.cpp \
		$(LDFLAGS) -o $@

abducer:
	hmake $(HMAKEFLAGS) $(GHCFLAGS) abducer

calibrate: decoder2/calibrate
	echo "camera-east (5 images)"
	$(CVLDPATH) decoder2/calibrate 5 7 0 "camera-0"
	echo "camera-west (5 images)"
	$(CVLDPATH) decoder2/calibrate 5 7 1 "camera-1"

calibrate3d: decoder2/calibrate3d
	$(CVLDPATH) decoder2/calibrate3d 5 7 2

decode-videos:
	$(CVLDPATH) decoder2/decoder ../videos/plse1.avi "camera-east" ../videos/plsw1-6fps.avi "camera-west" 6 > acquisitions.xml

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
	build/abducer acquisitions.xml | player/player ../videos/plsw1-6fps.ppm ../videos/plse1.ppm

gui:
	ghc -package wx -o gui src/GUI.hs

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
