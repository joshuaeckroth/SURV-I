
LDFLAGS = -L/usr/X11R6/lib -lX11 -lm
CFLAGS = -g -Wall -ansi
GHCFLAGS = -fglasgow-exts #-XScopedTypeVariables

#all: decode play_events
all: decode abducer

decode: decode.c
	$(CC) $(CFLAGS) decode.c $(LDFLAGS) -o $@

abducer: abducer.hs AcquisitionTypes.hs
	ghc -package HaXml $(GHCFLAGS) --make abducer.hs

#play_events: play_events.cpp xmlsp.cpp
#	g++ $(CFLAGS) play_events.cpp xmlsp.cpp $(LDFLAGS) -o $@

