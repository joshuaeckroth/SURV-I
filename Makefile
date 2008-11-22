
LDFLAGS = -L/usr/X11R6/lib -lX11 -lm
CFLAGS = -g -Wall -ansi
GHCFLAGS = -prof -auto-all
HMAKEFLAGS = -package HaXml -package containers -dbuild -isrc

#all: decode play_events
all: decode abducer

decode: decode.c
	$(CC) $(CFLAGS) decode.c $(LDFLAGS) -o $@

abducer:
	hmake $(HMAKEFLAGS) $(GHCFLAGS) abducer

clean:
	hmake $(HMAKEFLAGS) -realclean
