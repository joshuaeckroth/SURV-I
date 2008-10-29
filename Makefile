
LDFLAGS = -L/usr/X11R6/lib -lX11 -lm
CFLAGS = -g -Wall -ansi

#all: decode play_events
all: decode

decode: decode.c
	$(CC) $(CFLAGS) decode.c $(LDFLAGS) -o $@

#play_events: play_events.cpp xmlsp.cpp
#	g++ $(CFLAGS) play_events.cpp xmlsp.cpp $(LDFLAGS) -o $@

