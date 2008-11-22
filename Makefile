
LDFLAGS = -L/usr/X11R6/lib -lX11 -lm
CFLAGS = -g -Wall -ansi
GHCFLAGS = -prof -auto-all 

#all: decode play_events
all: decode abducer

decode: decode.c
	$(CC) $(CFLAGS) decode.c $(LDFLAGS) -o $@

abducer: abducer.hs Acquisition.hs Frame.hs Noise.hs Abducer.hs Reasoner.hs Vocabulary.hs Reasoner/*.hs WrappedInts/*.hs
	ghc -package HaXml $(GHCFLAGS) -o abducer WrappedInts/Types.hs WrappedInts/IDSet.hs WrappedInts/IDMap.hs \
	Reasoner/Types.hs Reasoner/Core.hs Reasoner/Constrainers.hs \
	Reasoner/Extra.hs Reasoner.hs Vocabulary.hs Types.hs World.hs Acquisition.hs \
        Noise.hs Frame.hs Track.hs \
        Abducer.hs abducer.hs

clean:
	rm -f *.o *.hi Reasoner/*.o Reasoner/*.hi WrappedInts/*.o WrappedInts/*.hi

#play_events: play_events.cpp xmlsp.cpp
#	g++ $(CFLAGS) play_events.cpp xmlsp.cpp $(LDFLAGS) -o $@

# DO NOT DELETE: Beginning of Haskell dependencies
AcquisitionTypes.o : AcquisitionTypes.hs
AcquisitionFuncs.o : AcquisitionFuncs.hs
AcquisitionFuncs.o : AcquisitionTypes.hi
WrappedInts/Types.o : WrappedInts/Types.hs
WrappedInts/IDSet.o : WrappedInts/IDSet.hs
WrappedInts/IDSet.o : WrappedInts/Types.hi
WrappedInts/IDMap.o : WrappedInts/IDMap.hs
WrappedInts/IDMap.o : WrappedInts/Types.hi
WrappedInts/IDMap.o : WrappedInts/IDSet.hi
Reasoner/Types.o : Reasoner/Types.hs
Reasoner/Types.o : WrappedInts/Types.hi
Reasoner/Types.o : WrappedInts/IDSet.hi
Reasoner/Types.o : WrappedInts/IDMap.hi
Reasoner/Core.o : Reasoner/Core.hs
Reasoner/Core.o : WrappedInts/Types.hi
Reasoner/Core.o : Reasoner/Types.hi
Reasoner/Core.o : WrappedInts/IDSet.hi
Reasoner/Core.o : WrappedInts/IDMap.hi
Reasoner/Core.o : WrappedInts/IDMap.hi
Reasoner/Constrainers.o : Reasoner/Constrainers.hs
Reasoner/Constrainers.o : Reasoner/Types.hi
Reasoner/Constrainers.o : Reasoner/Core.hi
Reasoner/Extra.o : Reasoner/Extra.hs
Reasoner/Extra.o : Reasoner/Constrainers.hi
Reasoner/Extra.o : Reasoner/Types.hi
Reasoner/Extra.o : WrappedInts/Types.hi
Reasoner/Extra.o : Reasoner/Core.hi
Reasoner/Extra.o : WrappedInts/IDSet.hi
Reasoner/Extra.o : WrappedInts/IDMap.hi
Reasoner/Extra.o : WrappedInts/IDMap.hi
Reasoner.o : Reasoner.hs
Reasoner.o : Reasoner/Extra.hi
Reasoner.o : Reasoner/Constrainers.hi
Reasoner.o : Reasoner/Types.hi
Reasoner.o : WrappedInts/Types.hi
Vocabulary.o : Vocabulary.hs
Vocabulary.o : Reasoner.hi
abducer.o : abducer.hs
abducer.o : WrappedInts/Types.hi
abducer.o : WrappedInts/IDSet.hi
abducer.o : Vocabulary.hi
abducer.o : Reasoner.hi
abducer.o : AcquisitionFuncs.hi
abducer.o : AcquisitionTypes.hi
# DO NOT DELETE: End of Haskell dependencies
