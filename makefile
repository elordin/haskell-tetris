COMPILER = ghc
COMFLAGS = -threaded
SRCFILES = util.hs font.hs keyboardHandler.hs game.hs display.hs main.hs

all:
	$(COMPILER) $(COMFLAGS) $(SRCFILES)

o2:
	$(COMPILER) $(COMFLAGS) -O2 $(SRCFILES)

clean:
	rm *.hi *.o main.exe