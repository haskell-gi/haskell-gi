
PROGS  = drawing drawing2 starandring text clock graph sdldrawing
SOURCES = Drawing.hs Drawing2.hs StarAndRing.hs Text.hs Clock.hs Graph.hs CairoSDL.hs

all : $(PROGS)

drawing : Drawing.hs
	$(HC_RULE)

drawing2 : Drawing2.hs
	$(HC_RULE)

starandring : StarAndRing.hs
	$(HC_RULE)

text : Text.hs
	$(HC_RULE)

clock : Clock.hs
	$(HC_RULE)

graph : Graph.hs
	$(HC_RULE)

sdldrawing : CairoSDL.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
