test-parser:
	ghc --make TestParser.hs -package mtl
	./TestParser

clean:
	rm -f *.hi *.o TestParser
