test-parser:
	ghc --make TestParser.hs -package mtl
	./TestParser

clean:
	rm *.hi *.o TestParser
