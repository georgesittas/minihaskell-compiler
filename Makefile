compile:
	ghc --make TestAll.hs -package mtl

clean:
	rm -f *.hi *.o TestParser TestAll
