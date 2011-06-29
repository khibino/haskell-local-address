
sources = Network/Socket/LocalAddress.hs

objs = $(sources:.hs=.o)
interfaces = $(sources:.hs=.hi)

%.o: %.hs
	ghc -Wall -c $<

all: build hlint cabal_build

build: $(objs)

hlint:
	hlint --utf8 $(sources)

cabal_build:
	cabal configure
	cabal build
	cabal haddock

clean:
	$(RM) $(objs) $(interfaces)
	cabal clean
