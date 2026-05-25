
I=$(HOME)/Hobby/github/hibernia/
J=$(HOME)/Hobby/github/ars-magica.github.io/
D=`find dist-newstyle -name doc | head`/html/harm/harm/

.force:

run: .force
	cabal run harm --allow-newer=base --allow-newer=template-haskell
test: .force
	cabal run yamltest --allow-newer=base --allow-newer=template-haskell

repl: .force
	cabal repl harm --allow-newer=base --allow-newer=template-haskell


bin/harm: .force
	cabal install harm  --overwrite-policy=always --installdir=./bin
doc: .force
	cabal haddock harm  

install: bin/harm 
	cp --copy-contents $< $I/bin

wc:
	find src -name "*.hs" | xargs wc
