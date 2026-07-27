
I=$(HOME)/bin/
H=$(HOME)/Hobby/github/hibernia/
J=$(HOME)/Hobby/github/ars-magica.github.io/
D=`find dist-newstyle -name doc | head`/html/harm/harm/

O=--allow-newer=base --allow-newer=template-haskell

.force:

run: .force
	cabal run harm  $O
test: .force
	cabal run yamltest $O

repl: .force
	cabal repl harm $O


install: .force
	cabal install harm  $O --overwrite-policy=always --installdir=$I
doc: .force
	cabal haddock harm  

oldinstall: bin/harm 
	cp --copy-contents $< $H/bin

wc:
	find src -name "*.hs" | xargs wc
