
I=$(HOME)/Hobby/github/hibernia/
J=$(HOME)/Hobby/github/ars-magica.github.io/
D=`find dist-newstyle -name doc | head`/html/harm/harm/

.force:

bin/harm: .force
	cabal install harm  --overwrite-policy=always --installdir=./bin
doc: .force
	cabal haddock harm  

install: bin/harm 
	cp --copy-contents $< $I/bin

wc:
	find src -name "*.hs" | xargs wc
