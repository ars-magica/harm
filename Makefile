all:  diff

md: grog.md sylvain.md
diff: grog.diff sylvain.diff marcus.diff
formats: charactersheet.pdf charactersheet.html

marcus.md:
sylvain.md:
grog.md:

.force:

O=Ontology/resources.ttl Ontology/arm.ttl

%.md: Test/%.ttl .force $O
	cabal run armchar-cli -- -c $< -s Test/diedne.ttl -o $@ -O $*.triples

%: Test/%.ttl .force $O
	cabal run armchar-cli -- -c $< -s Test/diedne.ttl -D $@
prof: Test/marcus.ttl .force 
	cabal run prof -- -c $< -s Test/diedne.ttl -D $@

%.pdf: %.md
	pandoc -o $@ $<
%.html: %.md
	pandoc -o $@ $<

Ontology/%.ttl: .force
	( cd Ontology ; $(MAKE) $*.ttl )

%.diff: %.md
	diff $< Test/$< | tee $@

wc:
	find src -name "*.hs" | xargs wc
