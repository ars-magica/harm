---
tags:
  - armchar/json/cli
title:
---

+ [[Visual Roadmap.canvas|Visual Roadmap]]

# Roadmap

**Critical Chall))enges**
+ [ ] Antologies
+ [ ] Multi-Tractatus
+ [ ] Books on Spell Masteries
+ [ ] Books on Loan
+ [ ] Covenant guests
+ [ ] Covenant membership
+ [ ] Remove lab from properties
+ [ ] Level cap on  advancement.

**Other issues**
+ [ ] Trade and Gifts

1. SQ from book
2. Book lookup
	1. originalBook
	2. access to DB

## Plan

+ [ ] Immediate
	+ See github
	+ [ ] Review current [[Demo Data]]
	+ [ ] Set up regression test
+ [ ] Might do
	+ [ ] Split character sheet in different pages
		+ [ ] Brief grimoire
		+ [ ] Long grimoire in separate doc
	+ [ ] SQ penalties (e.g. Cieran injured)
+ [ ] Phase 5. [[Covenant]]
	+ [ ] Reading and copying advancements
		+ [ ] calculate book quality from author
		+ [ ] check for rereading of tractatus
		+ [ ] check for collisions on books
		+ [ ] adding library books
			+ [ ] covenant advancement may depend on character advancement
			+ [ ] character advancement depends on previous covenant state for book availability
			+ [ ] new books are created by characters and may propagate into the covenant advancement, augmenting the library
	+ [ ] Grimoire
	+ [ ] Initiation scripts
	+ [ ] group and  sort books in library
	+ [ ] Handle Antologies
+ [ ] Phase 6. Advancement 
	+ [ ] Author books
	+ [ ] Teaching/Taught
		+ [ ] Difficult to derive SQ from the teacher
		+ [ ] Training
	+ [ ] Enchantments
		+ [ ] Create Lesser Enchantments
		+ [ ] Create Greater Enchanted Devices with state
	+ [ ] Lab assistance
+ [ ] Phase 3. Improvements
	+ [ ] Virtue/Flaw descriptions
	+ [ ] Remove dead and retired characters from main list
		+ [ ] Retired property in Aging type
	+ [ ] Advance Tessa until 1255 with more aging
	+ [ ] Error control
		+ [ ] Parse invalid ProtoTrait as KeyPairList and display error in advancement log
+ [ ] Phase 4. Polish
	+ [ ] Break up possession
		+ [ ] support book possession
		+ [ ] Get book from own CharacterState - when Possession supports book
	+ [ ] Mark house virtues, mysteries etc
	+ [ ] P/G Char Gen
	+ [ ] Remove trait when advancing
	+ [ ] More user friendly sheets
		+ [ ] More compact character sheets
		+ [ ] PDF sheets))))
	+ [ ] Step 3. Virtues and Flaws - Special cases
		+ [ ] Linguist
		+ [ ] Inventive Genius
		+ [ ] Unaging
	+ [ ] Count xp total (ingame) for validation
	+ [ ] Print weapon tables etc
	+ [ ] More [[Validation Rules]]
+ [ ] Characters staying at a foreign covenant
+ [ ] Familiar
+ [ ] Talisman (and other enchanted devices)
	+ [ ] Greater Enchanted Devices have a state - powers may be added
+ [ ] Phase 5. Performance
	+ [ ] Step 1.  CharacterState in JSON
		+ [ ] Remove null entries from JSON output
		+ [ ] Read CharacterState from JSON
+ [ ] Phase 6. Integer XP
	+ [ ] handle Correspondent (which goes beyond cap)
