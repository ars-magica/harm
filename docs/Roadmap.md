---
tags:
  - armchar/json/cli
title:
---

+ [[Visual Roadmap.canvas|Visual Roadmap]]

# Roadmap

**Critical Challenges**
+ [ ] Antologies
+ [ ] Multi-Tractatus
+ [ ] Books on Spell Masteries
+ [ ] Books on Loan
+ [ ] Covenant guests
+ [ ] Covenant membership
+ [ ] Remove lab from properties
+ [ ] Level cap on  advancement.

+ Advancement $\to$  need to identify
	+ [x] Book
	+ [ ] Part
	+ [ ] Original of Part
	+ [ ] Stats
	+ Book / Part and Original / Stats
		+ Book for resource Conflict
		+ Original for rereading check
		+ Part for Stats
		+ Stats for Advancement
	+ Primary XP Trait

**Other issues**
+ [ ] Trade and Gifts

1. SQ from book
2. Book lookup
	1. originalBook
	2. access to DB

## Plan

+ [ ] Immediate
	+ [x] Spell comments (see Cieran print)
	+ [ ] Split character sheet in different pages
		+ [ ] Brief grimoire
		+ [ ] Long grimoire in separate doc
	+ [ ] SQ penalties (e.g. Cieran injured)
	+ [ ] Double-check regression output
+ [ ] Phase 1.  **Review** Hibernia Saga
	+ [ ] Review Character JSON syntax
	+ [ ] Review recent seasons from Hibernia  for Elk's Run
	+ [ ] Review character sheet design (Cieran)
		+ [ ] Advancement view
		+ [ ] Lab totals
		+ [ ] Comments on advancing traits (possession in particular)
	+ [ ] Review covenant sheet and library
	+ [ ] Review Wiki layout for AnnalSeason
	+ [ ] Review object structure of AnnalSeason 
	+ [ ] [[Lab object]]
		+ [ ] Compute lab total in advancements
		+ [ ] Advance lab (refinement and virtue installation)
	+ [ ] Comment field on traits
+ [ ] Phase 2. Consolidation
	+ [ ] Validate missing seasons
	+ [ ] Character advancement
		+ [ ] Narrative 
		+ [ ] Comment
		+ [ ] Consistency of Narrative and Comment
	+ [ ] Fix ongoing
		+ [ ] Comment on ProtoTrait
		+ [ ] Validation: Compare SQ
+ [ ] Phase 3. Books
	+ [ ] [[Reading and Books]]
	+ [ ] Validate Reading Advancement
	+ [ ] levelCap on ProtoTrait - where should this go?
	+ [ ] Use of Keys and objects
	+ [ ] Type every book with edge cases and supersede wiki
+ [ ] Phase 4. Next steps
	+ [ ] Specialists (copyists in particular)
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
		+ [ ] PDF sheets
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
