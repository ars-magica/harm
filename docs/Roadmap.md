---
tags:
  - armchar/json/cli
title:
---

+ [[Visual Roadmap.canvas|Visual Roadmap]]

# Roadmap
## Check List

+ [ ] JSON syntax
	+ [ ] Character
	+ [x] Covenant
	+ [x] Saga  (SagaFile)
+ [ ] Wiki layout
	+ [ ] character view (Cieran)
	+ [x] covenant view (Elk's Run)
	+ [x] saga view
	+ [ ] annals view

## Plan

+ [ ] Phase 1.Consolidation
	+ [ ] Fix annals display (bullet points)
	+ [ ] Covenant
		+ [ ] Story advancement
			+ [ ] SQ
			+ [ ] Narrative
			+ [ ] Comment
			+ [ ] Should we allow multiple stories in the season?
		+ [ ] Story object
			+ [ ] Narrative
			+ [ ] Comment
			+ [ ] SQ
		+ [ ] JSON Syntax for advancement
		+ [ ] WIki layout for advancement
	+ [ ] Validate missing seasons
	+ [ ] Character advancement
		+ [ ] Narrative 
		+ [ ] Comment
	+ [ ] SQ from book
		+ [ ] Validate Reading Advancement
		+ [ ] levelCap on ProtoTrait - where should this go?
	+ [ ] Display aging bonus etc
+ [ ] **Review** Hibernia Saga
	+ [ ] Fix ongoing
		+ [ ] [[Reading and Books]]
		+ [ ] Check Exposure
		+ [ ] Comment on ProtoTrait
		+ [ ] Validation: Compare SQ
	+ [ ] Review character sheet design (Cieran)
		+ [ ] Spell view
		+ [ ] Advancement view
		+ [ ] Aging view
			+ [ ] Aging roll total
		+ [ ] Comments on advancing traits (possession in particular)
		+ [ ] Sort possessions
	+ [ ] More spell descriptions in CSV file
	+ [ ] Review covenant sheet and library
	+ [ ] Review object structure
		+ [ ] AnnalSeason 
	+ [ ] Specialists (copyists in particular)
+ [ ] Phase 2. [[Covenant]]
	+ [ ] Reading and copying advancements
		+ [ ] calculate book quality from author
		+ [ ] check for rereading of tractatus
		+ [ ] check for collisions on books
		+ [ ] adding library books
			+ [ ] covenant advancement may depend on character advancement
			+ [ ] character advancement depends on previous covenant state for book availability
			+ [ ] new books are created by characters and may propagate into the covenant advancement, augmenting the library
		+ [x] review use of ID
	+ [ ] Grimoire
	+ [ ] Initiation scripts
	+ [ ] group and  sort books in library
	+ [ ] Handle Antologies
+ [ ] Phase 3. Advancement 
	+ [ ] Author books
	+ [ ] Teaching/Taught
		+ [ ] Difficult to derive SQ from the teacher
		+ [ ] Could potentially augment advancement and advance in multiple steps
		+ [ ] Training
	+ [ ] Lab Total
		+ [ ] Define Lab object
		+ [ ] Advance lab (refinement and virtue installation)
		+ [ ] Use lab bonuses in lab total
	+ [ ] Enchantments
		+ [ ] Create Lesser Enchantments
		+ [ ] Create Greater Enchanted Devices with state
	+ [ ] Lab assistance
+ [ ] Phase 3. Improvements
	+ [ ] Distinguish between comment and narrative
	+ [ ] Virtue/Flaw descriptions
	+ [ ] Remove dead and retired characters from main list
		+ [ ] Retired property in Aging type
	+ [ ] Advance Tessa until 1255 with more aging
	+ [ ] Error control
		+ [ ] Parse invalid ProtoTrait as KeyPairList and display error in advancement log
	+ [ ] Consider a list of narrative items in advancements
+ [ ] Phase 4. Polish
	+ [ ] Break up possession
		+ [ ] different constructors for weapon and item
		+ [ ] parse JSON with different constructors
		+ [ ] support book possession
		+ [ ] Get book from own CharacterState - when Possession supports book
	+ [ ] Mark house virtues, mysteries etc
	+ [ ] Refactor and document code
		+ [ ] Use a single ProtoType field of type KeyTrait instead of a dozen Maybe Strings
		+ [ ] Draw a flow chart of the character and covenant processing
	+ [ ] Comment field on traits
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
+ [ ] Laboratory
+ [ ] Familiar
+ [ ] Talisman (and other enchanted devices)
	+ [ ] Greater Enchanted Devices have a state - powers may be added
+ [ ] Phase 5. Performance
	+ [ ] Step 1.  CharacterState in JSON
		+ [ ] Remove null entries from JSON output
		+ [ ] Read CharacterState from JSON
+ [ ] Phase 6. Integer XP
	+ [ ] handle Correspondent (which goes beyond cap)
