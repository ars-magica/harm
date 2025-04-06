---
tags:
  - armchar/json/cli
---

+ [[Visual Roadmap.canvas|Visual Roadmap]]

# Roadmap

+ [ ] **Review** Hibernia Saga
	+ [ ] Fix Cieran's vis
	+ [ ] Print and Double-Check Cieran
	+ [ ] Print and Double-Check Eogan
	+ [ ] More spell descriptions in CSV file
	+ [ ] Session notes from Hibernia
	+ [ ] Review library
+ [ ] Phase 1. [[Covenant]]
	+ [ ] Reading and copying advancements
		+ [ ] using library books
		+ [ ] calculate book quality from author
		+ [ ] adding library books
		+ [ ] review use of ID
	+ [ ] Season page, showing all activity in a season 
	+ [ ] Grimoire
	+ [ ] Initiation scripts
+ [ ] Phase 2. Advancement 
	+ [ ] Reading
	+ [ ] Author books
	+ [ ] Teaching/.Taught
	+ [ ] Training
	+ [ ] Lab Total
	+ [ ] Enchantments
		+ [ ] Create Lesser Enchantments
		+ [ ] Create Greater Enchanted Devices with state
	+ [ ] Lab assistance
+ [ ] Phase 3. Improvements
	+ [ ] Remove dead and retired characters from main list
		+ [ ] Retired property in Aging type
	+ [ ] Validate missing seasons
	+ [ ] Advance Tessa until 1255 with more aging
	+ [ ] Error control
		+ [ ] Parse invalid ProtoTrait as KeyPairList and display error in advancement log
	+ [ ] Consider a list of narrative items in advancements
	+ [ ] Covenant Advancement with list of stories with SQ
+ [ ] Phase 4. Polish
	+ [ ] Break up possession
		+ [ ] different constructors for weapon and item
		+ [ ] parse JSON with different constructors
		+ [ ] support book possession
	+ [ ] Display aging bonus etc
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
