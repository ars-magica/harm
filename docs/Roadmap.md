---
tags:
  - armchar/json/cli
---

# Roadmap

+ [ ] Phase 1. Review and bugfix
	+ [ ] Review
		+ [ ] Fix Cieran's vis
		+ [ ] Print and Double-Check Cieran
		+ [ ] Print and Double-Check Eogan
	+ [ ] Speciality in combat totals
	+ [ ] More spell descriptions in CSV file
	+ [ ] Change character names in test JSON files
+ [ ] Phase 2. Improvements
	+ [ ] Remove dead and retired characters from main list
		+ [ ] Retired property in Aging type
	+ [ ] Validate missing seasons
	+ [ ] Characteristics
		+ [ ] Great/Poor Characteristic (current warning)
		+ [ ] Handle multiple instances of the same virtue/flaw
	+ [ ] Decrepitude
		+ [ ] advance tessa until 1255 with more aging
		+ [ ] verify
		+ [ ] Aging points on characteristics
	+ [ ] Error control
		+ [ ] Parse invalid ProtoTrait as KeyPairList and display error in advancement log
+ [ ] Phase 3. Covenant
	+ [ ] Make Covenant Page (Markdown)
	+ [ ] Step 1. Covenant
		+ [ ] Book resource
		+ [ ] Library
		+ [ ] Read books from CSV
	+ [ ] Step 2. Covenant advancement
		+ [ ] Covenant advancement
		+ [ ] Covenant members
+ [ ] Phase 4. Polish
	+ [ ] Refactor 
		+ [ ] Use a single ProtoType field of type KeyTrait instead of a dozen Maybe Strings
	+ [ ] Refactor and document code
	+ [ ] Comment field on traits
	+ [ ]  P/G Char Gen
	+ [ ] Remove trait when advancing
	+ [ ] More user friendly sheets
		+ [ ] More compact character sheets
		+ [ ] PDF sheets
	+ [ ] Step 3. Virtues and Flaws - Special cases
		+ [ ] Linguist
		+ [ ] Inventive Genius
		+ [ ] Infer Second Sight from Strong Faerie Blood
		+ [ ] Unaging
	+ [ ] Count xp total (ingame) for validationparallel
	+ [ ] Print weapon tables etc
+ [ ] Phase 5. Performance
	+ [ ] Step 1.  CharacterState in JSON
		+ [ ] Remove null entries from JSON output
		+ [ ] Read CharacterState from JSON
+ [ ] Phase 6. Integer XP
	+ [ ] handle Correspondent (which goes beyond cap)
