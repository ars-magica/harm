---
tags:
  - armchar/json/cli
---

# Roadmap

+ [ ] **Review**
	+ [ ] Fix Cieran's vis
	+ [ ] Print and Double-Check Cieran
	+ [ ] Print and Double-Check Eogan
	+ [ ] More spell descriptions in CSV file
+ [ ] Phase 1. Covenant
	+ [ ] Library  see [[Possessions and Resources]]
	+ [ ] Read books from CSV
	+ [ ] Covenant advancement
	+ [ ] [[Covenant]] members
+ [ ] Phase 2. Improvements
	+ [ ] Speciality in combat totals
	+ [ ] Remove dead and retired characters from main list
		+ [ ] Retired property in Aging type
	+ [ ] Validate missing seasons
	+ [ ] Advance Tessa until 1255 with more aging
	+ [ ] Error control
		+ [ ] Parse invalid ProtoTrait as KeyPairList and display error in advancement log
	+ [ ] Consider a list of narrative items in advancements
+ [ ] Phase 4. Polish
	+ [ ] Comment on warping «from LR»
	+ [ ] Display aging bonus etc
	+ [ ] Mark house virtues, mysteries etc
	+ [ ] Refactor 
		+ [ ] Use a single ProtoType field of type KeyTrait instead of a dozen Maybe Strings
	+ [ ] Refactor and document code
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
+ [ ] Phase 5. Performance
	+ [ ] Step 1.  CharacterState in JSON
		+ [ ] Remove null entries from JSON output
		+ [ ] Read CharacterState from JSON
+ [ ] Phase 6. Integer XP
	+ [ ] handle Correspondent (which goes beyond cap)
