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
		+ [ ] More spell descriptions in CSV file
	+ [ ] Speciality in combat totals
	+ [ ] Web pages
		+ [ ] Make Covenant Page (Markdown)
		+ [ ] Narrative section on saga fron tpage
	+ [ ] Infer decrepitude from aging points
+ [ ] Phase 2. Improvements
	+ [ ] Remove dead and retired characters from main list
		+ [ ] Retired property in Aging type
	+ [ ] Validate missing seasons
	+ [ ] Infer decrepitude from aging points
	+ [ ] advance Tessa until 1255 with more aging
	+ [ ] Error control
		+ [ ] Parse invalid ProtoTrait as KeyPairList and display error in advancement log
+ [ ] Phase 3. Covenant
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
