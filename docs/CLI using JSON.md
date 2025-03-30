---
tags:
  - armchar/json/cli
aliases:
  - "#armchar/json/cli"
---
+ Main file: `harm.hs`
+ Libraries: `ArM`
+ Design notes
	+ [[Character Generation Process]]
	+ [[JSON Char Gen Process.canvas|JSON Char Gen Process]] (canvas)
	+ [[Combat Stats]]
	+ [[Types for JSON]]


+ Web pages
	+ Character sheet
		+ Use long sheet
	+ Error reports
		+ Per season listed
	+ Advancement - all characters per season
	+ Add text blocks
		+ saga data on main page

## Roadmap
+ [ ] Phase 1. Review and bugfix
	+ [ ] Review
		+ [ ] Print and Double-Check Cieran
		+ [ ] Print and Double-Check Eogan
	+ [ ] Finishing touches
		+ [ ] Vis accounting
		+ [ ] Speciality in combat totals
	+ [ ] More spell descriptions in CSV file
	+ [ ] Change character names in test JSON files
	+ [ ] JSON syntax check
		+ [x] Run json syntax check on github workflow
		+ [ ] Link error log on front page
+ [ ] Phase 2. Improvements
	+ [ ] Remove dead and retired characters from main list
		+ [ ] Retired property in Aging type
	+ [ ] Validate missing seasons
	+ [ ] Image for characters
		+ [ ] Generate image Eogan
	+ [ ] Step 3. Characteristics
		+ [ ] Great/Poor Characteristic (current warning)
		+ [ ] Handle multiple instances of the same virtue/flaw
	+ [ ] Step 6. Decrepitude
		+ [ ] make old grog test file
		+ [ ] verify
	+ [ ] Error control
		+ [ ] Parse invalid ProtoTrait as KeyPairList and display error in advancement log
+ [ ] Phase 3. Covenant
	+ [ ] Covenant data model - analogous to Character
	+ [ ] Step 1. Covenant
		+ [ ] Book resource
		+ [ ] Library
		+ [ ] Read books from CSV
	+ [ ] Step 2. Covenant advancement
		+ [ ] Covenant advancement
		+ [ ] Covenant members
+ [ ] Phase 4. Polish
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

+ Advancement process.
    + Uses State from previous season and Advancement from current season.
        + The State includes virtues and flaws which may modify behaviour.
        + However, implied traits are effective immediately.
    + Advancement may be amended
        + additional XP
        + `prepareAdvancement :: CharacterState -> Advancement -> Advancement`
    + Virtues and flaws add implied traits
        + `inferTraits :: CharacterState -> [ProtoTrait] -> [ProtoTrait]`
        + This handles affinities and puissant
    + Advance trait $\to$  `advance`
+ Advancement Types
	+ Ingame Season
		+ Adventure - fixed SQ + Independent study
		+ Practice - fixed SQ + Independent study
		+ Teaching - validate SQ against teacher
		+ Training - validate SQ against trainer
		+ Reading - get SQ from book
		+ Vis study - fixed SQ + free study
		+ Exposure - fixed SQ
	+ SQ bonuses
		+ correspondent
		+ study bonus
