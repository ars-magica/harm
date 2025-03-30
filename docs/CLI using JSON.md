---
tags:
  - armchar/json/cli
aliases:
  - "#armchar/json/cli"
---
+ Main file: `harm.hs`
+ Libraries: `ArM`
+ [[Roadmap]]
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
