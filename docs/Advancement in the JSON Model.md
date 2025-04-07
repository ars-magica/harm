
+ [[Saga Advancement Visualised.canvas|Saga Advancement Visualised]] illustrates the top level flow when a saga is loaded and processed.
	+ maps advance over `characters` and `covenants`
+ For each season, the `advance` function is called independently for each `Character` and each `Covenant`
	+ This is not fully developed for `Covenant` yet, for `Character` it
		+ First augments the advancement
			+ adding limits
			+ adding bonus
			+ inferring additional traits
			+ validation
		+ Then applies the advancement
	    + In both cases it depends on the State from previous season and Advancement from current season.
	        + The State includes virtues and flaws which may modify behaviour.
+ Possible extension of `Saga` `advance`
	+ Parse 1.  Compute providing characters
		+ Teacher SQ
		+ Possessions given away
	+ Parse 2.  Compute receivers
		+ Taught student SQ
			+ do we infer XP gain from SQ, or just validate?
		+ Possessions received
	+ This is not worth it at the moment
+ How do we augment the advancements using information from the covenant and other characters?
	+ Books from CovenantState
		+ pass joint state to advancement augmentation
	+ SQ from teacher (other character)
		+ tricky!  Inter-dependency between advancements
	+ Give/Receive possessions
		+ tricky!  Inter-dependency between advancements
	+ Add book to covenant library
		+ process characters first, then covenants
+ Advancement Types
	+ Ingame Season
		+ Adventure - fixed SQ + Independent study
		+ Practice - fixed SQ + Independent study
		+ Teaching - validate SQ against teacher
		+ Training - validate SQ against trainer
		+ Reading - get SQ from book
		+ Vis study - fixed SQ + free study
		+ Exposure - fixed SQ
			+ Copying
			+ Authoring
			+ Teaching
			+ Training
			+ Lab work
			+ Opening Arts
			+ Initiation
	+ SQ bonuses which require human judgment
		+ correspondent
		+ study bonus