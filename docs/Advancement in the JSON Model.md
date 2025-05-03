
+ Advancement process is managed from Saga
	+ [[Saga Advancement Visualised.canvas|Saga Advancement Visualised]] illustrates the top level flow when a saga is loaded and processed.
	+ `advanceSaga` maps the advancement functions over `characters` and `covenants`
	+ `Character` and `Covenant` are members of the `StepAdvance` class which provides the following function
		+ nextAdv -> `AdvancementStep` object
		+ applyAdv -> advances, `AdvancementStep` object in and out
		+ completeAdv -> completes the `AdvancementStep` and returns the Object
	+ Other functions could be interleaved between the above functions
		+ possibly using lists of `AdvancementStep` objects
+ [ ] Review below - is it still correct?		
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
+ Joint advancement, as implemented in `Saga`''s `advance`
	+ Parse 1.  Compute providing characters
		+ Teacher SQ
		+ Possessions given away
	+ Parse 2.  Compute receivers
		+ Taught student SQ
			+ do we infer XP gain from SQ, or just validate?
		+ Possessions received
+ How do we augment the advancements using information from the covenant and other characters?
	+ Books from CovenantState
		+ pass joint state to advancement augmentation
	+ SQ from teacher (other character)
		+ tricky!  Inter-dependency between advancements
	+ Give/Receive possessions
		+ tricky!  Inter-dependency between advancements
	+ Add book to covenant library
		+ process characters first, then covenants

+ P/G advancement
	+ P/G season
	+ P/G year
	+ P/G part-year ?  20 xp and 10 xp

## Source Quality Calculation

| Mode      | Base       | Virtue            | Other                               | Traits             |
| :-------- | ---------- | ----------------- | ----------------------------------- | ------------------ |
| Adventure | Covenant   | Independent study |                                     | Any (5xp limit)    |
| Practice  | Individual | Independent study |                                     | Any (Usually One)  |
| Teaching  | Teacher    | Apt Student       | Specialitiy, one/two students, lab? | Decided by Teacher |
| Training  | Teacher    | Apt Student       |                                     | One                |
| Reading   | Book       | Book Learner      |                                     | One + Folio        |
| Vis study | Die roll   | Free study        |                                     | One                |
| Exposure  | 2          |                   |                                     | Any                |

+ Reading
	+ Defined Book + Ability + Extra

+ Bonuses may be lists, including justifications
+ Validation - compare standard SQ to autocomputed SQ