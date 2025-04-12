
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
+ Process
	+ Current - three functions applied by `stepIf`
		+ nextAdv -> (Object,Augmented Advancement) pair
		+ applyAdv -> advances, (O,AA) pair in and out
		+ completeAdv -> completes and returns the Object
	+ Note, same function names for Covenant and Character - this may have to change
	+ Other functions could be interleaved between nextAdv and applyAdv
		+ possibly using lists of O/AA pairs

+ P/G advancement
	+ P/G season
	+ P/G year
	+ P/G part-year ?  20 xp and 10 xp

## Source Quality Calculation

| Mode      | Base       | Virtue            | Other                               |
| :-------- | ---------- | ----------------- | ----------------------------------- |
| Adventure | Covenant   | Independent study |                                     |
| Practice  | Individual | Independent study |                                     |
| Teaching  | Teacher    | Apt Student       | Specialitiy, one/two students, lab? |
| Training  | Teacher    | Apt Student       |                                     |
| Reading   | Book       | Book Learner      |                                     |
| Vis study | Die roll   | Free study        |                                     |
| Exposure  | 2          |                   |                                     |
+ Advancement
	+ sourceQuality (base)
	+ bonusQuality covers virtue
+ AugmentedAdvancement
	+ computedSQ

+ Other
    + correspondent ??
    + study bonus - Manual !
    + custom virtues


+ Advancement
	+ Manual `sourceQuality` - required for Practice (or may default to 4 if missing), otherwise discouraged
	+ Manual `bonusSQ` - required for correspondent and study bonus, and possibly custom virtues/flaws
+ AugmentedAdvancement
	+ `sourceQuality` derived from either book, teacher, advancement `sourceQuality` or advncement mode
	+ bonuses from virtues
	+ manual bonuses from `Advancement`
+ Bonuses may be lists, including justifications
+ `effectiveSQ` is the sum of the above
+ Validation - compare standard SQ to autocomputed SQ