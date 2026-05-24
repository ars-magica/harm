
# CharGen (pre-game advancement)

CharGen is simpler because each character and covenant is developed independently of all others. Hence the process can be managed from the `ArM.Character` and `ArM.Covenant` independently.

+ `ArM.Character.CharGen` provides the `prepareCharacter` function, which applies advancements recursively
+ Each advancement is prepared by `prepareCharGen` which applies
	+ `addInference` (shared with in-game)
			+ adding bonus
			+ validation
	+ `initialLimits` inferring XP and other limits
		+ this replaces the source quality (SQ) used in-game
	+ `agingYears` which computes age
		+ **TODO** aging
	+ sort traits (assumed by some steps)
	+ `validateCharGen` which checks the limits
		+ constituent functions for specific CharGen advancement types