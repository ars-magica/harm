

1. Joining and leaving covenants - happens at the start of the season
2. Group characters to covenants
	1. This allows resolution of conflicts over resources at each covenant
	2. Attention. Guests and people leaving the covenant with equipment.
3. Joint advancement

New principle for characters and covenants
+ **First**  prepare advancement and move to past
	+ Keep the current advancement at the head of past
+ For each iteration, make necessary additions to the augmented advancement at the head of future
+ **Consider** a new `consistent` flag which is false during an advancement step

for this process we need functions to
1. `cvgCurrentAdv` to get Current Advancement

Operate on saga (`stepSaga`)
1. Bump saga season
2. `stepInit` : for each character
	1. `initAdvancement` 
		1. `prepareAdvancement`  to make augmented advancement with inference
		2. move advancement from future to past
3. `stepCovenFolk` : for each covenant
	1. `initCovAdvancement` 
		1. `iaPrepare`  to make augmented advancement
			1. inference may be added later
		2. move advancement from future to past
	2. `cvgCovenFolk` Apply joining and leaving 
4. `stepMembership`
	1. Clear `memberOf` from all characters
	2. `updateMembership` : For each covenant, for each member
		1. update `memberOf` 
		2. flag contradicgtions if `memberOf` is already set
5. For each covenant
	1. resolve resources[^1]
6. For each Character
	1. infer SQ
7. For each Character
	1. advance traits

[^1]:  Add a «read at covenant» field, defaulting to `memberOf`