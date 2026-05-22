

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
1. getCurrentAdvancement
2. updateCurrrentAdvancement
3. prepare advancement

Operate on saga.
1. Bump saga season
2. For each covenant
	1. prepare advancement 
	2. bump season
	3. Apply joining and leaving 
3. Clear `memberOf` and put characters in Map
4. For each covenant, for each member
	1. update `memberOf` 
	2. flag contradicgtions if `memberOf` is already set
5. For each covenant
	1. resolve resources[^1]
6. For each Character
	1. infer SQ
7. For each Character
	1. advance traits

[^1]:  Add a «read at covenant» field, defaulting to `memberOf`