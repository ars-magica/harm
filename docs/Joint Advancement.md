

1. Joining and leaving covenants - happens at the start of the season
2. Group characters to covenants
	1. This allows resolution of conflicts over resources at each covenant
	2. Attention. Guests and people leaving the covenant with equipment.
3. Joint advancement

Operate on saga.
1. CovenFolk: Apply joining and leaving to all covenants  $\to$ `stepCovenFolk`
2. Update membership
	1. Clear `memberOf` 
	2. For each covenants update `memberOf` for each member
	3. flag contradicgtions
3. Character advancement
	1. Inference
	2. Individual advancement
	3. Validation
4. Joint advancement
5. For each covenant
	1. resolve resources[^1]
6. For each Character
	1. infer SQ
7. For each Character
	1. advance traits

[^1]:  Add a «read at covenant» field, defaulting to `memberOf`