

1. Joining and leaving covenants - happens at the start of the season
2. Group characters to covenants
	1. This allows resolution of conflicts over resources at each covenant
	2. Attention. Guests and people leaving the covenant with equipment.
3. Joint advancement

Operate on saga.
1. Apply joining and leaving to all covenants
2. Clear `memberOf` and put characters in Map
3. Put covenants in map and update `memberOf` for each member
	1. flag contradicgtions
4. For each covenant
	1. resolve resources[^1]
5. For each Character
	1. infer SQ
6. For each Character
	1. advance traits

[^1]:  Add a «read at covenant» field, defaulting to `memberOf`