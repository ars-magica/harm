---
tags:
  - armchar/json
  - advancement
---


+ Reading advancement
	+ Book
		+ dependent on ability
	+ Bonus SQ - can we mark explicit choice?
	+ Correspondent - can this be inferred?
+ Books
	+ Tractatus (Q - no L)
	+ Summa (Q+L)
	+ Folio (Antonology)
		+ `comprises :: [Book]`
	+ Multi-topic summæ
		+ `bookStats :: [BookStats]`
		+ identified by `TraitKey`
	+ Multi-read tractatus
		+ `bookStats` has a multiplicity?
+ Book ID
	+ (CopyID,TextID)
+ `Advancement` Object
	+ `mode` = `Reading`
	+ `usesBook` (ID)
		+ subbook 
		+ ability
	+ `changes`
		+ main ability
		+ other abilities? 
			+ correspondent
			+ OoH lore from Folio
+ Types of changes
	+ teaching/training/book - SQ by source
	+ correspondent bonus XP
	+ folio bonus XP