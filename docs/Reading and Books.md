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
	+ Antology has `collectionOf :: [ Book ]` 
		+ Individual work has empty list
		+ This includes `Folio` which has the additional feature of bonus XP in Hermes Lore
		+ Antology has empty list of `BookStats` - defers to constituent works
		+ If `collectionOf` is empty, it may still be an antology if is a copy of one
	+ Copy has `copyOf :: Maybe Book`
		+ Original has `Nothing`
		+ May have `BookStats` overriding original
			+ if empty list, it defers to original
	+ `BookStats` comprises
		+ Tractatus (Q - no L)
		+ Summa (Q+L)
		+ Lab Text (no Q no L)
		+ readingCount (default to 1) > 1 for tractatus that can be read a number of times 
		+ topic - `TraitKey`
	+ Multi-topic summæ
		+ `bookStats :: [BookStats]`
		+ identified by `TraitKey`
		+ vanilla books have a singleton list
+ Book ID
	+ (CopyID,TextID)
	+ Every `Book` and `BookStats` object needs a unique ID
		+ Antology and constituent part
		+ Copy and Original
		+ Each `BookStats` object in a multi-topic book
	+ Reading, we need to identify
		+ Copy - Part - Stats/Trait
	+ From Book Copy is retrived
		+ ID of the Original (to prevent rereading)
		+ BookStats of the Copy for the particular topic

```
data ReadingID = ReadingID
     { bookRead :: BookID
     , partRead :: Maybe BookID
     , topicRead :: TraitKey
     }
getCopy :: SagaState -> ReadingID -> Book 
getBookStats :: SagaState -> ReadingID -> Book 
getOriginal :: SagaState -> ReadingID -> Book 
```
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