---
tags:
  - armchar/json
---

+ There are three core types of domain objects in the model
	+ `Saga` 
	+ `Character`
	+ `Covenant`
+ The `Saga` is the top level of the data structure, maintaining lists of all characters and covenants in play.
+ Sagas, Characters, and Covenants are time dependent object, which change from one season to the next.
	+ All the state dependent information is kept in a separate field (as a `SagaState`, `CharacterState`, or `CovenantState` respectively)
	+ They are instances of the `Advance` class, with function to advance to new seasons in time
+ A saga is loaded by the `readSaga` function from `ArM.IO`, which also loads all constituent files as specified in the main file
	+ hArM uses both CSV and JSON files
	+ JSON is used for saga, characters, and covenants
	+ CSV is used for lists of books, spells, and weapons

A large and critical part of the logic is concerned with the individual character. We will therefore discuss that first, before we go into the management of covenants and sagas.


## The Character

+ The `Character Type` is provided by `ArM.Char.Character`.  It is a composite type, comprising
	+ a `CharacterConcept` (with permanent metadata like name and year of birth)
	+ a `CharacterState` (with current stats)
+ The character state is derived from a series of advancements.
	+ In the input from the user, the advancements are defined by `Advancement` objects, defined in `ArM.Internal.Advancement`. The `Character` object includes two lists of such objects.
		+ `pregameAdvancement` for CharGen advancement
		+ `futureAdvancement` for in-game advancement
	+ When the character state is computed, each `Advancement` object is augmented with inferred data and the result applied to the `CharacterState` to generate a new `CharacterState`.
	+ When a state is calculated, the advancements are removed from the input lists and added to new lists for augmented advancements
		+ `pregameDesign` for CharGen 
		+ `pastAdvancement` for in-game advancement
+ In principle, a character can be persisted at any stage of advancement, but this is not implemented yet.  When it is, it will allow saving startup time.
+ It is always possible to revert to a past state, since the original advancements are stored as part of `AugmentedAdvancement`
+ `CharacterState` contains
	+ list of traits
	+ current season
+ The `CharacterState` object stores all kinds of traits in one list. To process the character, we normally use `CharacterSheet` which is easily calculated from `CharacterState` and provides lists for each type of trait (art, ability, spell)

### Traits

+ Constituent types
	+ `ProtoTrait` represents advancement of a trait
	+ `Trait` represents a trait with computed scores
		+ [[Characteristics]]
		+ see also [[Traits and Possessions]] for a general view inhereted from the RDF model
		+ [[Possessions and Resources]]
	+ `Advancement` represents advancement of a character or covenant
		+ includes a list of `ProtoTrait` advancing individual traits
		+ includes a time (season) or a stage (pregame)
		+ may include a narrative
		+ may include appearance, if it changes

### Advancement	

+ Advancement
	+ as entered by user
		+ season 
		+ totalXP
		+ uses (book)
		+ changes 

## Covenant and Saga

+ [[Saga object]]