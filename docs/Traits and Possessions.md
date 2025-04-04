---
tags:
   - armchar/domain/model
---

+ **Note** Type references here refer to the Swish implementation #armchar/swish/model .
+ However, the principles apply to the JSON model #armchar/json too


+ Common Framework for Possessions and Trait
	+ same function `advanceTraitList`
+ The `Traitlike` class generalises some features which is common for every character aspect which changes over time
	+ Some features, called metadata, are defined as constant at character level.
+ Some features distinguish between Traits and Possessions
	+ [[Traits and Possessions in RDF.canvas|Traits and Possessions in RDF]]
+ [x] Is it necessary to treat Possession and Trait separately?  Can we use Traitlike instead?  #kanban/backlog 
+ Traits include
	+ Abilities
	+ Spells
	+ Arts
	+ CombatOption 
	+ Natural Weapons
+ Possessions include
	+ Equipment (including regular weapons)
	+ Vis
+ Attributes (common)
	+ label and description (see below)
+ Possession attributes
	+ quantity for StockItem
	+ art for Vis
+ [ ] Consider other attributes #cli/backlog 
	+ [ ] Comment
	+ [ ] Origin
+ Attributes for traits
	+ Specialty
+ [ ] link specialities to other traitlike classes (e.g. weapon) #cli/backlog 

+ [ ] Mark unskilled combat option specially  #cli/backlog 
+ [ ] Do we want to separate Weapon with stats from Weapon as equipment?  #kanban/backlog 
	+ This would allow us to link one piece of weaponry with different weapons using different skills

+ In **Haskell** there are two `Trait` types.
	+ `ArM.Types.Trait.Trait` is used for the application of advancements when the character graphs are constructed and augmented
	+ `ArM.Types.SheetObject.Trait` is used when the character sheets are read from RDFGraphs to produce output for play.



# History of Development

+ [x] How do we deal with multipurpose weapons?   #cli/selected
	+  e.g. throwing axe used as hatchet
	+ lance used as two-handed long spear
	+ need separate Weapon instances, since the stats are different
+ [x] Fix the inference rules for Combat Option #cli/selected
	+ [x]  Record skill with weapon
	+ [x] Default skill score of 0 when unskilled @completed(2024-02-03T18:31:10+01:00)
+ [x] Infer combat option from weapon (this requires a blind node) #cli/backlog  @completed(2024-02-04T21:26:14+01:00)
+ [x] Add class for Natural Weapons #cli/backlog
+ [x] Clean up subclasses of Possession #cli/backlog
