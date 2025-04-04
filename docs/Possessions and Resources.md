---
tags:
  - possession
  - armchar/json
---
+ Possessions (weapons, equipment, etc.) are, in a sense, [[Trait]]s, but they are also something else.
+ The current implementation as of [[2025-04-04]]  has the `PossessionTrait` type, which wraps a `Possession` as a trait.
+ A `Possession` can have a reference to weapon or armour stats, either specific to the Possession or generic from a lookup table
+ **Question** *How do we handle books?*
	+ Characters may own copies of books, so they should be `Possession`s.
	+ Covenants commonly own books.
	+ Books may be copied. Do we want to count multiplicities or have multiple objects?
	+ Copies may sometimes differ from the original, with different stats.
	+ The original must be tracked, especially for tractatus which may not be reread
+ `BookOriginal` with stats and key
+ `BookCopy` with stats and reference to original key
	+ **note** the key is not a `TraitKey` since different copies need to be treated as different possessions but the same book