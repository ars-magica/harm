---
tags:
  - possession
  - armchar/json
---

+ Possessions (weapons, equipment, etc.) are, in a sense,
  [[Trait]]s, but they are also something else.
+ The current implementation as of [[2025-04-04]]  has the `PossessionTrait` type,
  which wraps a `Possession` as a trait.
+ A `Possession` can have a reference to weapon or armour stats,
  either specific to the Possession or generic from a lookup table
+ Both covenants and characters may possess `Possession` objects.
    + any `Possession` type may be owned by either, but some are more
      commonly owned by covenants and others by characters.  This is
      left to troupe discressions
+ A `Possession` may be a book or an antology.  The actual text (summa or
  tractatus is represented by the `Book` type, but the `Possession` type
  has a list of `Book` objects (which may be empty for non-books).
+ Similarly to books, a `Possession` contains a list of `LabText` objects.
+ When a character reads a book, they need to record both the `Possession`
  object occupied and the `Book` object consumed.
+ When books are copied, they may be recorded as individual `Possession` 
  objects, tracking copyist, time of copying, and other details, or they
  may be recorded as multiple copies of the original, non-descript object.
+ `Possession` has a `qualityBonus` which may modify the book stats, in the
  case of non-standard quality.

+ **TODO**
    + translations of books
    + Possessions for
        + [ ] Income source
        + [ ] Vis source
        + [ ] Specialist
    + Advancement to transfer Possessions
    + create possession
    + copy possession


+ Special cases.
    + Non-descript copy of book.
    + Non-descript instance of weapon/armour.

+ Characters may
    + `read: bookID`
    + `read: possessionName` 
        + infer `read: bookID`
        + infer `use: possessionName`
    + `read:`
        + `item: possessionName`
        + `booktitle: title` or `bookid: id`
    + `requires:` possessionName`
    + `create:` possession object
    + `copy:` possession object
