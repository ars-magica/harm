
+ `Book` defines a text
+ `Possession` defines a volume
+ Both are `HarmObject` and kan be referenced by `HarmKey`
	+ In YAML this ay be
		+ `item: Book title`
		+ `book: HerbamRoot`


+ Explicit advancement : `read` - this is required for all reading
	+ either `item` or `book`
	+ both may be defined (list) to disambiguate
+ Explicit or implicit, depending on need for disambiguation
	+ `changes` - advancement from reading
+ Inferred
	+ `requires` - `Possession` objects for exclusive use
	+ `bookRead` - `Book` object used for advancement - always inferred