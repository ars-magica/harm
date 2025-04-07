---
title: Advancement
tags:
    - armchar/swish/model
---

+ Advancement entails two computational tasks
	+ Advancing the Character
	+ Checking for resource conflicts
+ In both cases, resources from the covenant is required (e.g. library)
+ In the second case, advancements from all characters must be considered together

## Collection of graphs

+ At present, the database is split into individual graphs
	+ Character with advancement
	+ Character Sheet at a given time, incorporating advancement
	+ The Covenant is a Character with Character graph and Sheet graphs
	+ Schema
	+ Resources
	+ saga (not used)
+ To avoid conflicts, traits are referenced by class, and the instance inferred
	+ new instances are created if the values change
+ Resources in an advancement must be checked against current covenant sheet
	+ scores must be changed against previous character sheet
## Character Computation

+ `ArM.Types.Advancement` exports three functions
	+ `getPregameAdvancements`
	+ `getIngameAdvancements`
	+ `getAllAdvancements`
+ and the `Advancement` type
+ the three functions are essentially the same (two different subsets and the whole)

```
getAllAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
```

+ `g` is the graph
+ `c` is the character's RDFLabel
+ `inGameAdv` is the class of advancements to be selected
+ The `queryGraph` gets the arc $(s,p,o)$ and $l_p$ where $s$ is the advancement and and $p$ is any property with an RDFS label $l_p$

```
queryGraph c = listToRDFGraph  . f c
   where f c1 c2 = [ arc (Var "id") typeRes c1
            , arc (Var "id") (Var "property") (Var "value")
            , arc (Var "id") (armRes  "advanceCharacter") c2
            , arc (Var "property") resRdfsLabel (Var "label") ]

```

The critical function is thus this
```
getAdvancements g = fixAdvancements g . map toAdvancement . arcListSplit . getGenQuads g
```

Hence we go through several steps
+ `getGenQuads` makes the query and turns the result into arcs $(s,p,o)$
+ `arcListSplit` splits the list into a list of lists per subject (advancement) $s$
+ `toAdvancement` parses the advancement properties
+ `fixAdvancements` parses the traits
