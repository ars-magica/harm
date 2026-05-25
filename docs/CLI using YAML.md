---
tags:
  - armchar/json/cli
aliases:
  - "#armchar/json/cli"
---
+ Main file: `harm.hs`
+ Libraries: `ArM`
+ [[Roadmap]]
+ Design notes
	+ [[Character Generation Process]]
	+ [[JSON Char Gen Process.canvas|JSON Char Gen Process]] (canvas)
	+ [[Combat Stats]]
	+ [[Types for YAML]]
	+ [[Advancement in the YAML Model]]
	+ [[Validation Rules]]
	+ [[Books and Reading]]
	+ [[Joint Advancement]]
	+ [[Rendering in Markdown]]
+ Technical
	+ [[Aeson]]

+ The CLI performs one operation in two steps, both defined in `ArM.IO`
	+ `readSaga` reads the saga file and all the dependent files it requires, and computes every state requested in the file
		+ [[Saga Advancement Visualised.canvas|Saga Advancement Visualised]]
	+ `writeSaga` writes all the markdown files needed for saga website
+ 

+ Web pages
	+ Character sheet
		+ Use long sheet
	+ Error reports
		+ Per season listed
	+ Advancement - all characters per season
	+ Add text blocks
		+ saga data on main page

