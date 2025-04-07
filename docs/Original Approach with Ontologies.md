---
tags:
  - armchar/swish
---
+ Two different attempts
    1. [[Webserver]] Model
    2. [[CLI with Swish]]
+ Soure code is hosted by hgeorgsch at githun
	+ git@github.com:hgeorgsch/armchar-swish.git
		+ old repo, focusing on the client/server architecture
		+ Note that these two repoes share a root
	+ git@github.com:hgeorgsch/armchar.git
		+ java server
	+ git@github.com:hgeorgsch/armchar-client.git
		+ client for the java server


## Design with the Ontology Approach

+ [[Webserver]] aiming for ReSTful support for a javascript client
    + the client subdirectory contains a simple python client
	+ The main line of development at the moment is a standalone CLI interface.
+ [[CLI Design]].  The following pages are not necessarily exclusive to CLI, but have been written in that context and are up to date.
	+ [[Loading RDF Graphs]]
	+ [[Design Notes]]
	+ [[Backend Modules]]
	+ [[CLI Data Model.canvas|CLI Data Model]]  including the Markdown output.  This is accurate and fairly complete as of 2024-02-03
	+ [[CLI Workflow.canvas|CLI Workflow]] 
		+ **Outdated**
		+ This is rather crude, but contains some more low level detail than the above.
	+ [[Advancement in the RDF model]]
+ Basic principls, common for all user interfaces.
	+ Business [[Logic]]
	+ [[Swish-vs-Jena]]
	+ [[Traits and Possessions in RDF.canvas|Traits and Possessions in RDF]]
+ [[DataTypes]]
	+ This is written with reference to the web server, but some parts may still be informative wrt CLI
+ [[Ontology]]