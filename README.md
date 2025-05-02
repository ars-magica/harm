# armchar-swish

ArM character generator implemented in declarative programming,
using Haskell.

The repository includes several distinct attempts.  The current
attempt uses data files in JSON and CSV.  The code for this is
essentially
+ The `ArM.Char` modules
+ The `ArmChar.hs` main script
This provides a CLI tool to produce character sheets and validation
reports from the character defined in JSON.


Further documentation on the
[CLI using JSON](docs/CLI%20using%20JSON.md)
is found in the docs hierarchy.  
Sample character files are found in the `Data` subdirectory.

Previous attempts used data files in RDF and the Swish library.
One problem with this approach is the computational complexity
of logic inference on RDF graphs.  The code includes attempts
on both a client/server solution and a standalone CLI tool, but
both are currently incomplete and on hold.
This has been removed from the main branch, but exist on the swish
branch.  See also [CLI with Swish](docs/CLI%20with%20Swish.md).

## Download

This is still pre-release, but pre-compiled binaries can be
downloaded the last workflow run. 

1.  Choose the Actions tab (above).
2.  Choose the Build workflow on the left hand side.
3.  Choose the last (top of the list) run that is marked green.
4.  Find the artifacts at the bottom of the page.  There should be
    three of them, for Ubuntu, MacOS, and Windows.

Unzipping the archive, you will find a single executable to run
from the command line.

## Usage

The following command builds all the character sheets for the
Hibernia saga defined in `Data/hibernia.json:
```
armchar -s Data/hibernia.json 
```

The test files are included in the distribution.
Note that everything is defined in the saga file, including output
directories.

Although other options are defined in the source code (`src/harm.hs`),
these are not used at present.  The only use case supported is the
generation of web pages (markdown) from a collection of files defined
by the saga file specified by `-s`.

## Build

If you want to build the software yourself, you should download the Haskell
Platform.  To build and run the example above, use for instance,
```
cabal run armchar -- -s Data/hibernia.json 
```
