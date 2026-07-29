# armchar-swish

ArM character generator implemented in declarative programming,
using Haskell.  This is work in progress, and is best documented
by various sample files.  See for instance the
[harm-test](https://github.com/ars-magica/harm-test/) repository.

The [documentation site](https://ars-magica.github.io/) is also
work in progress.

## Download

Precompiled binaries are available from the releases
(right hand side on github).
+ `harm` is the executable for Linux
+ `harm-macos` is the executable for MacOS
+ `harm.exe` is the executable for Windows

I have only had opportunity to test on Linux.

## Build

Obviously, if you have the haskell platform and cabal installed,
you can clone the repository and compile yourself:
```sh
cabal build harm
```
or if you have trouble with incompatible dependencies, you can
force cabal to disregard them
```sh
cabal build harm --allow-newer=base --allow-newer=template-haskell
```
This is an issue if you have a new compiler, for which the libraries
have not been tested.

## Usage

Sample files are provided in the Data subdirectory.
The following command builds all the character sheets for the
Hibernia saga defined in `Data/hibernia.yaml:
```
cabal -s Data/hibernia.yaml 
```

Although other options are defined in the source code (`src/harm.hs`),
these are not used at present.  The only use case supported is the
generation of web pages (markdown) from a collection of files defined
by the saga file specified by `-s`.

