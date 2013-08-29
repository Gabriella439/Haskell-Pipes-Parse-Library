# Pipes-Parse v2.0.0

`pipes-parse` builds upon
[the `pipes` library](https://github.com/Gabriel439/Haskell-Pipes-Library) to
provide shared streaming parsing utilities.

## Quick start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install pipes-parse`

Then fire up `ghci`:

    Prelude> import Pipes
    Prelude Pipes> import qualified Pipes.Prelude as P
    Prelude Pipes P> import qualified Pipes.Parse as P

... and limit standard input to first three consecutive groups of equal lines:

    Prelude Pipes P P> let threeGroups = P.concat . P.takeFree 3 . P.groupBy (==)
    Prelude Pipes P P> run $ threeGroups P.stdin >-> P.stdout
    Group1<Enter>
    Group1
    Group1<Enter>
    Group1
    Group2<Enter>
    Group2
    Group3<Enter>
    Group3
    Group3<Enter>
    Group3
    Group4<Enter>
    Prelude Pipes P P> -- Done, because we began entering our fourth group

The official tutorial is on
[Hackage](http://hackage.haskell.org/package/pipes-parse).

## Features

* *Perfect Streaming*: Program in a list-like style in constant memory

* *Concise Parsing API*: Just use `draw` and `unDraw` or `input`

* *Leftovers*: Save unused input for later consumption

* *Connect and Resume*: Use `StateT` to save unused input for later

* *Termination Safety*: Detect and recover from end of input

## Outline

`pipes-parse` provides generic examples for how to parse streams using `pipes`.
Datatype-specific parsers belong in separate libraries.  The purpose behind
this library is to provide example idioms that other libraries can reuse,
as well as providing useful tools for generic stream parsing.  Typically,
stream-specific parsing libraries will need to redefine specialized versions of
the primitives in this library and will probably not need to depend on
`pipes-parse` at all.

## Development Status

`pipes-parse` is close to complete and will probably add a few more functions
over the new few months.  If the API does not make any backwards incompatible
changes by the end of 2013 then the library will be officially stabilized.

## Community Resources

Use the same resources as the core `pipes` library to learn more, contribute, or
request help:

* [Haskell wiki page](http://www.haskell.org/haskellwiki/Pipes)

* [Mailing list](mailto:haskell-pipes@googlegroups.com) ([Google Group](https://groups.google.com/forum/?fromgroups#!forum/haskell-pipes))

## How to contribute

* Build derived libraries

* Write `pipes-parse` tutorials

## License (BSD 3-clause)

Copyright (c) 2013 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
