# Pipes-Parse v2.0.0

`pipes-parse` builds upon
[the `pipes` library](https://github.com/Gabriel439/Haskell-Pipes-Library) to
provide shared streaming parsing utilities.

## Quick start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install pipes-parse`

Then fire up ` ghci`:

    $ ghci
    Prelude> import Pipes
    Prelude Pipes> import Pipes.Parse as Parse
    Prelude Pipes Parse> :set -XNoMonomorphismRestriction

... and parse only consecutive input elements less than 4:

    Prelude Pipes Parse> let printAll = run $ for input (liftIO . print)
    Prelude Pipes Parse> let parser   = zoom (spans (< 4)) printAll
    Prelude Pipes Parse> evalStateT parser (each [1..])
    1
    2
    3
    Prelude Pipes Pipes.Parse P> -- Done!

The official tutorial is on
[Hackage](http://hackage.haskell.org/package/pipes-parse).

## Features

* *Concise API*: Just use `draw` and `unDraw` or `input`

* *Termination Safety*: Detect and recover from end of input

* *Leftovers*: Save unused input for later consumption

* *Lens Support*: Zoom in on subsets of a stream using isomorphisms

## Outline

`pipes-parse` only provides the minimal tools necessary for datatype agnostic
parsing.  Datatype-specific parsers belong in derived libraries.  The purpose
behind this is to stabilize the parsing API, which is a crucial component of the
`pipes` ecosystem and also to avoid scope creep.

## Development Status

`pipes-parse` is complete and if the API does not change by the end of 2013 then
the library will be officially stabilized.

The only planned update to this library is to switch to a smaller `lens-core`
dependency for isomorphism support if Edward Kmett releases that package to
Hackage.  This will be a backwards compatible change.

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
