# Pipes-Parse v3.0.6

`pipes-parse` builds upon
[the `pipes` library](https://github.com/Gabriel439/Haskell-Pipes-Library) to
provide shared streaming parsing utilities.

## Quick start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install pipes-parse`
* `cabal install lens-family-core`

Then fire up `ghci`:

    Prelude> import Pipes
    Prelude Pipes> import Pipes.Parse as Parse
    Prelude Pipes Parse> import Lens.Family.State.Strict as Lens

... and draw the first group of consecutive equal elements:

    Prelude Pipes Parse Lens> evalStateT (zoom group drawAll) (each [1, 1, 2, 3])
    [1,1]

The official tutorial is on
[Hackage](http://hackage.haskell.org/package/pipes-parse).

## Features

* *Leftovers*: Save unused input for later consumption

* *Leftover propagation*: Leftovers are propagated backwards perfectly

* *Connect and Resume*: Use `StateT` to save unused input for later

* *Termination Safety*: Detect and recover from end of input

## Outline

`pipes-parse` provides the core idioms for how to parse streams using `pipes`.
Stream-specific parsers are provided in downstream libraries, such as
`pipes-bytestring` and `pipes-text`.

## Development Status

The core mechanism behind `pipes-parse` is stable.  I may accept requests for
additional generic parsers, but the central idioms will not change.  Most
development work concentrates on downstream libraries to provide stream-specific
parsing utilities.

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
