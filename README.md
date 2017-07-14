# optparse-generic v1.2.2

Use this library to auto-generate a command-line interface that parses a typed
value.  This library uses Haskell's support for generic programming to
customize the command-line interface to the type of value that you request.

For example, the following program:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Generic

data Example = Example { foo :: Int, bar :: Double } deriving (Generic, Show)

instance ParseRecord Example

main = do
    x <- getRecord "Test program"
    print (x :: Example)
```

... will generate a command-line interface that has one flag per field of the
record:

```bash
$ stack build optparse-generic
$ stack runghc Example.hs -- --help
Test program

Usage: Example.hs --foo INT --bar DOUBLE

Available options:
  -h,--help                Show this help text

$ stack runghc Example.hs -- --foo 1 --bar 2.5
Example {foo = 1, bar = 2.5}
```

This library also provides support out-of-the-box for many existing Haskell
types.  Try to run this program to see what command-line interface it generates:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Generic

main = do
    x <- getRecord "Test program"
    print (x :: Either Double Int)
```

This library tries to be as intelligent as possible:

* Unlabeled fields become positional arguments
* Data types with multiple constructors auto-generate subcommands
* The `Maybe` type constructor translates to an optional flag/argument
* The `[]` type constructor translates to a repeated flag/argument
* `Any`/`All`/`First`/`Last`/`Sum`/`Product` also translate to repeated
  arguments, but with different behaviors (i.e. `First` will return the first
  option/argument that matches and `Sum` will sum them all)

For the full tutorial, read the
[Hackage documentation](http://hackage.haskell.org/package/optparse-generic/docs/Options-Generic.html)

## Development status

[![Build Status](https://travis-ci.org/Gabriel439/Haskell-Optparse-Generic-Library.png)](https://travis-ci.org/Gabriel439/Haskell-Optparse-Generic-Library)

I expect this library's API to be reasonably stable, but only time will tell.
Most changes will likely be related to adding new built-in support for
existing commonly used data types in the Haskell ecosystem by adding new
`ParseField`, `ParseFields`, or `ParseRecords` instances, but this will not
require any breaking changes to the API.

## LICENSE (BSD 3-Clause)

Copyright (c) 2016 Gabriel Gonzalez  
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of Gabriel Gonzalez nor the names of other contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

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
