# SV: Separated Values

![CSIRO's Data61 Logo](https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png)

sv (separated values) is a library for parsing, decoding, encoding, and
printing CSV and similar formats (such as PSV, TSV, and many more).

sv uses an Applicative combinator style for decoding and encoding, rather
than a type class based approach. This means we can have multiple decoders
for the same type, multiple combinators of the same type, and we never have
to worry about orphan instances. These decoders can be stitched together from
provided primitives and combinators, or you can build one from a parser
from your favourite parser combinator library.

For parsing, sv uses <https://hackage.haskell.org/package/hw-dsv hw-dsv>, a high performance streaming CSV parser based on rank-select data structures.
sv works with UTF-8, and should work with CP-1252 as well. It does not work
with UTF-16 or UTF-32.

sv returns values for all errors that occur - not just the first. Errors have
more structure than just a string, indicating what went wrong.

sv tries not to be opinionated about how your data should look. We intend for
the user to have a great degree of freedom to build the right decoder for
their dataset.

Parts of sv are intended to be imported as follows:

```hs
import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E
```

## Examples:

* Decoding a real CSV: [Species.lhs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Species.lhs)
* Decoding by column name: [Columnar.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Columnar.hs)
* Encoding data to a CSV: [Encoding.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Encoding.hs)
* Encoding data with a header to a CSV: [EncodingWithHeader.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/EncodingWithHeader.hs)
* Handling "NULL" and "Unknown" occuring in a column of numbers: [Numbers.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Numbers.hs)
* Dealing with non-rectangular data: [Ragged.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Ragged.hs)
* Integrating with an existing attoparsec parser to read date stamps: [TableTennis.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/TableTennis.hs)

To get the best performance, the hw-dsv parser and its dependencies
underlying sv should be compiled with the flag @+bmi2@ to enable . These
libraries are:  @bits-extra@, @hw-rankselect@, @hw-rankselect-base@, and
@hw-dsv@. A simple way to set the flags for all of them when building with
cabal is to include a cabal.project file in your project containing
something like the following:

```
packages: .
package bits-extra
  flags: +bmi2
package hw-rankselect
  flags: +bmi2
package hw-rankselect-base
  flags: +bmi2
package hw-dsv
  flags: +bmi2
```
