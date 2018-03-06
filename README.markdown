# SV: Separated Values

<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

sv is a swiss army knife for CSV and similar formats (such as PSV, TSV,
and many more). It can parse, decode, encode, and print these formats.

sv uses an Applicative combinator style for decoding and encoding, rather
than a type class based approach. This means we can have multiple decoders
for the same type, multiple combinators of the same type, and we never have
to worry about orphan instances. These decoders can be stiched together from
provided primitives and combinators, or you can build one from a parser
from your favourite parser combinator library.

sv returns values for all errors that occur - not just the first. Errors have
more structure than just a string, indicating what went wrong.

sv's parser is exposed so you can use it independently of the decoding, and
encoding and printing are similarly standalone.

sv focuses on correctness, on flexible and composable data types,
and on useful and informative error values.
Speed is also important to us, but it is not as important as these other
qualities.

sv tries not to be opinionated about how your data should look. We intend for
the user to have a great degree of freedom to build the right decoder for
their dataset.

sv is intended to be imported as follows:

```hs
import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E
```

## Examples:

* Decoding a real CSV: [Species.lhs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Species.lhs)
* Encoding data to a CSV: [Encoding.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Encoding.hs)
* Handling "NULL" and "Unknown" occuring in a column of numbers: [Numbers.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Numbers.hs)
* Dealing with non-rectangular data: [Ragged.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Ragged.hs)
* Handling multiple logical documents in one file: [Concat.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Concat.hs)
* Integrating with an existing attoparsec parser to read date stamps: [TableTennis.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/TableTennis.hs)
* Fixing inconsistent formatting with lenses: [Requote.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Requote.hs)
