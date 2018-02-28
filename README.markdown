# SV: Separated Values

<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

sv is a swiss army knife for CSV and similar formats (such as PSV, TSV,
and many more).

sv focuses on correctness, on flexible and composable decoder data types,
and on useful and informative errors.
Speed is also important to us, but not as important as the above.

sv tries not to be opinionated about how your data should look. We intend for
the user to have a great degree of freedom to build the right decoder for
their dataset.

## Examples:

* Decoding a real CSV: [Species.lhs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Species.lhs)
* Encoding data to a CSV: [Encoding.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Encoding.hs)
* Handling "NULL" and "Unknown" occuring in a column of numbers: [Numbers.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Numbers.hs)
* Dealing with non-rectangular data: [Ragged.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Ragged.hs)
* Handling multiple logical documents in one file: [Concat.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Concat.hs)
* Integrating with an existing attoparsec parser to read date stamps: [TableTennis.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/TableTennis.hs)
* Fixing inconsistent formatting with lenses: [Requote.hs](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Requote.hs)
