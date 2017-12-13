# SV: Separated Values

<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

sv is for parsing and decoding CSV and similar formats (such as PSV).
It is called sv because it is useful for values separated by all sorts
of things; not just commas.

You should be able to use sv to read your CSV documents and marshal them into
rich Haskell data types - including not only products, but sums. Then write
your program in terms of those data types.

sv focuses on correctness, on flexible and composable decoder data types,
and on useful and informative errors.
Speed is also important to us, but not as important as the above.

sv chooses not to use type classes for decoders, since each data type might
be encoded in different ways in different files. Instead, decoders are
first-class values: a user has the power to create and pass decoders around.
This approach was inspired by hedgehog, which we use for testing.
In other design aspects, sv is somewhat inspired by argonaut.

sv tries not to be opinionated about how your data should look. We intend for
the user to have a great degree of freedom to build the right decoder for
their dataset. This focus often means that our types are heavily
parameterised. We have tried to provide hints in the haddock comments as to
how one would usually instantiate a given type.
