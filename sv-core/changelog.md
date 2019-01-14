# Revision history for sv-core

## 0.4 -- 2019-01-14

* Use attoparsec for decoding in `double`, which is faster and more accurate
* Deprecate `rational` decoder. `double` is now better in both speed and
  accuracy.
* Add `doubleFast`, a fast and correct way to render doubles. The only
  downside is that it does not satisfy a round-trip property with any
  decoder.

## 0.3.1 -- 2019-01-04

* Add `read` and `read'` decoders

## 0.3 -- 2018-09-26

* Re-release 0.2.2 as 0.3 due to a change in error messages that
  broke sv's tests

## 0.2.2 -- 2018-08-23

* Add `rational` and `withTextReadable`.
  `rational` can be more accurate than `double` but is slower.

## 0.2.1 -- 2018-08-10

* Add column-name-based encoding. NameEncode, NameEncode', and
  related functions

## 0.2 -- 2018-07-25

* Add column-name-based decoding NameDecode, NameDecode', and associated
  functions such as 'column'.
* Add some extra constructors to DecodeError for new errors

## 0.1 -- 2018-07-19

* Split off from sv-0.1
