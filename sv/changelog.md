# Revision history for sv

## 1.3.0.1 -- 2019-02-26

* Fix an occasionally failing property-test

## 1.3 -- 2019-01-14

* Update to sv-core 0.4

## 1.2 -- 2018-09-26

* Update to sv-core 0.3

## 1.1.1 -- 2018-08-10

* Depend on sv-core 0.2.1 to get column-name-based encoding

## 1.1 -- 2018-07-25

* Add column-name-based decoding by adding parseDecodeNamed and associated functions
* Support sv-core 0.2

## 1.0 -- 2018-07-19

* Split the package into sv, sv-core, svfactor. This removes Data.Sv.{Parse,Print,Syntax}
  and changes types in Data.Sv.Decode and Data.Sv.Encode.
  Compatibility with version 0.1 is minimal

## 0.1 -- 2018-03-06

* First version. Released on an unsuspecting world.
