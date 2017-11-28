{ mkDerivation, base, bifunctors, bytestring, charset, containers
, hedgehog, lens, mtl, parsec, parsers, profunctors, QuickCheck
, readable, semigroupoids, semigroups, separated, stdenv, tasty
, tasty-hedgehog, tasty-hunit, tasty-quickcheck, text, text1
, transformers, trifecta, validation
}:
mkDerivation {
  pname = "sv";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors bytestring charset containers lens mtl parsers
    profunctors readable semigroupoids semigroups separated text text1
    transformers trifecta validation
  ];
  testHaskellDepends = [
    base bytestring hedgehog lens parsec parsers QuickCheck
    semigroupoids semigroups separated tasty tasty-hedgehog tasty-hunit
    tasty-quickcheck text text1 trifecta validation
  ];
  homepage = "https://github.com/qfpl/sv";
  description = "Values which are separated, often by commas";
  license = stdenv.lib.licenses.bsd3;
}
