{ mkDerivation, base, bifunctors, charset, hedgehog, parsec, parsers, semigroupoids
, semigroups, separated, stdenv, tasty, tasty-hunit
, tasty-quickcheck
}:
mkDerivation {
  pname = "separated-values";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors charset parsers semigroupoids semigroups
  ];
  testHaskellDepends = [
    base bifunctors hedgehog parsec parsers semigroups tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/separated-values";
  description = "Values which are separated, often by commas";
  license = stdenv.lib.licenses.bsd3;
}
