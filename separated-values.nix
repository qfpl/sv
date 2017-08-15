{ mkDerivation, base, bifunctors, charset, parsec, parsers, semigroupoids
, semigroups, separated, stdenv, tasty, tasty-discover, tasty-hunit
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
    base bifunctors parsec parsers semigroups tasty tasty-discover tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/separated-values";
  description = "Values which are separated, often by commas";
  license = stdenv.lib.licenses.bsd3;
}
