{ mkDerivation, base, charset, parsec, parsers, semigroupoids
, stdenv, tasty, tasty-discover, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "separated-values";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base charset parsers semigroupoids ];
  testHaskellDepends = [
    base parsec parsers tasty tasty-discover tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/data61/separated-values";
  description = "Values which are separated, often by commas";
  license = stdenv.lib.licenses.bsd3;
}
