{ mkDerivation, attoparsec, base, bifunctors, bytestring
, containers, contravariant, deepseq, hedgehog, hw-dsv, lens, mtl
, parsec, parsers, profunctors, readable, semigroupoids, semigroups
, stdenv, tasty, tasty-hedgehog, tasty-hunit, text, transformers
, trifecta, utf8-string, validation, vector, void
}:
mkDerivation {
  pname = "sv";
  version = "0.2";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bifunctors bytestring containers contravariant
    deepseq hw-dsv lens mtl parsec profunctors readable semigroupoids
    semigroups text transformers trifecta utf8-string validation vector
    void
  ];
  testHaskellDepends = [
    base bytestring contravariant hedgehog lens parsers semigroupoids
    semigroups tasty tasty-hedgehog tasty-hunit text trifecta
    utf8-string validation vector
  ];
  homepage = "https://github.com/qfpl/sv";
  description = "Encode and decode separated values (CSV, PSV, ...)";
  license = stdenv.lib.licenses.bsd3;
}
