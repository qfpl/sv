{ mkDerivation, attoparsec, base, bifunctors, bytestring, cassava
, contravariant, hedgehog, hw-dsv, lens, Only, parsers
, semigroupoids, semigroups, stdenv, sv-core, tasty, tasty-hedgehog
, tasty-hunit, text, transformers, trifecta, utf8-string
, validation, vector
}:
mkDerivation {
  pname = "sv";
  version = "0.2";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bifunctors bytestring contravariant hw-dsv
    semigroupoids sv-core transformers utf8-string validation
  ];
  testHaskellDepends = [
    base bytestring cassava contravariant hedgehog lens Only parsers
    semigroupoids semigroups tasty tasty-hedgehog tasty-hunit text
    trifecta utf8-string validation vector
  ];
  homepage = "https://github.com/qfpl/sv";
  description = "Encode and decode separated values (CSV, PSV, ...)";
  license = stdenv.lib.licenses.bsd3;
}
