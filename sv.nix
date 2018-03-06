{ mkDerivation, ansi-wl-pprint, attoparsec, base, bifunctors
, bytestring, charset, containers, contravariant, deepseq, hedgehog
, lens, mtl, parsec, parsers, profunctors, readable, semigroupoids
, semigroups, stdenv, tasty, tasty-hedgehog, tasty-hunit, text
, transformers, trifecta, utf8-string, validation, vector, void
}:
mkDerivation {
  pname = "sv";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint attoparsec base bifunctors bytestring charset
    containers contravariant deepseq lens mtl parsec parsers
    profunctors readable semigroupoids semigroups text transformers
    trifecta utf8-string validation vector void
  ];
  testHaskellDepends = [
    ansi-wl-pprint base bytestring contravariant hedgehog lens parsers
    semigroupoids semigroups tasty tasty-hedgehog tasty-hunit text
    trifecta utf8-string validation vector
  ];
  homepage = "https://github.com/qfpl/sv";
  description = "Encode and decode separated values (CSV, PSV, ...)";
  license = stdenv.lib.licenses.bsd3;
}
