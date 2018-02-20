{ mkDerivation, ansi-wl-pprint, attoparsec, base, bifunctors
, bytestring, charset, containers, contravariant, deepseq, hedgehog
, lens, mtl, parsec, parsers, profunctors, readable, semigroupoids
, semigroups, stdenv, tasty, tasty-hedgehog, tasty-hunit, text
, transformers, trifecta, utf8-string, validation, vector, void
}:
mkDerivation {
  pname = "sv";
  version = "0.1.0.0";
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
  description = "Values which are separated, often by commas";
  license = stdenv.lib.licenses.bsd3;
}
