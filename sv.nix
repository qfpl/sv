{ mkDerivation, ansi-wl-pprint, attoparsec, base, bifunctors
, binary, bytestring, bytestring-show, charset, containers
, contravariant, hedgehog, lens, mtl, parsec, parsers, profunctors
, QuickCheck, readable, semigroupoids, semigroups, separated
, stdenv, tasty, tasty-hedgehog, tasty-hunit, tasty-quickcheck
, text, transformers, trifecta, validation
}:
mkDerivation {
  pname = "sv";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint attoparsec base bifunctors binary bytestring
    bytestring-show charset containers contravariant lens mtl parsec
    parsers profunctors readable semigroupoids semigroups separated
    text transformers trifecta validation
  ];
  testHaskellDepends = [
    ansi-wl-pprint base bytestring hedgehog lens parsers QuickCheck
    semigroupoids semigroups separated tasty tasty-hedgehog tasty-hunit
    tasty-quickcheck text trifecta validation
  ];
  homepage = "https://github.com/qfpl/sv";
  description = "Parse and decode values which are separated, often by commas";
  license = stdenv.lib.licenses.bsd3;
}
