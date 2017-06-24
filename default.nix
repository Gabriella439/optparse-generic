{ mkDerivation, base, bytestring, Only, optparse-applicative
, semigroups, stdenv, system-filepath, text, time, transformers
, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.2.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring Only optparse-applicative semigroups
    system-filepath text time transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = stdenv.lib.licenses.bsd3;
}
