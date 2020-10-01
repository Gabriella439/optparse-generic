{ mkDerivation, base, bytestring, Only, optparse-applicative
, stdenv, system-filepath, text, time, transformers, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.4.4";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring Only optparse-applicative system-filepath text time
    transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = stdenv.lib.licenses.bsd3;
}
