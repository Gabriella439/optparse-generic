{ mkDerivation, base, deepseq, stdenv }:
mkDerivation {
  pname = "Only";
  version = "0.1";
  sha256 = "0rdj3a629fk2vp121jq8mf2smkblrz5w3cxhlsyx6my2x29s2ymb";
  revision = "1";
  editedCabalFile = "f92f5da97e647451f1ee7f5bf44914fb75062d08ccd3f36b2000d649c63d13aa";
  libraryHaskellDepends = [ base deepseq ];
  description = "The 1-tuple type or single-value \"collection\"";
  license = stdenv.lib.licenses.bsd3;
}
