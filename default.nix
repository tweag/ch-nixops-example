{ mkDerivation, base, directory, distributed-process, filepath
, stdenv
}:
mkDerivation {
  pname = "dp-nixops-example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory distributed-process filepath
  ];
  license = stdenv.lib.licenses.bsd3;
}
