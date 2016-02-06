{ mkDerivation, base, directory, distributed-process, filepath
, network-transport-tcp, stdenv
}:
mkDerivation {
  pname = "ch-nixops-example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory distributed-process filepath network-transport-tcp
  ];
  license = stdenv.lib.licenses.bsd3;
}
