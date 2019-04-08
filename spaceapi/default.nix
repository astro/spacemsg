{ pkgs ? import <nixpkgs> {} }:
with pkgs;

haskellPackages.mkDerivation {
  pname = "server";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = with haskellPackages; [
    aeson base bytestring cereal ekg hashable http-client http-types
    network old-time scientific stm text unordered-containers vector
    yesod
  ];
  license = stdenv.lib.licenses.agpl3Plus;
}
