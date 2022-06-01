{ pkgs ? import <nixpkgs> {},
  spaceapiJson ? ./spaceapi.json,
  wifiLocationsJson ? ./wifi-locations.json
}:
with pkgs;

let
  server =
    haskellPackages.mkDerivation {
      pname = "spaceapi-server";
      version = "0.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = with haskellPackages; [
        aeson base bytestring cereal hashable http-client http-types
        network old-time scientific stm text unordered-containers vector
        yesod warp
      ];
      license = lib.licenses.agpl3Plus;
    };
in
  stdenv.mkDerivation {
    name = "spaceapi";
    src = ./.;
    buildInputs = [ server makeWrapper ];
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/bin $out/share/spaceapi

      makeWrapper ${server}/bin/server $out/bin/spaceapi-server \
        --run "cd $out/share/spaceapi"

      cp ${spaceapiJson} $out/share/spaceapi/spaceapi.json
      cp ${wifiLocationsJson} $out/share/spaceapi/wifi-locations.json
    '';
  }
