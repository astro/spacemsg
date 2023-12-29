let
  pkgs = import <nixpkgs> { };
  crossPkgs = import <nixpkgs> {
    crossSystem = {
      config = "armv6l-unknown-linux-musleabi";
    };
  };
in crossPkgs.mkShell {
  nativeBuildInputs = [ pkgs.cargo ];
}
