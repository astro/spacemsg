{ lib, pkgs, config, ... }:

with lib;
{
  options.services.spaceapi = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable the SpaceAPI HTTP server";
    };
    spaceapiJson = mkOption {
      type = types.path;
      default = ./spaceapi.json;
    };
    wifiLocationsJson = mkOption {
      type = types.path;
      default = ./wifi-locations.json;
    };
  };

  config =
    let
      cfg = config.services.spaceapi;
      server = pkgs.callPackage ./default.nix {
        inherit pkgs;
        inherit (cfg) spaceapiJson wifiLocationsJson;
      };
    in mkIf cfg.enable {
      systemd.services.spaceapi = {
        description = "SpaceAPI server";
        wantedBy = [ "multi-user.target" ];
        after    = [ "network.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${server}/bin/spaceapi-server";
          Restart = "always";
          RestartSec = "10sec";

          AmbientCapabilities = "CAP_NET_BIND_SERVICE";
          DynamicUser = true;
          NoNewPrivileges = true;
          LimitNPROC = 64;
          LimitNOFILE = 256;
          CPUWeight = 5;
          MemoryMax = "512M";
          ProtectSystem = "full";
        };
     };
  };
}
