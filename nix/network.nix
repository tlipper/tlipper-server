rec {
  network.description = "Tlipper Server";
  tlipperserver = { config, pkgs, ... }:
  let
    tlipper = import ../default.nix;
    prometheusConfig = ../prometheus.yml;
  in
  {
    networking.hostName = "tlipper";
    networking.firewall.allowedTCPPorts = [ 80 22 9090 8080 8081 3000 8082 ];
    environment.systemPackages = [ tlipper.tlipper-server ];
    services.postfix = {
      enable = true;
    };
    services.grafana = {
      enable = true;
      port = 3000;
      addr = "0.0.0.0";
      domain = "0.0.0.0";
      smtp = {
        enable = true;
        fromAddress = "admin@grafana.tlipper";
      };
      provision = {
        enable = true;
        datasources = [
          {
            name = "Prometheus";
            type = "prometheus";
            url = "http://0.0.0.0:9090";
          }
        ];
      };
    };
		services.postgresql = {
			enable = true;
			package = pkgs.postgresql_10;
			enableTCPIP = true;
			authentication = pkgs.lib.mkOverride 10 ''
				local all all trust
				host all all ::1/128 trust
			'';
			initialScript = pkgs.writeText "backend-initScript" ''
				CREATE ROLE postgres WITH LOGIN PASSWORD 'postgres' CREATEDB;
				CREATE DATABASE tlipper;
				GRANT ALL PRIVILEGES ON DATABASE tlipper TO postgres;
			'';
		};
    systemd.services.tlipper = {
      description = "tlipper Webserver";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = ''
          ${tlipper.tlipper-server}/bin/tlipper-server-exe
        '';
        EnvironmentFile = "${builtins.path { name = "dotenv"; path = ../.env; }}";
      };
    };
    systemd.services.prometheus = {
      description = "tlipper Prometheus Monitoring";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = ''
          ${pkgs.prometheus}/bin/prometheus --config.file=${prometheusConfig}
        '';
        EnvironmentFile = "${builtins.path { name = "dotenv"; path = ../.env; }}";
      };
    };
  };
}
