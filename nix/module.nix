{ lib, pkgs, config, ... }:
with lib;

let
  dnscheck = (import ../.).dnscheck;
  dnsCheckConfig = ./dns-check-config.yaml;
  cfg = config.services.dns-checker;
  dnscheckName = "dns-checker";
  dnscheckTimer = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "minutely";
      Persistent = true;
    };
  };
  configFile = pkgs.writeText "dnscheck-config-file" ''
    checks: ${builtins.toJSON cfg.checks}
    ${cfg.extraVerbatimConfig}
  '';
  dnscheckService = {
    description = "DNS Checker";
    path = with pkgs; [
      dnscheck
    ];
    serviceConfig = {
      User = "syd";
    };
    script = ''
      set +e # We need error codes to work this way.

      tmpfile=~/.dns-checker
      ${dnscheck}/bin/dnscheck ${configFile} 2>&1 > "$tmpfile"
      if [[ "$?" != "0" ]]
      then
        cat "$tmpfile" | ${cfg.notifyCommand}
      fi
      rm -f "$tmpfile"
    '';
  };

in
{
  options.services.dns-checker = {
    enable = mkEnableOption "DNS Checker";
    checks = mkOption {
      default = [];
      type = types.listOf (
        types.submodule {
          options = {
            type = mkOption {
              type = types.str;
              example = "a";
              description = "The type of record to check";
            };
            domain = mkOption {
              type = types.str;
              example = "cs-syd.eu";
              description = "The domain to query";
            };
            values = mkOption {
              type = types.listOf types.str;
              example = [ "52.211.121.166" ];
              default = [];
              description = "The value to expect in the dns response";
            };
          };
        }
      );
    };
    extraVerbatimConfig = mkOption {
      type = types.str;
      default = "";
      description = "Extra configuration, verbatim";
    };
    notifyCommand = mkOption {
      type = types.str;
      default = "false";
      description = "The command to pass the output to in case of a failure";
    };
  };
  config.systemd = mkIf cfg.enable {
    timers = { "${dnscheckName}" = dnscheckTimer; };
    services = { "${dnscheckName}" = dnscheckService; };
  };
}
