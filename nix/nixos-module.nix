{ dnscheck
}:
{ lib
, pkgs
, config
, ...
}:
with lib;

{
  options.services.dnscheck = {
    enable = mkEnableOption "DNS Checker";
    config = mkOption {
      # Turned off type-checking until this type-checking bug is fixed:
      # https://github.com/NixOS/nixpkgs/issues/337108
      # type = import ../dnscheck/options.nix { inherit lib; };
    };
    onCalendar = mkOption {
      type = types.str;
      default = "hourly";
      description = "The OnCalendar part of the systemd timer";
    };
    notifyCommand = mkOption {
      type = types.str;
      default = "false";
      description = "The command to pass the output to in case of a failure";
    };
  };
  config =
    let
      cfg = config.services.dnscheck;
      dnscheckName = "dnscheck";
      dnscheckTimer = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.onCalendar;
          Persistent = true;
        };
      };
      configFile = (pkgs.formats.yaml { }).generate "dnscheck-config.yaml" cfg.config;

      settingsCheck = pkgs.runCommand "dnscheck-settings-check" { } ''
        ${dnscheck}/bin/dnscheck ${configFile} --help > $out
      '';
      dnscheckService = {
        description = "DNS Checker";
        path = [ dnscheck ];
        script = ''
          # We need error codes to work this way.
          # We also need this to make sure that the service still fails
          # correctly when the notification command fails.
          set +e
          set -x # See what's happening.

          tmpfile=~/.dnscheck
          ${dnscheck}/bin/dnscheck ${configFile} 2>&1 > "$tmpfile"
          exitcode="$?"
          if [[ "$exitcode" != "0" ]]
          then
            cat "$tmpfile" | ${cfg.notifyCommand}
            rm -f "$tmpfile"
            # Make sure the service still fails.
            exit "$exitcode"
          fi
        '';
        documentation = [ "${settingsCheck}" ];
      };
    in
    mkIf cfg.enable {
      environment.systemPackages = [ dnscheck ];
      systemd = {
        timers = { "${dnscheckName}" = dnscheckTimer; };
        services = { "${dnscheckName}" = dnscheckService; };
      };
    };
}
