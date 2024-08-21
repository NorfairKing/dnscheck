{ pkgs
, dnscheck-nixos-module
}:
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "dnscheck-module-test";
    nodes.machine = {
      imports = [ dnscheck-nixos-module ];
      services.dnscheck.enable = true;
      services.dnscheck.config.checks = [ ];
    };
    testScript = ''
      machine.start()
      machine.wait_for_unit("multi-user.target")
      machine.systemctl("start dnscheck.service --wait")
      machine.require_unit_state("dnscheck.service", "inactive")
    '';
  }
)
