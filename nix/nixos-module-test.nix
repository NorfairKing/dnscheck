{ pkgs
, dnscheck-nixos-module
}:
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "dnscheck-module-test";
    nodes.machine = {
      imports = [ dnscheck-nixos-module ];
      services.dnscheck.enable = true;
    };
    testScript = ''
      machine.start()
      machine.wait_for_unit("multi-user.target")
      machine.systemctl("start dnscheck.service --wait")
    '';
  }
)
