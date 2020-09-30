let
  pkgs = import ./nix/pkgs.nix;
in
{
  inherit (pkgs) dnscheck;
  pre-commit-check = (import ./nix/pre-commit-hooks.nix).run;
}
