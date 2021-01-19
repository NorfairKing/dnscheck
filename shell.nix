let
  pkgs = import ./nix/pkgs.nix;
  pre-commit = import ./nix/pre-commit-hooks.nix;

in
pkgs.haskell.lib.buildStackProject {
  name = "dnscheck";
  buildInputs = with pkgs; [
    zlib
  ] ++ pre-commit.tools;
  shellHook = ''
    ${pre-commit.run.shellHook}
  '';
}
