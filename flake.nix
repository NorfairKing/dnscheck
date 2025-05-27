{
  description = "dnscheck";
  nixConfig = {
    extra-substituters = "https://dnscheck.cachix.org";
    extra-trusted-public-keys = "dnscheck.cachix.org-1:dJSRH3SurLpWrwRLtx4nbxDvZT4dBgeB5NUxWg68HTY=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
  };
  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , weeder-nix
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.dnscheck;
      checks.${system} = {
        release = self.packages.${system}.default;
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit pkgs;
          dnscheck-nixos-module = self.nixosModules.${system}.default;
        };
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = [ "dnscheck" ];
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [
              ".*/default.nix"
              "dnscheck/options.nix"
            ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "dnscheck-shell";
        packages = p: [ p.dnscheck ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nixosModules.${system}.default = import ./nix/nixos-module.nix { dnscheck = self.packages.${system}.default; };
    };
}
