let
  pkgsv = import (import ./nixpkgs.nix);
  yamlparse-applicative-overlay =
    import (
      builtins.fetchGit (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  sydtest-overlay =
    import (
      builtins.fetchGit (import ./sydtest-version.nix) + "/nix/overlay.nix"
    );

in
pkgsv {
  overlays =
    [
      yamlparse-applicative-overlay
      sydtest-overlay
      (import ./gitignore-src.nix)
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
