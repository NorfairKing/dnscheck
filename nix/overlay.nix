final: previous:
with final.lib;
with final.haskell.lib;

{
  dnscheck =
    failOnAllWarnings (
      disableLibraryProfiling (final.haskellPackages.callCabal2nixWithOptions "dnscheck" (final.gitignoreSource ../dnscheck) "--no-hpack" {})
    );
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super:
                {
                  inherit (final) autorecorder;
                }
            );
        }
    );
}
