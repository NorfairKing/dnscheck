final: previous:
with final.lib;
with final.haskell.lib;

let dnscheck =
  buildStrictly (
    disableLibraryProfiling (final.haskellPackages.callCabal2nixWithOptions "dnscheck" (final.gitignoreSource ../dnscheck) "--no-hpack" { })
  );
in
{
  dnscheck = justStaticExecutables dnscheck;
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions (old.overrides or (_: _: { })) (
            self: super:
              {
                inherit dnscheck;
              }
          );
      }
    );
}
