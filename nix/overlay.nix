final: prev:
with final.lib;
with final.haskell.lib;
{
  dnscheck = justStaticExecutables final.haskellPackages.dnscheck;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        dnscheck = buildStrictly (self.callPackage ../dnscheck { });
      }
    );
  });
}
