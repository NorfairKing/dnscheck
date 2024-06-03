final: prev:
with final.lib;
with final.haskell.lib;
{
  dnscheck =
    let
      enableStatic = pkg:
        if final.stdenv.hostPlatform.isMusl
        then
          overrideCabal pkg
            (old: {
              configureFlags = (old.configureFlags or [ ]) ++ [
                "--ghc-option=-optl=-static"
                # Static
                "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
                "--extra-lib-dirs=${final.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
                "--extra-lib-dirs=${final.zlib.static}/lib"
                "--extra-lib-dirs=${final.ncurses.override { enableStatic = true; }}/lib" # for -ltinfo
              ];
              enableSharedExecutables = false;
              enableSharedLibraries = false;

              postInstall = (old.postInstall or "") + ''
                for b in $out/bin/*
                do
                  if ldd "$b"
                  then
                    echo "ldd succeeded on $b, which may mean that it is not statically linked"
                    exit 1
                  fi
                done
              '';
            })
        else pkg;
    in
    justStaticExecutables (enableStatic final.haskellPackages.dnscheck);
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        let
          fixGHC = pkg:
            if final.stdenv.hostPlatform.isMusl
            then
              pkg.override
                {
                  # To make sure that executables that need template
                  # haskell can be linked statically.
                  enableRelocatedStaticLibs = true;
                  enableShared = false;
                  enableDwarf = false;
                }
            else pkg;
        in
        {
          ghc = fixGHC super.ghc;
          buildHaskellPackages = old.buildHaskellPackages.override (oldBuildHaskellPackages: {
            ghc = fixGHC oldBuildHaskellPackages.ghc;
          });


          dnscheck = buildStrictly (self.callPackage ../dnscheck { });
        }
    );
  });
}
