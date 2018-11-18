self: super:
let lib = self.haskell.lib; in
{
  haskellPackages = super.haskellPackages.override (args: {
    overrides = self: super:
      (args.overrides or (self: super: super)) self super // {
        # Tests cannot be run from Nix builder
        ghc-heap-view = lib.disableLibraryProfiling super.ghc-heap-view;
        ghc-datasize = lib.disableLibraryProfiling super.ghc-datasize;
      };
  });
}
