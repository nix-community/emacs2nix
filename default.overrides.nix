self: super:
let lib = self.haskell.lib; in
{
  haskellPackages = super.haskellPackages.override (args: {
    overrides = self: super:
      (args.overrides or (self: super: super)) self super // {
        ghc-heap-view = lib.disableLibraryProfiling super.ghc-heap-view;
        ghc-datasize = lib.disableLibraryProfiling super.ghc-datasize;
        hnix = lib.dontCheck (self.callPackage ./hnix.nix {});
        ref-tf = lib.doJailbreak super.ref-tf;
        these = lib.dontCheck super.these;
      };
  });
}
