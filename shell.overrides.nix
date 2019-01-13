self: super:
let lib = self.haskell.lib; in
{
  haskellPackages = super.haskellPackages.override (args: {
    overrides = self: super:
      (args.overrides or (self: super: super)) self super // {
        ghcWithPackages = self.ghcWithHoogle;
      };
  });
}
