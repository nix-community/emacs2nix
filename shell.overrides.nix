self: super:
{
  haskellPackages = super.haskellPackages.override (args: {
    overrides = self: super_:
      let
        super = (args.overrides or (self: super: super)) self super_;
      in
        super // {
          ghcWithPackages = self.ghcWithHoogle;
        };
  });
}
