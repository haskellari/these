{ cabal, bifunctors, mtl, profunctors, semigroupoids, semigroups
, transformers, vector
}:

cabal.mkDerivation (self: {
  pname = "these";
  version = "0.4.1";
  sha256 = "135qiyg87cf9rl10zb681mwnrwxdm37h76dnlia8amkkmpkg4wia";
  buildDepends = [
    bifunctors mtl profunctors semigroupoids semigroups transformers
    vector
  ];
  meta = {
    homepage = "https://github.com/isomorphism/these";
    description = "An either-or-both data type, with corresponding hybrid error/writer monad transformer";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
