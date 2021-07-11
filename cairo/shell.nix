with (import ./nix/nixpkgs.nix);
let
  myHaskellPackages =  haskellPackages.extend (self: super: {
    gi-cairo-render = self.callPackage ./gi-cairo-render/default.nix { };
    gi-cairo-connector =  self.callPackage ./gi-cairo-connector/default.nix { }; });
  haskellDeps = ps: with ps; [
    gi-cairo-render
    gi-cairo-connector  
  ];
  ghc = myHaskellPackages.ghcWithPackages haskellDeps;
  nixPackages = [
    ghc
    haskellPackages.cabal-install
  ];
in
mkShell {
  buildInputs = [ 
    nixPackages    
  ];
}
