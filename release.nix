with (import ./nix/nixpkgs.nix); 
let
  myHaskellPackages = import ./nix/overrides.nix;
in myHaskellPackages.callPackage ./default.nix { }
