with (import ./nixpkgs.nix); 
haskellPackages.extend (self: super: {
    gi-cairo-render = pkgs.haskell.lib.overrideCabal (self.callHackageDirect { 
        pkg = "gi-cairo-render";
        ver = "0.1.0"; 
        sha256 = "1igw50n0r1lalnsn89ydmliww35rpmq0hhl5kyz2s6n149k2famb"; } { }) (drv: {
          libraryPkgconfigDepends = [ pkgs.cairo ];
          preCompileBuildDriver = ''
          PKG_CONFIG_PATH+=":${cairo}/lib/pkgconfig"
          setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
          '';
          });
    gi-cairo-connector = pkgs.haskell.lib.overrideCabal (self.callHackageDirect { 
        pkg = "gi-cairo-connector";
        ver = "0.1.0"; 
        sha256 = "0pygmb85kzj463azr0mk6cvg7pivnwk9nkp5138i37ys91c4ri7h"; } { }) (drv: {
          libraryPkgconfigDepends = [ pkgs.cairo ];
          preCompileBuildDriver = ''
          PKG_CONFIG_PATH+=":${cairo}/lib/pkgconfig"
          setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
          ''; 
          });
    gi-gtk = pkgs.haskell.lib.overrideCabal (self.callHackageDirect { 
        pkg = "gi-gtk";
        ver = "3.0.36"; 
        sha256 = "17i2398yr3hg7ycja534j5pwr31jiakkqqvpwqmvfq84zpmsvnk5"; } { }) (drv: {
          libraryPkgconfigDepends = [ pkgs.gtk3 ];
          }); 
}) 
