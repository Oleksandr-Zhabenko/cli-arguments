{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.mkDerivation {
  pname = "cli-arguments";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ pkgs.haskellPackages.base ];
  description = "A library to process command line arguments in some more convenient way";
  license = pkgs.lib.licenses.mit;
}
