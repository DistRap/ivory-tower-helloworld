{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "ecae8db03b3b9aee5dd4eac085aa540286239743";
    sha256 = "1wd4lkw6kax5x99a321w0affvckmirjv637ycpnr5jnmcly45zsk";
  };

  itn = import itnSrc { inherit compiler; };

  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  itn // {
    shell = itn.mkShell itn.ivorypkgs.ivory-tower-helloworld;
  }
