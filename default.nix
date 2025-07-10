{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "6f8dee27bd564cb26639ec31e5fe3708cd98e279";
    sha256 = "1hyaapcmmk8795qrl6ml4fmmc23kl2k09q38nw0wdr0c55448wbm";
  };

  itn = import itnSrc { inherit compiler; };

  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  itn // {
    shell = itn.mkShell itn.ivorypkgs.ivory-tower-helloworld;
  }
