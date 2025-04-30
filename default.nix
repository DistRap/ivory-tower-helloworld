{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "ca278bb06a3ad5208d99a8d5cdb7222e36d7d734";
    sha256 = "02kmi4h8f8igjs48z5qgfk33g4gddi9biwzyb9439bki10varvlm";
  };

  itn = import itnSrc { inherit compiler; };

  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  itn // {
    shell = itn.mkShell itn.ivorypkgs.ivory-tower-helloworld;
  }
