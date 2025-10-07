{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  name = "blog";
  packages = with pkgs; [
    hugo
  ];
}
