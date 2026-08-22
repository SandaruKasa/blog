{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  __structuredAttrs = true;
  strictDeps = true;
  preferLocalBuild = true;

  name = "blog";
  packages = with pkgs; [
    hugo
  ];
}
