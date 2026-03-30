{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = with pkgs; [
    cargo
    cargo-edit
    rustc
    rustfmt
    clippy
    rust-analyzer
  ];

  env.RUST_SRC_PATH = pkgs.rustPlatform.rustLibSrc;
}
