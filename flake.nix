{
  description = "tricc";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-22.11;

    crane = {
      url = github:ipetkov/crane;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = github:oxalica/rust-overlay;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = inputs@{ self, nixpkgs, crane, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };

        inherit (pkgs) lib;

        toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

        craneLib = (crane.mkLib pkgs).overrideToolchain toolchain;
        src = craneLib.cleanCargoSource (craneLib.path ./.);

        commonArgs = { inherit src; };

        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        tricc = craneLib.buildPackage (commonArgs // {
          inherit cargoArtifacts;
        });
      in
      {
        checks = {
          inherit tricc;

          clippy = craneLib.cargoClippy (commonArgs // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });

          fmt = craneLib.cargoFmt {
            inherit src;
          };
        };

        packages = {
          inherit tricc;
          default = tricc;
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ toolchain ];
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
