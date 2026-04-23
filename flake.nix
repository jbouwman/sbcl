{
  description = "SBCL development environment (for building from source and hacking the runtime)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.mkShell {
          name = "sbcl-dev";

          nativeBuildInputs = with pkgs; [
            # Bootstrap Lisp: SBCL builds itself, so we need an existing SBCL
            # to act as XC host.
            sbcl

            # Core toolchain
            gcc
            gnumake
            binutils
            zstd
            pkgs.zlib

            # Texinfo is needed to build the manuals (optional, keeps
            # `make` happy if you invoke the doc targets).
            texinfo

            # Useful while debugging the runtime
            gdb
            pkg-config

            # Common scripting / search utilities used by the test harness
            git
            bash
            coreutils
            findutils
            gnugrep
            gnused
            gawk
            which
          ];

          shellHook = ''
            echo "sbcl-dev shell"
            echo "  host SBCL: $(sbcl --version)"
            echo "  CC:        $(cc --version | head -n1)"
            echo
            echo "Common commands:"
            echo "  sh make.sh --fancy                # full build"
            echo "  sh run-sbcl.sh                    # run the just-built SBCL in place"
            echo "  (cd tests && sh run-tests.sh ...) # run the test suite"
          '';
        };
      });
}
