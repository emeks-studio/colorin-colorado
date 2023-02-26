{
  # Initially this is a template created by `hix init` (nix flake init --template templates#haskell-nix)
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache (and others).
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://cache.iog.io"
      "https://pre-commit-hooks.cachix.org"
      "https://emeks-public.cachix.org"
      # ^ Use https://docs.cachix.org/pushing#pushing-flake-inputs to update this!
      # TODO: Push to the cache in order to save time!
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "emeks-public.cachix.org-1:sz2oZuYq7EsRb5FW6sDtpPU1CWh+6ymOgxFgmrYTKGI="
    ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0;37m\\](\\[\\e[0m\\]nix) \\[\\e[0;1;91m\\]colorin-colorado \\[\\e[0m\\]\\w \\[\\e[0;1m\\]λ \\[\\e[0m\\]";
  };

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/986ebab4075d4c1ecac1270a9304e65e7ef715bc";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        # Not for now!
        # "x86_64-darwin"
        # "aarch64-linux"
        # "aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            ormolu.enable = true;
          };
        };
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = system;
                compiler-nix-name = "ghc925";
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  ormolu = { };
                  # https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html
                  haskell-language-server = "1.9.1.0";
                };
                shell.shellHook = ''
                  echo "Installing pre-commit hooks";
                  ${pre-commit-check.shellHook}
                '';
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake { };
      in
      flake // {
        legacyPackages = pkgs;

        packages.default = flake.packages."colorin-colorado:exe:colorin-colorado";

        # Execute with "nix flake check"
        checks = {
          inherit pre-commit-check;
        };
      });
}
