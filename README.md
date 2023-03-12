# colorin-colorado

Files interpreted as SVG images and/or music notes (soon ?)

### Dataset

Ref. https://corpus.canterbury.ac.nz/descriptions/#cantrbry

## Development

1. Enter nix development shell:

```bash
nix develop
```

Obs: The first time this will take some time, meanwhile, you can drink mate and listen good cumbia

2. Compile the project

```bash 
# inside nix shell
cabal build
```

3. Run image-encoder executable

```bash 
# inside nix shell
cabal run image-encoder
```

### Update cachix

```bash
nix develop --profile colorin-colorado-dev-profile
# inside nix shell
export CACHIX_AUTH_TOKEN=$AUTH_TOKEN; cachix push emeks-public colorin-colorado-dev-profile 
```

Ref. https://docs.cachix.org/pushing#pushing-flake-inputs

## Troubleshooting

- Fix VSCODE Haskell extension:

1. Search for haskell-language-server "out" path, according the version we use in this project:

```bash
nix show-derivation nixpkgs.haskellPackages.haskell-language-server
```

```json
{
  "/nix/store/m4q0ir6jh74srjil52wff5944fb0i6bb-haskell-language-server-1.1.0.0.drv": {
    "outputs": {
      "doc": {
        "path": "/nix/store/hqqjczdnm7jqwgjm5phln98a9cblbh0m-haskell-language-server-1.1.0.0-doc"
      },
      "out": {
        "path": "/nix/store/59r8wk61czv5dm3avi118dimrmh57rn1-haskell-language-server-1.1.0.0"
      }
    },
    ...
```

2. Then, manually setup `serverExecutablePath` at extension (workspace) settings:

Change at `.vscode/settings.json`:

```json
"haskell.serverExecutablePath": "~/.config/Code/User/globalStorage/haskell.haskell/haskell-language-server-1.6.1.0-linux-8.8.4"
```

to:

```json
"haskell.serverExecutablePath": "/nix/store/59r8wk61czv5dm3avi118dimrmh57rn1-haskell-language-server-1.1.0.0/bin/haskell-language-server-8.10"
```
