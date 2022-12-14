# colorin-colorado

Binaries interpreted as color matrices or music notes (soon ?)

So far:

- Binary interpreted as a colored matrix

  - file formats: .svg
  - color palettes: "8-bit" (customizable), RGB and RGBA.

## Development

```
$ nix-shell
# ^ the first time this will take some time, meanwhile, you can drink mate and listen good cumbia

# Compile the project
[nix-shell:~]$ cabal v2-build

# Run (default) executable
[nix-shell:~]$ cabal v2-run
```

## Appendix

### Starting project from scratch

Add default.nix + .cabal file, then run nix-shell

Ref. https://srid.ca/haskell-nix 

### Better DevEx

Install vscode + Haskell extension (Haskell languague support) + ormolu (Haskell linter)

```
$ nix-shell

[nix-shell:~]$ code .
# ^ Running this inside nix-shell will allow work haskell extensions to work properly
```

### Dataset

Ref. https://corpus.canterbury.ac.nz/descriptions/#cantrbry

### Links about 8-bit palette

- https://lospec.com/palette-list/windows-95-256-colours

## Troubleshooting

- Fix vscode Haskell extension:

1. Search for haskell-language-server "out" path, according the version we use in this project:

```bash
[nix-shell:~]$ nix show-derivation nixpkgs.haskellPackages.haskell-language-server
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

2. then, manually setup `serverExecutablePath` (at extension settings):

Change default:

```json
"haskell.serverExecutablePath": "~/.config/Code/User/globalStorage/haskell.haskell/haskell-language-server-1.6.1.0-linux-8.8.4"
```

to:

```json
"haskell.serverExecutablePath": "/nix/store/59r8wk61czv5dm3avi118dimrmh57rn1-haskell-language-server-1.1.0.0/bin/haskell-language-server-8.10"
```

- Update hie.yaml

```
[nix-shell:~]$ gen-hie > hie.yaml
```