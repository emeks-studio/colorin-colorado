# colorin-colorado

Binaries as 8-bit colored SVGs

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

```json
"haskell.serverExecutablePath": "/nix/store/59r8wk61czv5dm3avi118dimrmh57rn1-haskell-language-server-1.1.0.0/bin/haskell-language-server-8.10"
```

- Update hie.yaml

```
[nix-shell:~]$ gen-hie > hie.yaml
```