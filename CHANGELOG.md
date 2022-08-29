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

Install vscode + haskell extension (Haskell languague support) + ormolu

```
$ nix-shell

[nix-shell:~]$ code .
# ^ Running this inside nix-shell will allow work haskell extensions to work properly
```