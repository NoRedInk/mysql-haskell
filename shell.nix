let pkgs = import nix/packages.nix;
in pkgs.mkShell {
  buildInputs = [
    (pkgs.haskell.packages.ghc884.ghcWithPackages (haskellPackges:
    with haskellPackges; [
      base
      binary
      binary-ieee754
      binary-parsers
      blaze-textual
      bytestring
      bytestring-lexing
      cryptonite
      io-streams
      memory
      monad-loops
      network
      scientific
      tcp-streams
      text
      time
      tls
      vector
      wire-streams
      word24
    ]))
    pkgs.cabal-install
    pkgs.haskellPackages.hpack
    pkgs.niv
    pkgs.zlib
    pkgs.haskellPackages.ghcid
    pkgs.ormolu
    pkgs.gnumake
    pkgs.hlint
  ];
}
