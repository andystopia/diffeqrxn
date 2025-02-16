{
  description = "A basic haskell cabal template dev env with hls";

  # Nixpkgs / NixOS version to use.
  inputs.nixpkgs.url = "nixpkgs/nixos-24.05";

  nixConfig = {
    allowBroken = true;
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    # System types to support.
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];

    # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

    # Nixpkgs instantiated for supported system types.
    nixpkgsFor = forAllSystems (system: import nixpkgs {inherit system;});
  in {
    # Provide some binary packages for selected system types.
    packages = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};

      diffeqrxn = import ./build-diffeqrxn.nix {pkgs = pkgs;};
    in {
      default = diffeqrxn.default;
      # static = diffeqrxn.haskell-static;
      # nixpkgsStatic = (import ./build-diffeqrxn.nix {pkgs = pkgs.pkgsStatic;}).default;
    });

    # Add dependencies that are only needed for development
    devShells = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
    in {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          starship
          # put other package below
          cabal-install
          haskell.compiler.ghc96
          haskellPackages.implicit-hie
        ];

        nativeBuildInputs = with pkgs; [
          haskell.compiler.ghc96
          haskell-language-server
        ];

        shellHook = "
              eval \"$(starship init bash)\";
        ";
      };
    });

    formatter = forAllSystems (
      system:
        nixpkgsFor.${system}.alejandra
    );
  };
}
