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
        compiler = "ghc94";

        hpkgs = pkgs.haskell.packages.${compiler}.override {
          overrides = hself: hsuper: {

            # some packages are not yet available / lack the 
            # necessary version within the nixpkgs, so we'll just
            # download these special cases from hackage.
            effectful = pkgs.haskell.lib.overrideCabal hsuper.effectful {
              version = "2.3.1.0";
              sha256 = "sha256-A85wBg/FOFXJ7fDhdK3nNb5bLMAm6ESZo7rrZsdk/IU=";
            };

            effectful-core = pkgs.haskell.lib.overrideCabal hsuper.effectful-core {
              version = "2.3.1.0";
              sha256 = "sha256-NfqWA7z0oq/trtKMegqDX9ObnZPqeC18j6rx907swU4=";
            };

            effectful-plugin =  pkgs.haskell.lib.overrideCabal hsuper.effectful-plugin {
              version = "1.1.0.3";
              sha256 = "sha256-AVIQq06Oy9XUR7gI2Q0b3xPKSj+zPdJuqDgpAxI8oB8=";
            };

            megaparsec =  pkgs.haskell.lib.overrideCabal hsuper.megaparsec {
              version = "9.6.1";
              sha256 = "sha256-oyJXvoR9vnGsItUSqxFfnOJvoIvNvqfKi1JWoXANy/8=";
              editedCabalFile = null; 
              revision = null;
            };

            diagnose = pkgs.lib.pipe hsuper.diagnose [
              # doJailbreak means "yeah, you can just ignore
              # this package's version constraints"

              # this is *probably* okay: https://github.com/Mesabloo/diagnose/issues/18
              # the reason this is marked as broken is because the text version is 
              # to old for this, but it works fine with a new text version too, just
              # nobody has PR'd away the upper bound
              pkgs.haskell.lib.doJailbreak 
              # now that we told it to just use the current version of text, 
              # we'll just mark it as not broken
              pkgs.haskell.lib.markUnbroken
              # we want the megaparsec-compat flag enabled on this package
              # for pretty errors when we're using megaparsec
              (pkgs.lib.trivial.flip (pkgs.haskell.lib.appendConfigureFlags) ["-f megaparsec-compat"])
              # and then we want it to use our megaparsec 9.6.1, because it
              # isn't in the nixpkgs default package definition.
              (pkgs.lib.trivial.flip (pkgs.haskell.lib.addBuildDepends) [ hself.megaparsec ])
            ];
            
          };
        };
    in {
      default = hpkgs.developPackage { root = ./.; };
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
          haskell.compiler.ghc94
          haskellPackages.implicit-hie
        ];

        nativeBuildInputs = with pkgs; [
          haskell.compiler.ghc94
          (haskell-language-server.override {supportedGhcVersions = ["94"];})
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
