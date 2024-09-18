# this file defines the actual logic that is required
# to build the actual software itself, and then we'll
# just include this file into the main flake which
# defines more of the system level shuffling, and things, but
# this is actual the definition that we care about to
# actually compile this package
{
  pkgs,
  compiler ? "ghc96",
}: let
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

      effectful-plugin = pkgs.haskell.lib.overrideCabal hsuper.effectful-plugin {
        version = "1.1.0.3";
        sha256 = "sha256-AVIQq06Oy9XUR7gI2Q0b3xPKSj+zPdJuqDgpAxI8oB8=";
      };

      megaparsec = pkgs.haskell.lib.overrideCabal hsuper.megaparsec {
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
        (pkgs.lib.trivial.flip (pkgs.haskell.lib.addBuildDepends) [hself.megaparsec])
      ];
    };
  };
  # from https://github.com/ethereum/hevm/blob/9048acdbf40aa434dc6a00210d37fdaac826d07b/flake.nix
  stripDylib = drv:
    pkgs.runCommand "${drv.name}-strip-dylibs" {} ''
      mkdir -p $out
      mkdir -p $out/lib
      cp -r ${drv}/* $out/
      rm -rf $out/**/*.dylib
      rm -rf $out/**/*.so
    '';
  iconv-rescue =
    if pkgs.stdenv.isDarwin
    then
      # this isn't a great option, but I think this is fine because of SIP
      "--extra-lib-dirs=/usr/lib"
    else
      # however otherwise we'll try to statically link iconv for reals.
      "--extra-lib-dirs=${stripDylib (pkgs.pkgsStatic.iconv.overrideAttrs (_: {dontDisableStatic = true;}))}/lib";
in rec {
  # output the default haskell compilation result
  default = hpkgs.developPackage {root = ./.;};

  # haskell-static indicates that
  # we're statically linking the haskell
  # libraries. This doesn't mean that all system
  # libraries will be statically linked.
  # this is not a foolproof way to do this, this just
  # at least takes care of the original haskell bits,
  haskell-static = pkgs.lib.pipe default [
    (pkgs.haskell.lib.compose.appendBuildFlags ["-v3"])
    (
      pkgs.haskell.lib.compose.appendConfigureFlags ([
          "-fci"
          "--extra-lib-dirs=${stripDylib (pkgs.gmp.override {withStatic = true;})}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${stripDylib (pkgs.libffi.overrideAttrs (_: {dontDisableStatic = true;}))}/lib"
          iconv-rescue
        ]
        ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
          "--enable-static"
          "--enable-executable-static"
          # TODO: replace this with musl: https://stackoverflow.com/a/57478728
          "--extra-lib-dirs=${pkgs.musl}/lib"
        ])
    )
  ];
}
