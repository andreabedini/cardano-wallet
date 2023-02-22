############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
CHaP: haskell-nix: haskell-nix.cabalProject' [
  ({ lib, pkgs, buildProject, ... }: {
    options = {
      gitrev = lib.mkOption {
        type = lib.types.str;
        description = "Git revision of sources";
      };
      profiling = lib.mkOption {
        type = lib.types.bool;
        description = "Enable profiling";
        default = false;
      };
      coverage = lib.mkOption {
        type = lib.types.bool;
        description = "Enable Haskell Program Coverage for cardano-wallet libraries and test suites.";
        default = false;
      };
      cacheTestFailures = lib.mkOption {
        type = lib.types.bool;
        description = ''If false, prevent check results from being cached on `nix build`'';
        default = true;
      };
    };
  })
  ({ pkgs
   , lib
   , config
   , buildProject
   , ...
   }:

    let
      inherit (pkgs) stdenv;
      inherit (haskell-nix) haskellLib;

      # Add this string to a tests preCheck to prevent test results from
      # being cached.
      #
      # It is useful to have when your tests are flaky and fail a lot --
      # we don't want to cache false failures.
      noCacheCookie = ''
        # Causes tests to be re-run whenever the git revision
        # changes, even if everything else is identical.
        echo "Git revision is ${toString config.gitrev}"
      '';

      noCacheTestFailuresCookie = lib.optionalString (!config.cacheTestFailures) noCacheCookie;

      # setGitRev is a postInstall script to stamp executables with
      # version info. It uses the "gitrev" option.
      # use buildPackages here, we want set-git-rev on the build machine even under
      # cross compilation (e.g. to windows)
      setGitRevPostInstall = setGitRevPostInstall' config.gitrev;
      setGitRevPostInstall' = gitrev: ''
        ${pkgs.buildPackages.haskellBuildUtils}/bin/set-git-rev "${gitrev}" $out/bin/*
      '';

      rewriteLibsPostInstall = lib.optionalString (pkgs.stdenv.hostPlatform.isDarwin) ''
        export PATH=$PATH:${lib.makeBinPath (with pkgs.buildPackages; [ haskellBuildUtils binutils nix ])}
        rewrite-libs $out/bin $out/bin/*
      '';

      stripBinariesPostInstall = lib.optionalString stdenv.hostPlatform.isLinux ''
        ${pkgs.buildPackages.binutils-unwrapped}/bin/*strip $out/bin/*
      '';

      # This exe component postInstall script adds shell completion
      # scripts. These completion
      # scripts will be picked up automatically if the resulting
      # derivation is installed, e.g. by `nix-env -i`.
      optparseCompletionPostInstall = lib.optionalString stdenv.hostPlatform.isUnix ''
        exeName=$(ls -1 $out/bin | head -n1)  # FIXME add $exeName to Haskell.nix
        bashCompDir="$out/share/bash-completion/completions"
        zshCompDir="$out/share/zsh/vendor-completions"
        fishCompDir="$out/share/fish/vendor_completions.d"
        mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"
        "$out/bin/$exeName" --bash-completion-script "$exeName" >"$bashCompDir/$exeName"
        "$out/bin/$exeName" --zsh-completion-script "$exeName" >"$zshCompDir/_$exeName"
        "$out/bin/$exeName" --fish-completion-script "$exeName" >"$fishCompDir/$exeName.fish"
      '';

      # The list of project packages is not automatically discovered yet,
      # instead it is generated by ./nix/regenerate.sh
      projectPackages = import ./project-package-list.nix;

      srcAll = lib.cleanSourceWith {
        name = "cardano-wallet-src-all";
        src = ../.;
        filter = lib.cleanSourceFilter;
      };

    in {
      name = "cardano-wallet";
      compiler-nix-name = "ghc8107";

      src = haskellLib.cleanSourceWith {
        name = "cardano-wallet-src";
        src = srcAll;
        filter = haskell-nix.haskellSourceFilter;
      };

      shell = {
        name = "cardano-wallet-shell${lib.optionalString config.profiling "-profiled"}";
        packages = ps: builtins.attrValues (haskellLib.selectProjectPackages ps);
        tools = {
          cabal-cache.version = "1.0.2.1";
          haskell-language-server = {
            version = "1.8.0.0";
            modules = [{ reinstallableLibGhc = false; }];
          };
          hie-bios = {
            modules = [{ reinstallableLibGhc = false; }];
          };
          hoogle.version = "5.0.18.1";
          hlint.version = "3.3.1";
          lentil.version = "1.5.2.0";
          stylish-haskell.version = "0.11.0.3";
          weeder.version = "2.1.3";
        };
        nativeBuildInputs = with buildProject.hsPkgs; [
          cardano-node.components.exes.cardano-node
          cardano-cli.components.exes.cardano-cli
          cardano-addresses-cli.components.exes.cardano-address
          bech32.components.exes.bech32
          pretty-simple.components.exes.pretty-simple
        ] ++ (with pkgs.buildPackages.buildPackages; [
          go-jira
          haskellPackages.ghcid
          pkgconfig
          python3Packages.openapi-spec-validator
          (ruby_3_1.withPackages (ps: [ ps.rake ps.thor ]))
          sqlite-interactive
          curlFull
          jq
          yq
          nixWrapped
          cabalWrapped
        ]);
      };

      inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };

      modules =
        let inherit (config) src coverage profiling;
        in
        [
          {
            packages = lib.genAttrs projectPackages (name: {
              # Mark package as local non-dep in the nix-shell.
              # fixme: Haskell.nix should set it
              package.isProject = true;

              # Enable release flag (optimization and -Werror)
              flags.release = true;

              # Enable Haskell Program Coverage for all local libraries
              # and test suites.
              doCoverage = coverage;
            });
          }

          # Provide configuration and dependencies to cardano-wallet components
          ({ config, pkgs, ... }:
            let
              cardanoNodeExes = with config.hsPkgs;
                [
                  cardano-node.components.exes.cardano-node
                  cardano-cli.components.exes.cardano-cli
                ];
            in
            {
              reinstallableLibGhc = true;

              # These are here to make `stackProject` vs `cabalProject` `nix-diff` cleaner
              # TODO remove
              packages.entropy.components.setup.doExactConfig = true;
              packages.prettyprinter-configurable.components.setup.doExactConfig = true;
              packages.pretty-simple.components.setup.doExactConfig = true;
              packages.wai-logger.components.setup.doExactConfig = true;
              packages.openapi3.components.setup.doExactConfig = true;
              packages.servant-openapi3.components.setup.doExactConfig = true;
              packages.system-filepath.components.setup.doExactConfig = true;

              packages.cardano-wallet.components.tests = {
                # Running Windows integration tests under Wine is disabled
                # because ouroboros-network doesn't fully work under Wine.
                integration.doCheck = !pkgs.stdenv.hostPlatform.isWindows;

                unit.preCheck = noCacheTestFailuresCookie +
                  lib.optionalString stdenv.isDarwin ''
                    # cardano-node socket path becomes too long otherwise
                    export TMPDIR=/tmp
                  '';

                # Force more integration tests to run in parallel than the
                # default number of build cores.
                #
                # To alleviate TimeInterpreter race conditions on the mac builders
                # since #2755, we run slightly less in parallel on macOS.
                integration.testFlags =
                  if pkgs.stdenv.hostPlatform.isDarwin
                  then [ "-j" "2" ]
                  else [ "-j" "3" ];

                integration.preCheck = noCacheCookie + ''
                  # Variables picked up by integration tests
                  export CARDANO_NODE_TRACING_MIN_SEVERITY=notice
                  export TESTS_RETRY_FAILED=yes

                  # Integration tests will place logs here
                  export TESTS_LOGDIR=$(mktemp -d)/logs
                '' + lib.optionalString stdenv.isDarwin ''
                  export TMPDIR=/tmp
                '';

                integration.postCheck = ''
                  # fixme: There needs to be some Haskell.nix changes to
                  # permit getting build products from failed builds.
                  if [ -n "$TESTS_LOGDIR" && -f $out/nix-support/failed ]; then
                    logfile=$out/cardano-wallet-integration-logs.tar.gz
                    ${pkgs.buildPackages.gnutar}/bin/tar -C $(dirname $TESTS_LOGDIR) -czvf $logfile $TESTS_LOGDIR
                    echo "file none $logfile" >> $out/nix-support/hydra-build-products
                  fi
                '';

                # provide cardano-node & cardano-cli to tests
                unit.build-tools = cardanoNodeExes;
                integration.build-tools = cardanoNodeExes;
              };

              # Add node backend to the PATH of the latency benchmarks, and
              # set the source tree as its working directory.
              packages.cardano-wallet.components.benchmarks.latency =
                lib.optionalAttrs (!stdenv.hostPlatform.isWindows) {
                  build-tools = [ pkgs.buildPackages.makeWrapper ];
                  postInstall = ''
                    wrapProgram $out/bin/* \
                      --run "cd ${srcAll}/lib/wallet" \
                      --prefix PATH : ${lib.makeBinPath cardanoNodeExes}
                  '';
                };

              # Add cardano-node to the PATH of the byroon restore benchmark.
              # cardano-node will want to write logs to a subdirectory of the working directory.
              # We don't `cd $src` because of that.
              packages.cardano-wallet.components.benchmarks.restore =
                lib.optionalAttrs (!stdenv.hostPlatform.isWindows) {
                  build-tools = [ pkgs.buildPackages.makeWrapper ];
                  postInstall = ''
                    wrapProgram $out/bin/restore \
                      --set CARDANO_NODE_CONFIGS ${pkgs.cardano-node-deployments} \
                      --prefix PATH : ${lib.makeBinPath cardanoNodeExes}
                  '';
                };


              packages.cardano-wallet.components.exes.local-cluster =
                let
                  testData = src + /lib/wallet/test/data/cardano-node-shelley;
                in
                if (stdenv.hostPlatform.isWindows) then {
                  postInstall = ''
                    mkdir -p $out/bin/test/data
                    cp -Rv ${testData} $out/bin/test/data
                  '';
                } else {
                  build-tools = [ pkgs.buildPackages.makeWrapper ];
                  postInstall = ''
                    wrapProgram $out/bin/local-cluster \
                      --set SHELLEY_TEST_DATA ${testData} \
                      --prefix PATH : ${lib.makeBinPath cardanoNodeExes}
                  '';
                };

              # Add shell completions for main executables.
              packages.cardano-wallet.components.exes.cardano-wallet.postInstall = optparseCompletionPostInstall + setGitRevPostInstall + rewriteLibsPostInstall + stripBinariesPostInstall;
            })

          ({ config, ... }:
            let
              setGitRevPostInstall = setGitRevPostInstall' config.packages.cardano-node.src.rev;
            in
            {
              # Add shell completions for tools.
              packages.cardano-cli.components.exes.cardano-cli.postInstall = optparseCompletionPostInstall + setGitRevPostInstall;
              packages.cardano-node.components.exes.cardano-node.postInstall = optparseCompletionPostInstall + setGitRevPostInstall;
              packages.cardano-addresses-cli.components.exes.cardano-address.postInstall = optparseCompletionPostInstall;
              packages.bech32.components.exes.bech32.postInstall = optparseCompletionPostInstall;
            })

          # Provide the git revision for cardano-addresses
          ({ config, ... }:
            {
              packages.cardano-addresses-cli.components.library.preBuild = ''
                export GITREV=${config.hsPkgs.cardano-addresses-cli.src.rev}
              '';
            })

          # Provide the swagger file in an environment variable for
          # tests because it is located outside of the Cabal package
          # source tree.
          {
            packages.cardano-wallet.components.tests.unit.preBuild = ''
              export SWAGGER_YAML=${src + /specifications/api/swagger.yaml}
            '';
          }

          ({ lib, pkgs, ... }: {
            # Use our forked libsodium from iohk-nix crypto overlay.
            packages.plutus-tx.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
            packages.byron-spec-ledger.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
            packages.cardano-wallet-cli.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
            packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
            packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
          })

          # Build fixes for library dependencies
          {
            # Packages we wish to ignore version bounds of.
            # This is similar to jailbreakCabal, however it
            # does not require any messing with cabal files.
            packages.katip.doExactConfig = true;

            # split data output for ekg to reduce closure size
            packages.ekg.components.library.enableSeparateDataOutput = true;

            # Avoid this error on the windows build:
            #   Wrap.hsc:96:10: fatal error: regex.h: No such file or directory
            packages.regex-posix.flags._regex-posix-clib = stdenv.hostPlatform.isWindows;

            # Lets us put the pretty-simple tool in shell.nix.
            packages.pretty-simple.flags.buildexe = true;
          }

          # Enable profiling on executables if the profiling argument is set.
          (lib.optionalAttrs profiling {
            enableLibraryProfiling = true;
            packages.cardano-wallet.components.exes.cardano-wallet.enableProfiling = true;
            packages.cardano-wallet.components.benchmarks.restore.enableProfiling = true;
            packages.plutus-core.ghcOptions = [ "-fexternal-interpreter" ];
          })

          # Musl libc fully static build
          (lib.optionalAttrs stdenv.hostPlatform.isMusl (
            let
              staticLibs = with pkgs; [ zlib openssl libffi gmp6 pkgs.secp256k1 ];

              # Module options which add GHC flags and libraries for a fully static build
              fullyStaticOptions = {
                enableShared = false;
                enableStatic = true;
                configureFlags = map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
              };
            in
            {
              # Apply fully static options to our Haskell executables
              packages.cardano-wallet.components.benchmarks.restore = fullyStaticOptions;
              packages.cardano-wallet.components.exes.cardano-wallet = fullyStaticOptions;
              packages.cardano-wallet.components.tests.integration = fullyStaticOptions;
              packages.cardano-wallet.components.tests.unit = fullyStaticOptions;
              packages.cardano-wallet.components.benchmarks.db = fullyStaticOptions;
              packages.cardano-wallet-launcher.components.tests.unit = fullyStaticOptions;

              # systemd can't be statically linked - disable lobemo-scribe-journal
              packages.cardano-config.flags.systemd = false;
              packages.cardano-node.flags.systemd = false;

              # Haddock not working for cross builds and is not needed anyway
              doHaddock = false;
            }
          ))

          # Silence some warnings about "cleaning component source not
          # supported for hpack package" which appear in nix-shell
          {
            packages.cardano-addresses.cabal-generator = lib.mkForce null;
            packages.cardano-addresses-cli.cabal-generator = lib.mkForce null;
          }

          # Disable scrypt support on ARM64
          ({ pkgs, ... }: {
            packages.cardano-wallet.flags.scrypt = !pkgs.stdenv.hostPlatform.isAarch64;
          })
        ];
    })
]
