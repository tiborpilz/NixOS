# Flake packages for the tandoor upgrade test:
#
#   testTandoorUpgrade                    baseline -> module image (the real check)
#   testTandoorUpgradeTo_<version>        baseline -> any other pinned version
#   testTandoorUpgradeSabotage<Kind>      must fail; asserts it failed for the right reason
{ inputs, lib, pkgs }:

let
  pinned = import ./images.nix;

  mkTest = args: pkgs.testers.runNixOSTest (import ./. ({ inherit inputs lib pkgs; } // args));

  tandoorRefs = lib.filter
    (ref: lib.hasPrefix "docker.io/vabene1111/recipes:" ref && ref != pinned.baseline)
    (lib.attrNames pinned.pins);

  versionOf = ref: lib.last (lib.splitString ":" ref);

  # The raw test derivation has to fail, print the expected marker, print no
  # other marker, and must not have been killed by the global timeout.
  expectFailure = kind: marker:
    let
      test = mkTest { sabotage = kind; };
      failed = pkgs.testers.testBuildFailure test.config.rawTestDerivation;
    in
    pkgs.runCommand "tandoor-upgrade-sabotage-${kind}-detected" { } ''
      log=${failed}/testBuildFailure.log
      code=$(cat ${failed}/testBuildFailure.exit)
      echo "sabotaged test exited with $code"
      if [ "$code" = 143 ] || grep -q 'timeout reached; test terminating' "$log"; then
        echo "test was killed by its timeout instead of failing a check" >&2
        exit 1
      fi
      markers=$(grep -oE 'E2E-FAIL\[[a-z-]+\]' "$log" | sort -u)
      echo "markers: $markers"
      if [ "$markers" != 'E2E-FAIL[${marker}]' ]; then
        echo "expected exactly E2E-FAIL[${marker}]" >&2
        exit 1
      fi
      cp "$log" $out
    '';
in
{
  testTandoorUpgrade = mkTest { };
  testTandoorUpgradeSabotageMigration = expectFailure "migration" "migrations";
  testTandoorUpgradeSabotageWeb = expectFailure "web" "web";
} // lib.listToAttrs (map
  (ref: lib.nameValuePair
    "testTandoorUpgradeTo_${lib.replaceStrings [ "." ] [ "_" ] (versionOf ref)}"
    (mkTest { target = ref; }))
  tandoorRefs)
