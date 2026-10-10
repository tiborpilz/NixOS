# Flake packages for the tandoor upgrade test:
#
#   testTandoorUpgrade                    baseline -> module image (the real check)
#   testTandoorUpgradeTo_<version>        baseline -> any other pinned version
#   testTandoorUpgradeSabotage<Kind>      broken upgrades that must fail, and for the right reason
{ inputs, lib, pkgs }:

let
  pinned = import ./images.nix;

  mkTest = args: pkgs.testers.runNixOSTest (import ./. ({ inherit inputs lib pkgs; } // args));

  tandoorRefs = lib.filter
    (ref: lib.hasPrefix "docker.io/vabene1111/recipes:" ref && ref != pinned.baseline)
    (lib.attrNames pinned.pins);

  versionOf = ref: lib.last (lib.splitString ":" ref);

  # The raw test derivation has to fail after the baseline passed its checks,
  # print the expected marker and no other, and must not have been killed by
  # the global timeout.
  expectFailure = kind: args: marker:
    let
      test = mkTest args;
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
      if ! grep -q 'E2E-OK verify' "$log"; then
        echo "the baseline never passed verify, so the sabotage was not what failed" >&2
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
  testTandoorUpgradeSabotageMigration = expectFailure "migration" { sabotage = "migration"; } "migrations";
  testTandoorUpgradeSabotageWeb = expectFailure "web" { sabotage = "web"; } "web";
  # A major postgres bump cannot start on the old data directory.
  testTandoorUpgradeSabotageDbMajor = expectFailure "db-major"
    {
      targetDb = "docker.io/postgres:17";
      extraPins."docker.io/postgres:17" = {
        imageName = "postgres";
        imageDigest = "sha256:2d2b8998d31037bf721cfdf764d76ba74171b4fab3431b7f72c27c56ddbdf9e3";
        hash = "sha256-z1V4jdBQxzCx6BWCYp6QhiYuLnnJy+VPPDJMS+XhN/Y=";
      };
    } "db";
} // lib.listToAttrs (map
  (ref: lib.nameValuePair
    "testTandoorUpgradeTo_${lib.replaceStrings [ "." ] [ "_" ] (versionOf ref)}"
    (mkTest { target = ref; }))
  tandoorRefs)
