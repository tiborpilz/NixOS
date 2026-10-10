# Pinned container images for the tandoor upgrade test. The VM has no network,
# so every image the test runs has to be pinned here by digest and hash.
#
# `baseline` is the version that creates the database (the one currently
# deployed); `target` must match `modules.services.tandoor.image`.
# Regenerate with tests/tandoor/update-images.sh after bumping the module.
{
  baseline = "docker.io/vabene1111/recipes:2.4.2";
  baselineDb = "docker.io/postgres:14";

  pins = {
    "docker.io/postgres:14" = {
      imageName = "postgres";
      imageDigest = "sha256:14bfab572eec6abf65892e1db7c3ba8d41b2a2855c1b143664907d6e146eb6e1";
      hash = "sha256-rIj2V9eX3+yF48EqyRY8yWvgGzevYTeQJf09BY470YQ=";
    };
    "docker.io/vabene1111/recipes:2.4.2" = {
      imageName = "vabene1111/recipes";
      imageDigest = "sha256:c1a1d494631955bdb6c200066ee5914adbc378805ac0e4df9aeb795fe25c87aa";
      hash = "sha256-FxXMRMrhsMGxsDPP2ADuudltEjRc6BcwV3M96nS1a5A=";
    };
    "docker.io/vabene1111/recipes:2.6.15" = {
      imageName = "vabene1111/recipes";
      imageDigest = "sha256:2e759dd1478a2ed119ee474e28522079fb1cfa50b3fd25cba89f6b9a67abad72";
      hash = "sha256-lF0LCnA6pnMASGjdtFt60tVSRZYnKmE6M2yXGkz7sy4=";
    };
  };
}
