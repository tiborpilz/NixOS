#!/usr/bin/env bash
# Regenerates tests/tandoor/images.nix, the pinned container images used by the
# tandoor upgrade test (the test VM has no network).
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: update-images.sh [--baseline REF] [--baseline-db REF] [--refresh]
                        [EXTRA_REF...]
       update-images.sh -h | --help

Regenerates tests/tandoor/images.nix. The pinned set is the union of:

  baseline   the currently deployed app image: the app image of
             modules/nixos/services/tandoor.nix on ${BASE_REF:-origin/main}
             (override with --baseline REF)
  baselineDb the currently deployed database image: the db image of the same
             module revision (override with --baseline-db REF)
  target     the default of modules.services.tandoor.image in the working tree
  db         the default of modules.services.tandoor.dbImage in the working tree
  extras     every EXTRA_REF given on the command line, e.g.
             docker.io/vabene1111/recipes:2.6.15 (these become the
             testTandoorUpgradeTo_* packages)

Pins that already exist in images.nix are reused unchanged; only missing ones
are prefetched with nix-prefetch-docker (needs network access).

Options:
  --baseline REF   use REF as the baseline instead of the app image found on
                   ${BASE_REF:-origin/main}
  --baseline-db REF
                   use REF as the baseline database image instead of the db
                   image found on ${BASE_REF:-origin/main}
  --refresh        prefetch every ref again instead of reusing existing pins
                   (useful for floating tags such as postgres:14)
  -h, --help       show this help

Environment:
  BASE_REF         git ref holding the deployed module (default: origin/main)

Refs without a registry host get "docker.io/" prepended.
EOF
}

die() { echo "update-images.sh: $*" >&2; exit 1; }
log() { echo "update-images.sh: $*" >&2; }

baseline_override=""
baseline_db_override=""
refresh=0
extras=()
while [ $# -gt 0 ]; do
  case "$1" in
    -h | --help) usage; exit 0 ;;
    --baseline)
      [ $# -ge 2 ] || die "--baseline needs a REF argument"
      baseline_override="$2"; shift 2 ;;
    --baseline=*) baseline_override="${1#--baseline=}"; shift ;;
    --baseline-db)
      [ $# -ge 2 ] || die "--baseline-db needs a REF argument"
      baseline_db_override="$2"; shift 2 ;;
    --baseline-db=*) baseline_db_override="${1#--baseline-db=}"; shift ;;
    --refresh) refresh=1; shift ;;
    --) shift; extras+=("$@"); break ;;
    -*) usage >&2; die "unknown option: $1" ;;
    *) extras+=("$1"); shift ;;
  esac
done

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(git -C "$script_dir" rev-parse --show-toplevel)"
cd "$repo_root"

images_nix="tests/tandoor/images.nix"
module="modules/nixos/services/tandoor.nix"
base_ref="${BASE_REF:-origin/main}"

jq_() {
  if command -v jq >/dev/null 2>&1; then jq "$@"; else nix run nixpkgs#jq -- "$@"; fi
}

# docker.io is implied for refs whose first component is not a registry host.
normalize_ref() {
  local ref="$1" first
  [[ "$ref" == *:* ]] || die "image ref '$ref' has no tag"
  first="${ref%%/*}"
  if [[ "$ref" != */* || ( "$first" != *.* && "$first" != *:* && "$first" != localhost ) ]]; then
    ref="docker.io/$ref"
  fi
  echo "$ref"
}

# Default of a module option, evaluated from the working tree. Only the option
# defaults are read, so the module needs no real config (same trick as
# tests/tandoor/default.nix). Needs a nixpkgs lib: use the locked one, or the
# registry's if the locked revision cannot be fetched.
eval_option() {
  local opt="$1" nixpkgs_expr out
  for nixpkgs_expr in \
    'builtins.fetchTree (builtins.fromJSON (builtins.readFile (root + "/flake.lock"))).nodes.nixpkgs.locked' \
    '(builtins.getFlake "nixpkgs").outPath'; do
    if out=$(nix eval --impure --raw --expr "
      let
        root = $repo_root;
        base = import ((${nixpkgs_expr}) + \"/lib\");
        my = import (root + \"/lib\") { inputs = { }; lib = base; pkgs = null; };
        lib = base // { inherit my; };
        mod = import (root + \"/$module\") { config = { }; inherit lib; pkgs = null; };
      in mod.options.modules.services.tandoor.${opt}.default" 2>/dev/null); then
      [ -n "$out" ] && { echo "$out"; return 0; }
    fi
  done
  return 1
}

# Fallback: read the `images = { app.image = ...; db.image = ...; }` bindings
# from module text. Fails unless there is exactly one match.
grep_image() {
  local kind="$1" text="$2" matches n
  matches=$(printf '%s\n' "$text" | sed -nE "s/^[[:space:]]*${kind}\\.image[[:space:]]*=[[:space:]]*\"([^\"]+)\"[[:space:]]*;.*/\\1/p")
  n=$(printf '%s' "$matches" | grep -c . || true)
  [ "$n" -eq 1 ] || die "expected exactly one '${kind}.image = \"...\"' in $module, found $n"
  echo "$matches"
}

# App or db image of an older module revision that predates the `images` block:
# the `image = "..."` literals are the app image and the database image (the
# one containing "postgres"). Fails unless exactly one literal matches.
legacy_image() {
  local kind="$1" text="$2" all matches n
  all=$(printf '%s\n' "$text" | sed -nE 's/^[[:space:]]*image[[:space:]]*=[[:space:]]*"([^"]+)"[[:space:]]*;.*/\1/p' | sort -u)
  if [ "$kind" = db ]; then
    matches=$(printf '%s\n' "$all" | grep -i postgres || true)
  else
    matches=$(printf '%s\n' "$all" | grep -vi postgres || true)
  fi
  n=$(printf '%s' "$matches" | grep -c . || true)
  [ "$n" -eq 1 ] || die "expected exactly one $kind image literal in $module on $base_ref, found $n"
  echo "$matches"
}

# Image of the given kind (app or db) from the deployed module text, in either
# layout.
base_image() {
  local kind="$1" text="$2"
  if printf '%s\n' "$text" | grep -qE "^[[:space:]]*${kind}\\.image[[:space:]]*="; then
    grep_image "$kind" "$text"
  else
    legacy_image "$kind" "$text"
  fi
}

# --- target, db, baseline -----------------------------------------------------

if target=$(eval_option image) && db=$(eval_option dbImage); then
  :
else
  log "nix eval of the module options failed, falling back to parsing $module"
  text=$(cat "$module")
  target=$(grep_image app "$text")
  db=$(grep_image db "$text")
fi
target=$(normalize_ref "$target")
db=$(normalize_ref "$db")

if [ -z "$baseline_override" ] || [ -z "$baseline_db_override" ]; then
  base_text=$(git show "$base_ref:$module") \
    || die "cannot read $module from $base_ref (set BASE_REF or pass --baseline and --baseline-db)"
fi
if [ -n "$baseline_override" ]; then
  baseline="$baseline_override"
else
  baseline=$(base_image app "$base_text")
fi
if [ -n "$baseline_db_override" ]; then
  baseline_db="$baseline_db_override"
else
  baseline_db=$(base_image db "$base_text")
fi
baseline=$(normalize_ref "$baseline")
baseline_db=$(normalize_ref "$baseline_db")

refs=("$baseline" "$baseline_db" "$target" "$db")
for e in ${extras[@]+"${extras[@]}"}; do refs+=("$(normalize_ref "$e")"); done
mapfile -t refs < <(printf '%s\n' "${refs[@]}" | sort -u)

# --- existing pins ------------------------------------------------------------

old_json='{"pins":{}}'
if [ -f "$images_nix" ]; then
  old_json=$(nix eval --json --file "$images_nix") || die "cannot evaluate existing $images_nix"
fi

# Split "registry/name:tag" into name (docker.io/ stripped, as Docker Hub
# resolves it) and tag.
ref_name() { local n="${1%:*}"; echo "${n#docker.io/}"; }
ref_tag() { echo "${1##*:}"; }

# Prints "imageName<TAB>imageDigest<TAB>hash" for a ref.
prefetch() {
  local ref="$1" name tag json
  name=$(ref_name "$ref"); tag=$(ref_tag "$ref")
  log "prefetching $ref"
  json=$(nix shell nixpkgs#nix-prefetch-docker -c nix-prefetch-docker \
    --image-name "$name" --image-tag "$tag" \
    --final-image-name "${ref%:*}" --final-image-tag "$tag" \
    --os linux --arch amd64 --json --quiet) \
    || die "nix-prefetch-docker failed for $ref"
  local digest hash
  digest=$(jq_ -er '.imageDigest' <<<"$json") || die "no imageDigest in prefetch output for $ref"
  hash=$(jq_ -er '.hash // .sha256' <<<"$json") || die "no hash in prefetch output for $ref"
  case "$hash" in
    sha256-*) ;;
    *) hash=$(nix hash convert --hash-algo sha256 --to sri "$hash") ;;
  esac
  printf '%s\t%s\t%s\n' "$name" "$digest" "$hash"
}

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
{
  cat <<EOF
# Pinned container images for the tandoor upgrade test. The VM has no network,
# so every image the test runs has to be pinned here by digest and hash.
#
# \`baseline\` is the version that creates the database (the one currently
# deployed); \`target\` must match \`modules.services.tandoor.image\`.
# Regenerate with tests/tandoor/update-images.sh after bumping the module.
{
  baseline = "$baseline";
  baselineDb = "$baseline_db";

  pins = {
EOF
  reused=()
  fetched=()
  for ref in "${refs[@]}"; do
    line=""
    if [ "$refresh" -eq 0 ]; then
      line=$(jq_ -r --arg ref "$ref" '.pins[$ref] // empty | [.imageName, .imageDigest, .hash] | @tsv' <<<"$old_json")
    fi
    if [ -n "$line" ]; then
      reused+=("$ref")
    else
      line=$(prefetch "$ref")
      fetched+=("$ref")
    fi
    IFS=$'\t' read -r p_name p_digest p_hash <<<"$line"
    cat <<EOF
    "$ref" = {
      imageName = "$p_name";
      imageDigest = "$p_digest";
      hash = "$p_hash";
    };
EOF
  done
  cat <<'EOF'
  };
}
EOF
} >"$tmp"

# Validate before replacing the real file.
nix eval --file "$tmp" --json >/dev/null || die "generated file does not evaluate"
cat "$tmp" >"$images_nix"
nix eval --file "$images_nix" --json >/dev/null || die "$images_nix does not evaluate"

echo "Wrote $images_nix"
echo "  baseline:   $baseline"
echo "  baselineDb: $baseline_db"
echo "  target:     $target"
echo "  db:         $db"
echo "  pinned:"
for ref in "${refs[@]}"; do
  state=reused
  for f in ${fetched[@]+"${fetched[@]}"}; do [ "$f" = "$ref" ] && state=prefetched; done
  echo "    $ref ($state)"
done
