#!/usr/bin/env bash
set -euo pipefail

# Maintain bitwarden/ in pass as a generated, one-way mirror. Bitwarden is the
# source of truth; entries outside bitwarden/ are never touched.

ensure_bw_session() {
    local status session

    if ! status=$(bw status | jq -er '.status'); then
        echo "Unable to determine Bitwarden status." >&2
        exit 1
    fi

    case "$status" in
        unlocked)
            return
            ;;
        locked)
            if [[ ! -t 0 ]]; then
                echo "Bitwarden is locked. Run 'bw unlock' and export BW_SESSION first." >&2
                exit 1
            fi
            session=$(bw unlock --raw)
            ;;
        unauthenticated)
            if [[ ! -t 0 ]]; then
                echo "You're not logged in to Bitwarden. Run 'bw login' first." >&2
                exit 1
            fi
            session=$(bw login --raw)
            ;;
        *)
            echo "Unknown Bitwarden status: $status" >&2
            exit 1
            ;;
    esac

    if [[ -z "$session" ]]; then
        echo "Bitwarden did not return a session key." >&2
        exit 1
    fi
    export BW_SESSION="$session"
}

workdir=
stage_store=
backup_dir=
store_dir=${PASSWORD_STORE_DIR:-${HOME:?HOME is not set}/.password-store}
mirror_dir=$store_dir/bitwarden

cleanup() {
    local status=$?
    trap - EXIT HUP INT TERM

    # If replacement failed between the two renames, put the old mirror back.
    if [[ -n "$backup_dir" && -d "$backup_dir" && ! -e "$mirror_dir" ]]; then
        mv -- "$backup_dir" "$mirror_dir"
    fi
    if [[ -n "$workdir" && -d "$workdir" ]]; then
        rm -rf -- "$workdir"
    fi
    if [[ -n "$stage_store" && -d "$stage_store" ]]; then
        rm -rf -- "$stage_store"
    fi
    exit "$status"
}
trap cleanup EXIT HUP INT TERM

ensure_bw_session

if [[ ! -f "$store_dir/.gpg-id" ]]; then
    echo "Password store is not initialized: $store_dir/.gpg-id is missing." >&2
    exit 1
fi

# bw list reads the CLI's local cache, so refresh it before constructing the
# mirror. Nothing in the current mirror is changed if sync or import fails.
echo "Synchronizing Bitwarden..." >&2
bw sync >/dev/null

workdir=$(mktemp -d "${TMPDIR:-/tmp}/bw2pass.XXXXXX")
chmod 700 "$workdir"
mkdir "$workdir/items"

items_file=$workdir/items.json
bw list items > "$items_file"
item_count=$(jq -er 'length' "$items_file")

# Split items into private files so secrets never appear in argv. Calculate all
# destination names before writing, allowing duplicate (or sanitized-colliding)
# names to receive stable Bitwarden-ID suffixes.
declare -A path_counts=()
index=0
while IFS= read -r item_json; do
    item_file=$workdir/items/$index.json
    printf '%s\n' "$item_json" > "$item_file"
    chmod 600 "$item_file"

    path=$(jq -er '
        (.name // "")
        | gsub("/"; "_")
        | gsub("\\\\"; "_")
        | gsub("[[:cntrl:]]"; "_")
        | gsub("^[[:space:]]+|[[:space:]]+$"; "")
        | if . == "" or . == "." or . == ".." then "unnamed" else . end
    ' "$item_file")
    printf '%s' "$path" > "$workdir/items/$index.path"
    path_counts["$path"]=$(( ${path_counts["$path"]:-0} + 1 ))
    ((index += 1))
done < <(jq -c '.[]' "$items_file")

for ((index = 0; index < item_count; index += 1)); do
    path=$(<"$workdir/items/$index.path")
    printf '%s' "${path_counts["$path"]}" > "$workdir/items/$index.count"
done

stage_store=$(mktemp -d "$store_dir/.bw2pass-stage.XXXXXX")
chmod 700 "$stage_store"
cp -- "$store_dir/.gpg-id" "$stage_store/.gpg-id"
mkdir "$stage_store/bitwarden"

worker=$workdir/worker.sh
cat > "$worker" <<'WORKER'
#!/usr/bin/env bash
set -euo pipefail

item_file=$1
path_file=${item_file%.json}.path
count_file=${item_file%.json}.count
path=$(<"$path_file")
if (( $(<"$count_file") > 1 )); then
    id=$(jq -er '.id' "$item_file")
    path="$path--${id:0:8}"
fi

value_file=$(mktemp "$BW2PASS_WORKDIR/value.XXXXXX")
trap 'rm -f -- "$value_file"' EXIT

# Keep the password on line one for pass(1), followed by useful metadata. jq
# performs the formatting so embedded newlines and absent item types are safe.
jq -r '
    (.login.password // ""),
    (if (.login.username // "") != "" then "Username: \(.login.username)" else empty end),
    (.login.uris[]? | select((.uri // "") != "") | "Url: \(.uri)"),
    (if (.login.totp // "") != "" then "Totp: \(.login.totp)" else empty end),
    (.fields[]? | select((.name // "") != "") | "Field-\(.name): \(.value // "")"),
    (if (.notes // "") != "" then "Notes: \(.notes)" else empty end),
    "Bitwarden-Id: \(.id)"
' "$item_file" > "$value_file"

PASSWORD_STORE_DIR=$BW2PASS_STAGE_STORE \
    pass insert -f -m "bitwarden/$path" < "$value_file"
WORKER
chmod 600 "$worker"

export BW2PASS_WORKDIR=$workdir
export BW2PASS_STAGE_STORE=$stage_store

echo "Encrypting $item_count Bitwarden item(s)..." >&2
seq 0 $((item_count - 1)) \
    | parallel --will-cite --halt now,fail=1 -j "${BW2PASS_JOBS:-100%}" \
        bash "$worker" "$workdir/items/{}.json"

# Both directories are on the password-store filesystem. The live mirror is
# moved aside before the staged mirror is installed, and cleanup restores it if
# the second rename fails.
backup_dir=$(mktemp -d "$store_dir/.bw2pass-old.XXXXXX")
rmdir -- "$backup_dir"
if [[ -e "$mirror_dir" ]]; then
    mv -- "$mirror_dir" "$backup_dir"
else
    backup_dir=
fi
mv -- "$stage_store/bitwarden" "$mirror_dir"

if [[ -n "$backup_dir" ]]; then
    rm -rf -- "$backup_dir"
    backup_dir=
fi
rm -rf -- "$stage_store"
stage_store=

echo "Mirrored $item_count Bitwarden item(s) to $mirror_dir."
