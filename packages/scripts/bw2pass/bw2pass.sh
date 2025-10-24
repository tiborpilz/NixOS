#!/usr/bin/env bash
set -eo pipefail
# Requires: bw, jq, pass, GNU parallel, flock (util-linux)
# Exports Bitwarden items into pass, with parallel processing but serialized pass inserts.

check_bw_login() {
    local status
    status=$(bw status | jq -r '.status')
    local interactive
    interactive=$(tty -s && echo true || echo false)

    if [[ "$status" == "unauthenticated" ]]; then
        if [[ "$interactive" == "true" ]]; then
            result=$(bw login)
            key=$(echo "$result" | grep '^\$ export' | cut -d'=' -f2 | tr -d '"')
            echo "$key"
        else
            echo "You're not logged in to Bitwarden, please run 'bw login' first" >&2
            exit 1
        fi
    fi
}

# Worker script: gets a single JSON line (a full item) as $1
# Writes formatted pass value to a temp file, then uses flock to serialize pass insert.
cat > /tmp/bw-pass-worker.sh <<'WORKER'
#!/usr/bin/env bash
set -euo pipefail
item_json="$1"

# parse fields (guard against missing fields)
name=$(jq -r '.name // empty' <<<"$item_json")
username=$(jq -r '.login.username // empty' <<<"$item_json")
password=$(jq -r '.login.password // empty' <<<"$item_json")
url=$(jq -r '.login.uris[0].uri // empty' <<<"$item_json")
notes=$(jq -r '.notes // empty' <<<"$item_json")

# build pass content (first line = secret)
pass_value="$password"
if [[ -n "$username" ]]; then
    pass_value+=$'\n'"Username: $username"
fi
if [[ -n "$url" ]]; then
    pass_value+=$'\n'"Url: $url"
fi
if [[ -n "$notes" ]]; then
    pass_value+=$'\n'"Notes: $notes"
fi

# temp file and a global lockfile for serializing pass inserts
tmpfile=$(mktemp)
trap 'rm -f "$tmpfile"' EXIT
printf '%s\n' "$pass_value" > "$tmpfile"

lockfile="/tmp/bitwarden-pass.lock"
# ensure lockfile exists
: > "$lockfile"

# Use flock to serialize the actual pass insert (so gpg won't be invoked concurrently)
flock -x "$lockfile" -- bash -c "pass insert -m \"bitwarden/$name\" < \"$tmpfile\""

# remove tempfile and exit
rm -f "$tmpfile"
exit 0
WORKER

chmod +x /tmp/bw-pass-worker.sh

# Ensure user is logged in (and get session if script produced one)
check_bw_login

# Fetch items once
items_json=$(bw list items)

# If there are no items, exit quietly
if [[ -z "$items_json" || "$(echo "$items_json" | jq length)" -eq 0 ]]; then
    echo "No Bitwarden items found." >&2
    exit 0
fi

# Create a newline-delimited stream of compact JSON objects
echo "$items_json" | jq -c '.[]' > /tmp/bw_items.jsonl

# Run the worker in parallel. Adjust -j N to control concurrency.
# Using -j+0 lets GNU parallel choose number of jobs (number of CPU cores),
# but you can set -j 4 or other number if you prefer.
cat /tmp/bw_items.jsonl | parallel -j+0 --halt soon,fail=1 --line-buffer /tmp/bw-pass-worker.sh {}

# clean up worker and tmp list
rm -f /tmp/bw-pass-worker.sh /tmp/bw_items.jsonl

echo "Done."
