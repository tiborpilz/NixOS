#!/usr/bin/env bash
set -euo pipefail

script=$1
test_root=$(mktemp -d "${TMPDIR:-/tmp}/bw2pass-test.XXXXXX")
trap 'rm -rf -- "$test_root"' EXIT
mkdir "$test_root/bin" "$test_root/store" "$test_root/store/bitwarden"
printf 'test-key\n' > "$test_root/store/.gpg-id"
printf 'stale\n' > "$test_root/store/bitwarden/old.gpg"

printf '#!%s\n' "$BASH" > "$test_root/bin/bw"
cat >> "$test_root/bin/bw" <<'EOF'
set -euo pipefail
case "$1" in
    status) printf '{"status":"unlocked"}\n' ;;
    sync) printf 'sync\n' >> "$BW_TEST_LOG" ;;
    list) cat "$BW_TEST_ITEMS" ;;
    *) exit 2 ;;
esac
EOF

printf '#!%s\n' "$BASH" > "$test_root/bin/pass"
cat >> "$test_root/bin/pass" <<'EOF'
set -euo pipefail
path=${*: -1}
if [[ -n "${PASS_FAIL_MATCH:-}" && "$path" == *"$PASS_FAIL_MATCH"* ]]; then
    exit 23
fi
destination=$PASSWORD_STORE_DIR/$path.gpg
mkdir -p "${destination%/*}"
cp /dev/stdin "$destination"
EOF
chmod +x "$test_root/bin/bw" "$test_root/bin/pass"

cat > "$test_root/items.json" <<'EOF'
[
  {
    "id": "11111111-1111-1111-1111-111111111111",
    "name": "mail",
    "login": {
      "username": "me@example.test",
      "password": "mail-secret",
      "uris": [{"uri": "imaps://mail.example.test"}],
      "totp": "totp-secret"
    },
    "fields": [{"name": "client", "value": "mbsync"}],
    "notes": "mail note"
  },
  {
    "id": "22222222-2222-2222-2222-222222222222",
    "name": "same/name",
    "login": {"password": "first"}
  },
  {
    "id": "33333333-3333-3333-3333-333333333333",
    "name": "same\\name",
    "login": {"password": "second"}
  }
]
EOF

export PATH=$test_root/bin:$PATH
export PASSWORD_STORE_DIR=$test_root/store
export BW_TEST_ITEMS=$test_root/items.json
export BW_TEST_LOG=$test_root/bw.log

bash "$script" >/dev/null

test -f "$test_root/store/bitwarden/mail.gpg"
test -f "$test_root/store/bitwarden/same_name--22222222.gpg"
test -f "$test_root/store/bitwarden/same_name--33333333.gpg"
test ! -e "$test_root/store/bitwarden/old.gpg"
grep -qx 'sync' "$test_root/bw.log"
grep -qx 'mail-secret' "$test_root/store/bitwarden/mail.gpg"
grep -qx 'Username: me@example.test' "$test_root/store/bitwarden/mail.gpg"
grep -qx 'Url: imaps://mail.example.test' "$test_root/store/bitwarden/mail.gpg"
grep -qx 'Totp: totp-secret' "$test_root/store/bitwarden/mail.gpg"
grep -qx 'Field-client: mbsync' "$test_root/store/bitwarden/mail.gpg"
grep -qx 'Bitwarden-Id: 11111111-1111-1111-1111-111111111111' \
    "$test_root/store/bitwarden/mail.gpg"

# A failed import must leave the previously completed mirror intact.
cp "$test_root/store/bitwarden/mail.gpg" "$test_root/mail.before"
export PASS_FAIL_MATCH=same_name
if bash "$script" >/dev/null 2>&1; then
    echo "bw2pass unexpectedly succeeded with a failing pass command" >&2
    exit 1
fi
cmp "$test_root/mail.before" "$test_root/store/bitwarden/mail.gpg"

# An empty upstream vault removes the generated subtree's old entries.
printf '[]\n' > "$test_root/items.json"
unset PASS_FAIL_MATCH
bash "$script" >/dev/null
test -d "$test_root/store/bitwarden"
test -z "$(find "$test_root/store/bitwarden" -type f -print -quit)"
