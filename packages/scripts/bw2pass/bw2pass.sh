#!/usr/bin/env bash

set -eo pipefail
# This script exports bitwarden passwords to pass
# It requires the bitwarden cli to be installed and logged in
# It requires pass to be installed and initialized

check_bw_login() {
    local status=$(bw status | jq -r '.status')
    local interactive=$(tty -s && echo true || echo false)

    if [[ "$status" == "unauthenticated" ]]; then
        # Log in when we're in an interactive shell, exit otherwise
        if [[ "$interactive" ]]; then
            result=$(bw login)
            key=$(echo "$result" | grep '^\$ export' | cut -d'=' -f2 | tr -d '"')
            # export BW_SESSION="$key"
            echo $key
        else
            echo "You're not logged in to Bitwarden, please run 'bw login' first"
            exit 1
        fi
    fi
}

append_pass_value() {
    local pass_value="$1"
    local key="$2"
    local value="$3"

    if [ "$value" != "null" ]; then
        pass_value=$(printf "%b\n" "$pass_value" "$key: $value")
    fi
    cat <<< $pass_value
}

insert_password() {
    local name="$1"
    local username="$2"
    local password="$3"
    local url="$4"
    local notes="$5"

    local pass_name="bitwarden/$name"

    pass_value="$password"
    echo "$name"

    pass_value=$(append_pass_value "$pass_value" "Username" "$username")
    pass_value=$(append_pass_value "$pass_value" "Url" "$url")
    pass_value=$(append_pass_value "$pass_value" "Notes" "$notes")

    pass insert -m "$pass_name" <<< "$pass_value"
}

get_items() {
    local items="$(bw list items)"
    local item_count="$(echo "$items" | jq length)"

    for ((i=0; i<item_count; i++)); do
        local item="$(echo "$items" | jq -r ".[$i]")"
        local name="$(echo "$item" | jq -r '.name')"
        local username="$(echo "$item" | jq -r '.login.username')"
        local password="$(echo "$item" | jq -r '.login.password')"
        local url="$(echo "$item" | jq -r '.login.uris[0].uri')"
        local notes="$(echo "$item" | jq -r '.notes')"

        insert_password "$name" "$username" "$password" "$url" "$notes"
    done
}

# check_bw_login
get_items
