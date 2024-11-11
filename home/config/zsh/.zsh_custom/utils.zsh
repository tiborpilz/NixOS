#!/usr/bin/env

# Utility functions
# TODO: Move each of these into their own files

python-init() {
  # Init python venv in current dir if no argument given
  projectPath=${PWD##*/}

  if [ -n "$1" ]
  then
    echo "$1"
    export project="$1"
    mkdir "$1"
  else
    export project=${PWD##*/}
  fi

  export projectPath="./${project}"

  # Create .venv folder with project folder name as prompt
  python -m venv "$projectPath/.venv" --prompt "$project"
  source "${projectPath}/.venv/bin/activate"

  # Activate venv and install kernelspec for jupyter
  # pip install ipykernel
  # python -m ipykernel install --user --name "$project"
  # pip install jupyter
}

# Select Git Branch interactively
fzf-git-branch() {
  git rev-parse HEAD > /dev/null 2>&1 || return # not a git repo

  git branch --color=always --all --sort=-committerdate |
    grep -v HEAD |
    fzf-tmux --ansi --no-multi --preview-window right:65% \
      --preview 'git log -n 50 --color=always --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed "s/.* //" <<< {})' \
      | sed "s/.* //"
}

# Interactive git checkout
gch() {
  git checkout $(fzf-git-branch)
}

# Urlencode
urlencode () {
  old_lc_collate=$LC_COLLATE
  LC_COLLATE=C
  local length="${#1}"
  for ((i = 0; i < length; i++ )) do
      local c="${1:$i:1}"
      case $c in
        ([a-zA-Z0-9.~_-]) printf '%s' "$c" ;;
        (*) printf '%%%02X' "'$c" ;;
      esac
  done
      LC_COLLATE=$old_lc_collate
}

# Format branch name for MRs etc.
format_branch_name() {
  branch_name=$(git rev-parse --abbrev-ref HEAD)
  ticket=$(echo "$branch_name" | cut -d'/' -f2 | sed -E 's/^([A-Z0-9]+-[0-9]+).*/\1/')
  rest=$(echo "$branch_name" | cut -d'/' -f2 | sed -E "s/^$ticket-//" | sed 's/-/ /g')

  echo "$ticket: $rest"
}

# Create MR with formatted branch name
mr() {
  title=$(format_branch_name)
  glab mr create --web --title "$title" "$@"
}

# Show current pipeline state abbreviated
# TODO: move this into its own package and use in tmux, prompt, nvim or emacs
cistatus() {
  local pipeline_output=$(glab ci status 2>/dev/null)

  echo "$pipeline_output" | awk '
    {
        if ($1 ~ /\(created\)/) state="⏳"
        else if ($1 ~ /\(running\)/) state="👟"
        else if ($1 ~ /\(success\)/) state="✅"
        else if ($1 ~ /\(failed\)/) state="❌"
        else state="❓"

        if ($1 ~ /\(running\)/) {
            printf "%s %s (%s) | ", state, $6, $2
        } else {
            printf "%s %s | ", state, $6
        }
    }
    '
}
