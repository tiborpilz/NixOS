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

# Select Git Branch interactively; jj bookmarks in a jj repo
fzf-git-branch() {
  if jj root > /dev/null 2>&1; then
    jj bookmark list --all-remotes --sort committer-date- \
      -T 'if(remote != "git", name ++ if(remote, "@" ++ remote) ++ "\n")' |
      fzf-tmux --no-multi --preview-window right:65% \
        --preview 'jj log --color=always -r "::{}" -n 50'
    return
  fi

  git rev-parse HEAD > /dev/null 2>&1 || return # not a git repo

  git branch --color=always --all --sort=-committerdate |
    grep -v HEAD |
    fzf-tmux --ansi --no-multi --preview-window right:65% \
      --preview 'git log -n 50 --color=always --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed "s/.* //" <<< {})' \
      | sed "s/.* //"
}

# Interactive git checkout
gch() {
  local branch=$(fzf-git-branch)
  [[ -n $branch ]] || return
  # checkout would move git's HEAD behind jj's back
  if jj root > /dev/null 2>&1; then
    jj new "$branch"
  else
    git checkout "$branch"
  fi
}

# In a jj repo git's HEAD is detached, so the branch is the nearest bookmark
vcs_current_branch() {
  if jj root > /dev/null 2>&1; then
    jj log --no-graph -r 'closest_bookmark(@)' -T 'local_bookmarks.map(|b| b.name()).join("\n") ++ "\n"' | head -1
  else
    git rev-parse --abbrev-ref HEAD
  fi
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
  branch_name=${1:-$(vcs_current_branch)}
  ticket=$(echo "$branch_name" | cut -d'/' -f2 | sed -E 's/^([A-Z0-9]+-[0-9]+).*/\1/')
  rest=$(echo "$branch_name" | cut -d'/' -f2 | sed -E "s/^$ticket-//" | sed 's/-/ /g')

  echo "$ticket: $rest"
}

# Create MR with formatted branch name
mr() {
  local branch=$(vcs_current_branch)
  title=$(format_branch_name "$branch")
  glab mr create --web --source-branch "$branch" --title "$title" "$@"
}

# Show the Nth most recent zprof entry from the profiling log (default: latest).
#   zprof-last      # newest run
#   zprof-last 2    # second-newest, etc.
zprof-last() {
  local n=${1:-1}
  local log=${ZPROF_LOG:-${ZSH_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/zsh}/zprof.log}
  awk -v want=$n '
    /^=== / { idx++ }
    { blocks[idx] = blocks[idx] $0 ORS }
    END {
      target = idx - want + 1
      if (target < 1) { print "no entry " want " of " idx > "/dev/stderr"; exit 1 }
      printf "%s", blocks[target]
    }
  ' $log
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
