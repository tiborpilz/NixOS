#!/usr/bin/env bash
#
# Simple wrapper to create a new Org Roam node from the CLI.
# Usage: roam-new "My Note Title"
#

TITLE="$1"
if [[ -z "$TITLE" ]]; then
  echo "Usage: roam-new \"Some Note Title\""
  exit 1
fi

emacsclient -n --eval "(progn
  (require 'org-roam)
  (let ((node (org-roam-node-create :title \"${TITLE}\")))
    (org-roam-capture- :node node :props '(:finalize find-file))))"

