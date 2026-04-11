;;; emacs-setup.el --- Set up consistent Emacs frame for screenshots -*- lexical-binding: t; -*-

(defun showcase--wait-for-doom ()
  "Wait until Doom Emacs has finished loading."
  (while (not (featurep 'doom-start))
    (sleep-for 1)))

(defun showcase--set-frame-size ()
  "Set frame to consistent size for screenshots."
  (set-frame-size (selected-frame) 160 50)
  (set-frame-position (selected-frame) 100 100))

(defun showcase--show-dashboard ()
  "Show the Doom dashboard."
  (showcase--set-frame-size)
  (+doom-dashboard/open (selected-frame))
  (redisplay t))

(defun showcase--show-org ()
  "Open the sample org file with org-modern styling."
  (showcase--set-frame-size)
  (find-file "/tmp/showcase/notes.org")
  (goto-char (point-min))
  (org-overview)
  (org-show-entry)
  ;; Ensure org-modern is active
  (when (fboundp 'org-modern-mode)
    (org-modern-mode 1))
  (redisplay t))

(defun showcase--show-code-treemacs ()
  "Open a code file with Treemacs sidebar."
  (showcase--set-frame-size)
  (find-file "/tmp/showcase/config.nix")
  (goto-char (point-min))
  ;; Open treemacs
  (when (fboundp 'treemacs)
    (treemacs)
    (other-window 1))
  (redisplay t))

(provide 'emacs-setup)
;;; emacs-setup.el ends here
