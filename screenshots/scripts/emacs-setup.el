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

(defun showcase--reset-org-variant ()
  "Reset Org styling overrides before applying a variant."
  (setq-local line-spacing nil
              org-hide-leading-stars nil
              org-startup-indented nil
              org-adapt-indentation nil)
  (visual-line-mode -1)
  (org-indent-mode -1)
  (when (fboundp 'mixed-pitch-mode)
    (mixed-pitch-mode -1))
  (when (fboundp 'org-modern-mode)
    (org-modern-mode -1)
    (setq org-modern-star nil
          org-modern-label-border 0.1)))

(defun showcase--apply-org-variant (variant)
  "Apply Org screenshot VARIANT."
  (let ((resolved-variant (if (memq variant '(current focus minimal))
                              variant
                            'current)))
    (pcase resolved-variant
      ('minimal
       (setq-local line-spacing nil))
      ('focus
       (setq-local line-spacing 0.3
                   org-hide-leading-stars t
                   org-startup-indented t
                   org-adapt-indentation nil)
       (visual-line-mode 1)
       (org-indent-mode 1)
       (when (fboundp 'mixed-pitch-mode)
         (mixed-pitch-mode 1))
       (when (fboundp 'org-modern-mode)
         (setq org-modern-star 'replace
               org-modern-label-border 0.2)
         (org-modern-mode 1)))
      ('current
       (setq-local line-spacing 0.2)
       (when (fboundp 'mixed-pitch-mode)
         (mixed-pitch-mode 1))
       (when (fboundp 'org-modern-mode)
         (setq org-modern-star nil
               org-modern-label-border 0.1)
         (org-modern-mode 1))))))

(defun showcase--show-org (&optional variant)
  "Open the sample org file with Org styling VARIANT."
  (showcase--set-frame-size)
  (find-file "/tmp/showcase/notes.org")
  (showcase--reset-org-variant)
  (showcase--apply-org-variant (or variant 'current))
  (goto-char (point-min))
  (org-overview)
  (org-show-entry)
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
