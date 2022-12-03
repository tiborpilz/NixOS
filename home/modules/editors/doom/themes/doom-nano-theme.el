;;; doom-nano-theme.el --- inspired by Nicolas Rougier nano-theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Yann Esposito <https://yannesposito.com>
;; Created: August 16, 2021
;; Version: 1.0.0
;; Keywords: custom themes, faces
;; Homepage: https://github.com/hlissner/emacs-doom-themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Ported from nano-theme: https://github.com/rougier/nano-theme
;;
;;; Code:

(require 'doom-themes)

;;; Variables

(defgroup doom-plain-theme nil
  "Options for the `doom-plain' theme."
  :group 'doom-themes)

(defcustom doom-plain-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-plain-theme
  :type '(or integer boolean))
;;
;;; Theme definition

(def-doom-theme doom-nano
  "Theme inspired by Nicolas Rougier nano-theme"

  ;; name      default/256/16
  ((nano-color-foreground '("#37474F")) ;; Blue Grey / L800
   (nano-color-background '("#FFFFFF")) ;; White
   (nano-color-highlight  '("#FAFAFA")) ;; Very Light Grey
   (nano-color-critical   '("#FF6F00")) ;; Amber / L900
   (nano-color-salient    '("#673AB7")) ;; Deep Purple / L500
   (nano-color-strong     '("#000000")) ;; Black
   (nano-color-popout     '("#FFAB91")) ;; Deep Orange / L200
   (nano-color-subtle     '("#ECEFF1")) ;; Blue Grey / L50
   (nano-color-faded      '("#B0BEC5")) ;; Blue Grey / L200

   (bg         nano-color-background)
   (bg-alt     nano-color-highlight)
   (base0      '("#18282f"))
   (base1      '("#24323a"))
   (base2      '("#556066"))
   (base3      '("#6f787d"))
   (base4      '("#8a9296"))
   (base5      '("#a6acaf"))
   (base6      '("#e7e8e9"))
   (base7      '("#f6f6f6"))
   (base8      '("#fafafa"))
   (fg         nano-color-foreground)
   (fg-alt     nano-color-faded)

   (grey       fg)
   (red        fg)
   (blue       fg)
   (dark-blue  fg)
   (orange     fg)
   (green      fg)
   (teal       fg)
   (yellow     fg)
   (magenta    fg)
   (violet     fg)
   (cyan       fg)
   (dark-cyan  fg)


   ;; face categories -- required for all themes
   (highlight      nano-color-salient)
   (vertical-bar   base5)
   (selection      nano-color-highlight)
   (builtin        nano-color-salient)
   (comments       nano-color-faded)
   (doc-comments   nano-color-faded)
   (constants      nano-color-strong)
   (functions      nano-color-salient)
   (keywords       nano-color-strong)
   (methods        nano-color-salient)
   (operators      nano-color-strong)
   (type           nano-color-strong)
   (strings        base0)
   (variables      base0)
   (numbers        base0)
   (region         base4)
   (error          nano-color-critical)
   (warning        nano-color-popout)
   (success        nano-color-salient)
   (vc-modified    nano-color-salient)
   (vc-added       fg-alt)
   (vc-deleted     nano-color-critical)

   ;; custom categories
   (-modeline-pad
    (when doom-plain-padded-modeline
      (if (integerp doom-plain-padded-modeline) doom-plain-padded-modeline 4)))

   (modeline-bg              (doom-darken bg-alt 0.4))
   (modeline-bg-alt          (doom-darken bg-alt 0.3))
   (modeline-bg-inactive     (doom-darken bg-alt 0.3))
   (modeline-bg-inactive-alt bg-alt)
   (modeline-fg              fg)
   (modeline-fg-alt          (doom-darken modeline-bg-inactive 0.35)))

  ;;;; Base theme face overrides
  ((error   :underline `(:style wave :color ,error))
   (warning :underline `(:style wave :color ,warning))
   ((font-lock-constant-face &override)      :slant 'italic)
   ((font-lock-comment-face &override)       :slant 'italic)
   ((font-lock-function-name-face &override) :slant 'italic)
   ((font-lock-type-face &override)          :slant 'italic)
   ;;(hl-line :background base8)
   ((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground base2)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)

   ;;;; doom-modeline
   (doom-modeline-bar :background modeline-bg)
   (doom-modeline-bar-inactive :inherit 'doom-modeline-bar)
   (doom-modeline-project-dir :foreground fg)
   (doom-modeline-buffer-file :foreground fg)
   (doom-modeline-buffer-modified :weight 'bold :foreground "#000000")
   (doom-modeline-panel :inherit 'mode-line-highlight :background base3 :foreground fg)
   ;;;; ivy
   (ivy-posframe :background bg-alt)
   ;;;; magit
   ((magit-diff-added-highlight &override)   :foreground fg :background (doom-blend vc-added bg 0.3))
   ((magit-diff-removed &override)           :foreground (doom-lighten fg 0.4) :background (doom-blend vc-deleted bg 0.1))
   ((magit-diff-removed-highlight &override) :foreground fg :background (doom-blend vc-deleted bg 0.22))
   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground keywords :weight 'bold)
   ;;;; outline <built-in>
   (outline-1 :slant   'italic :foreground fg-alt)
   (outline-2 :inherit 'outline-1 :foreground base2)
   (outline-3 :inherit 'outline-2)
   (outline-4 :inherit 'outline-3)
   (outline-5 :inherit 'outline-4)
   (outline-6 :inherit 'outline-5)
   (outline-7 :inherit 'outline-6)
   (outline-8 :inherit 'outline-7)

   (org-level-1 :inherit 'org-level-1 :foreground nano-color-strong)
   (org-level-2 :inherit 'org-level-2 :foreground nano-color-strong)
   (org-level-3 :inherit 'org-level-3 :foreground nano-color-strong)
   (org-level-4 :inherit 'org-level-4 :foreground nano-color-strong)
   (org-level-5 :inherit 'org-level-5 :foreground nano-color-strong)
   (org-level-6 :inherit 'org-level-6 :foreground nano-color-strong)
   (org-level-7 :inherit 'org-level-7 :foreground nano-color-strong)
   (org-level-8 :inherit 'org-level-8 :foreground nano-color-strong)

   (org-code     :inherit 'org-code
                 :foreground nano-color-salient
                 :weight 'bold)
   (org-verbatim :inherit 'org-verbatim
                 :foreground nano-color-salient
                 :weight 'bold)
   (org-upcoming-deadline :inherit 'org-upcoming-deadline
                          :foreground nano-color-critical
                          :weight 'bold)
   (org-upcoming-distant-deadline :inherit 'org-upcoming-distant-deadline
                                  :foreground nano-color-salient)

   (org-habit-overdue-face
    :inherit 'org-habit-overdue-face
    :background fg-alt)
   (org-habit-overdue-future-face
    :inherit 'org-habit-overdue-future-face
    :background nano-color-subtle)
   (org-habit-alert-face
    :inherit 'org-habit-alert-face
    :background nano-color-critical)
   (org-habit-alert-future-face
    :inherit 'org-habit-alert-future-face
    :background nano-color-subtle)

   (org-scheduled-today :inherit 'org-scheduled-today :foreground fg)
   (org-scheduled-previously :inherit 'org-scheduled-previously :foreground fg)

   ;;;; org <built-in>
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :foreground base5)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))))

;;; doom-plain-theme.el ends here
