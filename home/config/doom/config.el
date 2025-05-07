(setq user-full-name "Tibor Pilz"
      user-mail-address "tibor@pilz.berlin")

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'light)
      doom-big-font (font-spec :family "FiraCode Nerd Font" :size 28 :weight 'light)
      doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 16 :weight 'light))

(setq display-line-numbers-type 'visual)

(setq tab-width 2)

(setq fancy-splash-image (concat doom-private-dir "splash-logos/emacs-logo-cutout.svg"))

(setq frame-title-format "%b - Emacs")

(setq auth-source-pass-filename
      (concat (getenv "HOME") "/.local/share/password-store"))

(defun add-hooks (hook-list function)
  "Add FUNCTION to all hooks in HOOK-LIST."
  (dolist (hook hook-list)
    (add-hook hook function)))

(setq org-directory "~/org/")
(setq org-agenda-files (list org-directory))

(setq org-use-property-inheritance t)
(setq org-log-done 'time) ; Log time when task completes
(setq org-list-allow-alphabetical t)       ; a, A, a) A) list bullets)
(setq org-catch-invisible-edits 'smart) ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{})

(setq org-return-follows-link 1)
(setq calendar-week-start-day 1) ;; start on monday
(setq org-agenda-include-diary t)

(defun set-org-headline-color ()
  "Set the org headline colors to darker variants of the foreground color."
  (dotimes (i 8)
    (set-face-foreground (intern (format "org-level-%d" (1+ i))) (doom-color 'fg)))
  (set-face-foreground 'org-document-title (doom-color 'fg)))

(add-hook 'org-mode-hook 'set-org-headline-color)

(setq org-hide-leading-stars nil)

(setq org-startup-indented nil)

(add-hook 'org-mode-hook #'mixed-pitch-mode)

(add-hook 'org-mode-hook
          (lambda () (setq line-spacing 0.2)))

(use-package! org-modern
  :defer t
  :hook (org-mode . global-org-modern-mode)
  :config
  (setq org-modern-label-border 0.1
        org-modern-star 'replace))

(setq
  org-auto-align-tags nil
  org-tags-column 0
  org-catch-invisible-edits 'show-and-error
  org-special-ctrl-a/e t
  org-insert-heading-respect-content t

  ;; Org styling, hide markup etc
  org-hide-emphasis-markers t
  org-pretty-entities t
  org-ellipsis "..."

  ;; Agenda styling
  org-agenda-tags-column 0
  org-agenda-block-separator ?─
  org-agenda-time-grid
  '((daily today require-timed)
    (800 1000 1200 1400 1600 1800 2000)
    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  org-agenda-current-time-string
  "⭠ now ─────────────────────────────────────────────────"

  org-modern-block-name
  '(("src" . ""))

  ;; Org-Modern settings
  org-modern-star 'nil ;; Use old org-modern star icons
)

;; customize org-modern face
(custom-set-faces!
  '(org-modern-label :height 1.0))


(global-org-modern-mode)

(setq prettify-symbols-alist
      '(("CLOCK:" . ?)
        (":LOGBOOK:" . ?)
        (":END:" . ?-)))

(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

(setq org-fontify-quote-and-verse-blocks t)

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(use-package! org-tidy
  :defer t
  :hook (org-mode . org-tidy-mode)
  :config (map! :map org-mode-map
                :localleader
                :desc "Toggle org-tidy" "z" #'org-tidy-mode))

(use-package! ob-http
  :defer t
  :commands org-babel-execute:http)

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noeweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))

(defun org-babel-tangle-config ()
  (when (string-equal (file-name-nondirectory (buffer-file-name))
                      "config.org")
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle-config)))

(setq org-export-headline-levels 5)

(setq org-highlight-latex-and-related '(native script entities))

(setq ob-mermaid-cli-path (shell-command-to-string "printf %s \"$(readlink -f $(which mmdc))\""))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)))

(setq org-roam-directory (concat org-directory "roam"))

(use-package! websocket
  :after org-roam
  :defer t)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-synch-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; (load (expand-file-name "org-roam-logseq.el" doom-user-dir))

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(add-hook! 'emacs-startup-hook #'doom-init-ui-h)

(use-package! org-tempo)

(use-package! org-autolist
  :hook (org-mode . org-autolist-mode))

(after! org
  (setq org-capture-templates
        '(("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox")
          "* TODO %?\n%i\n%a" :prepend t)
         ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox")
          "* %u %?\n%i\n%a" :prepend t)
         ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
          "* %U %?\n%i\n%a" :prepend t)
         ("p" "Templates for projects")
         ("pt" "Project-local todo" entry
          (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a"
          :prepend t)
         ("pn" "Project-local notes" entry
          (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a"
          :prepend t)
         ("pc" "Project-local changelog" entry
          (file+headline +org-capture-project-changelog-file "Unreleased")
          "* %U %?\n%i\n%a" :prepend t)
         ("o" "Centralized templates for projects")
         ("ot" "Project todo" entry #'+org-capture-central-project-todo-file
          "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
         ("on" "Project notes" entry #'+org-capture-central-project-notes-file
          "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
         ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file
          "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))))

(after! org-capture
    (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
    Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
            (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
                '(("t" "Task" entry (file+headline "" "Tasks")
                    "* TODO %?\n  %u\n  %a")))))
        (if keys
            (or (assoc keys org-capture-templates)
                (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                "Template key: "
                `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
    (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier))

(setf (alist-get 'height +org-capture-frame-parameters) 15)
      ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(use-package! khalel
  :after org
  :config
  (khalel-add-capture-template))

(setq khalel-khal-command (shell-command-to-string "printf %s \"$(readlink -f $(which khal))\""))
(setq khalel-vdirsyncer-command "vdirsyncer")

(setq khalel-capture-key "e")
(setq khalel-import-org-file (concat org-directory "/" "calendar.org"))

(setq khalel-import-org-file-confirm-overwrite nil)

(setq khalel-import-end-date "+30d")

(khalel-add-capture-template)

(setq projectile-project-search-path '(("~/Code/" . 1)))

(setq +workspaces-on-switch-project-behavior nil)

(use-package! jest
  :after (typescript-mode js-mode typescript-tsx-mode)
  :hook (typescript-mode . jest-minor-mode))

(setq find-sibling-rules
      '(("src/\\(.*/\\)?\\([^/]+\\)\\.\\(ts\\|vue\\)\\'"
         "test/.*\\2.test.ts")
        ("test/\\(.*/\\)?\\([^/]+\\)\\.test.ts\\'"
         "src/.*\\2.\\(ts\\|vue\\)")))

;; (advice-add 'lsp
;;             :before (lambda (&rest _args)
;;                       (setf (lsp-session-server-id->folders (lsp-session)) (ht))))

(use-package! svelte-mode
  :defer t
  :mode "\\.svelte\\'")

(with-eval-after-load 'web-mode
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(setq treesit-language-source-alist
      '((astro "https://github.com/virchau13/tree-sitter-astro")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '(("\\.astro\\'" . astro-mode))
              auto-mode-alist))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(astro-mode . "astro"))
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls"))
                    :activation-fn (lsp-activate-on "astro")
                    :server-id 'astro-ls)))

(use-package! lsp-tailwindcss
  :defer t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode typescript-mode typescript-tsx-mode rust-mode rustic-mode))
  )

(setq typescript-indent-level 2)
(setq js-indent-level 2)

(use-package! nix-mode
  :mode "\\.nix\\'")

(add-hook! python-mode
  (advice-add 'python-pytest-file :before
              (lambda (&rest args)
                (setq-local python-pytest-executable
                            (executable-find "pytest")))))

(setq lsp-terraform-ls-enable-show-reference t)
(setq lsp-semantic-tokens-enable t)
(setq lsp-semantic-tokens-honor-refresh-requests t)

(setq flymake-allowed-file-name-masks nil)

(defcustom lsp-jsonnet-executable "jsonnet-language-server"
  "The jsonnet executable to use for the jsonnet language server."
  :group 'lsp-jsonnet
  :risky t
  :type 'file)

(with-eval-after-load 'lsp-mode
  ;; Configure lsp-mode-language identifiers
  (add-to-list 'lsp-language-id-configuration '(jsonnet-mode . "jsonnet"))

  ;; Register jsonnet-language-server with the LSP client
  (lsp-register-client
    (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () lsp-jsonnet-executable))
      :activation-fn (lsp-activate-on "jsonnet")
      :initialized-fn (lambda (workspace)
                        (with-lsp-workspace workspace
                          (lsp--set-configuration
                            (lsp-configuration-section "jsonnet"))))
                    :server-id 'jsonnet-language-server))

  ;; Start language server when jsonnet-mode is enabled
  (add-hook 'jsonnet-mode-hook #'lsp-deferred))

(setq lsp-rust-features "all")

(setq auto-mode-alist
      (append '(("\\.mdx\\'" . markdown-mode))
              auto-mode-alist))

(use-package! gleam-ts-mode
  :config
  ;; setup formatter to be used by `SPC c f`
  (after! apheleia
    (setf (alist-get 'gleam-ts-mode apheleia-mode-alist) 'gleam)
    (setf (alist-get 'gleam apheleia-formatters) '("gleam" "format" "--stdin"))))

(after! treesit
  (add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-ts-mode)))

(after! gleam-ts-mode
  (unless (treesit-language-available-p 'gleam)
    ;; compile the treesit grammar file the first time
    (gleam-ts-install-grammar)))

(setq  corfu-auto-delay 0.1
       corfu-auto-prefix 2
       corfu-left-margin-width 2
       corfu-right-margin-width 2
       corfu-bar-width 1)

(setq global-corfu-minibuffer nil)

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun adjust-corfu-colors ()
  "Adjust corfu colors to match the current theme"
  (set-face-background 'corfu-border (doom-darken 'bg 0.25))
  (set-face-background 'corfu-current (doom-lighten 'bg 0.25)))

(eval-after-load 'corfu '(adjust-corfu-colors))

(setq corfu--frame-parameters '((internal-border-width . 5)
                                (min-width . 80)
                                (max-width . 100)))

(setq corfu--buffer-parameters '((mode-line-format . nil)
                                 (header-line-format . nil)
                                 (left-margin-width . 2)
                                 (right-margin-width . 2)
                                 (fringes-outside-margins . 0)))

(setq corfu-popupinfo-delay '(0.1 . 0.05)
      corfu-popupinfo-hide nil
      corfu-popupinfo-max-width 160
      corfu-popupinfo-min-width 160
      corfu-popupinfo-max-height 30
      corfu-popupinfo--buffer-parameters '((truncate-lines . nil)
                                           (left-margin-width . 2)
                                           (right-margin-width . 2)
                                           (word-wrap . t)))

(use-package! copilot
  :defer t
  :hook
  (prog-mode . copilot-mode)
  (copilot-mode . (lambda ()
                    (setq-local copilot--indent-warning-printed-p t)))
  :bind (:map copilot-completion-map
              ("C-<space>" . 'copilot-accept-completion)
              ("C-SPC" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(map! :leader
      (:prefix-map ("i" . "insert")
       (:prefix ("g" . "github copilot")
        :desc "Show Copilot Completion" "s" #'copilot-complete
        :desc "Insert Copilot Completion" "c" #'copilot-accept-completion))
      (:prefix ("t" . "toggle")
       :desc "Toggle Copilot" "p" #'copilot-mode))

(use-package! aidermacs
  :defer t
  :hook (aidermacs-minor-mode . (lambda () (setenv "OPENAI_API_KEY" (password-store-get "bitwarden/openai-gpt-key"))))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "4o"))

(setq dap-python-debugger 'debugpy)

;;;###autoload
(defun +debugger/clear ()
  "Clear the debugger configuration from the doom-store."
  (interactive)
  (doom-store-rem (doom-project-root) "+debugger"))

(setq debugger-start-copy (symbol-function '+debugger/start))

;;;###autoload
(defun +debugger/repeat (arg)
  "Start the debugger."
  (interactive)
  (funcall debugger-start-copy arg))

;;;###autoload
(defun +debugger/start (arg)
  "Launch a debugger session.
Launches the last used debugger, if one exists. Otherwise, you will be prompted
for what debugger to use. If the prefix ARG is set, prompt anyway."
  (interactive "P")
  (message arg)
  (+debugger--set-config (+debugger-completing-read))
  (+debugger/start-last))

(defun get-window-with-file-buffer ()
  "Get the window with a file buffer."
  (seq-find (lambda (window)
              (buffer-file-name (window-buffer window)))
            (window-list)))

(defun reset-file-window-buffer ()
  "Reset the file window's buffer."
  (let ((window (get-window-with-file-buffer)))
    (when window
      (set-window-buffer window (window-buffer window)))))

(defun add-reset-file-window-buffer-hook (&rest args)
  "Add the reset-file-window-buffer function to the window-configuration-change-hook."
  (add-hook 'window-configuration-change-hook 'reset-file-window-buffer))

(defun remove-reset-file-window-buffer-hook (&rest args)
    "Remove the reset-file-window-buffer function from the window-configuration-change-hook."
    (remove-hook 'window-configuration-change-hook 'reset-file-window-buffer))

(add-hook 'dap-mode-hook 'add-reset-file-window-buffer-hook)

(map! :leader
      (:prefix-map ("d" . "debugger")
       :desc "Debug" "d" #'dap-debug
       :desc "Next" "n" #'dap-next
       :desc "Step in" "i" #'dap-step-in
       :desc "Step out" "o" #'dap-step-out
       :desc "Continue" "c" #'dap-continue
       :desc "Restart" "r" #'dap-restart-frame
       :desc "Disconnect" "D" #'dap-disconnect
       :desc "Evaluate" "e" #'dap-eval
       :desc "Add Expression" "a" #'dap-ui-expressions-add
       (:prefix ("b" . "breakpoints")
        :desc "Toggle" "t" #'dap-breakpoint-toggle
        :desc "Add" "a" #'dap-breakpoint-add
        :desc "Delete" "d" #'dap-breakpoint-delete
        :desc "Set condition" "c" #'dap-breakpoint-condition
        :desc "Set log message" "m" #'dap-breakpoint-log-message
        :desc "Set hit condition" "h" #'dap-breakpoint-hit-condition)))

(setq flycheck-syntax-automatically '(save-mode-enable))

(setq lsp-use-plists 't)

(setq lsp-completion-provider :capf)

(setq lsp-completion-show-detail t)

(setq lsp-completion-show-kind t)

(setq lsp-auto-guess-root t)
(add-hook 'prog-mode-hook #'lsp-deferred)

(map! :leader
      (:prefix ("c" . "code")
       :desc "Glance at documentation" "g" #'lsp-ui-doc-glance))

(setq lsp-lens-enable t)

(setq lsp-headerline-breadcrub-enable t)

(setq lsp-eldock-enable-hover nil)

(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)

(setq lsp-ui-doc-max-height 400
      lsp-ui-doc-max-width 250)

(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package! lab
  :config
  (setq lab-host "https://gitlab.com")
  (setq lab-token (password-store-get "bitwarden/gitlab-token")))

(map! :leader
      :desc "List Pipelines" "g l p" #'lab-list-project-pipelines
      :desc "List Merge Requests" "g l m" #'lab-list-project-merge-requests
      :desc "List all owned projects" "g l o" #'lab-list-all-owned-projects)

(map! :leader
      (:prefix ("D" . "devdocs")
       :desc "Open devdocs" "o" #'devdocs-peruse
       :desc "Search devdocs" "l" #'devdocs-lookup
       :desc "Install devdocs set" "i" #'devdocs-install))

(use-package! gptel
  :config
  (setq! gptel-api-key (lambda () (password-store-get "bitwarden/openai-gpt-key")))
  (setq! gptel-model "gpt-4"))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Open GPTel" "g" #'gptel))

(use-package! justl
  :config

  (map! :leader
        (:prefix ("c" . "Code")
         :desc "Make" "m" #'justl))
  (map! :n "e" 'justl-exec-recipe))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc -f markdown -t html5"))

(use-package simple-httpd
  :config
  (setq httpd-port 7070))

(use-package impatient-mode
  :commands impatient-mode)

(defun markdown-html-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
        (set-buffer buffer)
        (set-buffer (markdown tmp))
        (format "<!DOCTYPE html><html><title>Markdown Preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/><body><article class=\"markdown-body\">%s</article></body></html>" (buffer-string))))
    (current-buffer)))

(defun markdown-html-preview ()
  "Preview Markdown in browser."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'markdown-html-filter)
  (imp-visit-buffer))

(defun markdown-html-preview-stop ()
  "Stop previewing Markdown in browser."
  (interactive)
  (imp-visit-buffer)
  (impatient-mode -1))

(map! :leader
      (:prefix ("m" . "markdown")
       :desc "Preview" "p" #'markdown-html-preview
       :desc "Stop Preview" "s" #'markdown-html-preview-stop))

(setq catppuccin-flavor 'frappe)

(setq doom-theme 'doom-nord-aurora)

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p nil
              ewal-built-in-palette "sexy-material"))

(setq doom-modeline-vcs-max-length 50)

(setq doom-modeline-hud t)

(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-modal nil)
  (setq doom-modeline-column-format "")
  (setq size-indication-mode nil)
  (setq doom-modeline-bar-width 0))

(after! doom-modeline
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
  (line-number-mode -1))

(add-hook 'treemacs-mode-hook (lambda () (hide-mode-line-mode)))

(use-package! spacious-padding
  :config
  (setq spacious-padding-width '(
    :internal-border-width 15
    :header-line-width 4
    :mode-line-width 6
    :tab-width 8
    :right-divider-width 30
    :scroll-bar-width 8)))

(setq spacious-padding-subtle-mode-line t)

(spacious-padding-mode 1)

(when (featurep! :checkers syntax +childframe)
  (defun flycheck-posframe-monitor-post-command ()
    (when (not (flycheck-posframe-check-position))
      (posframe-hide flycheck-posframe-buffer)))

  (defun fix-flycheck-posframe-not-hide-immediately ()
    (cond (flycheck-posframe-mode
           (add-hook 'post-command-hook 'flycheck-posframe-monitor-post-command nil t))
          ((not flycheck-posframe-mode)
           (remove-hook 'post-command-hook 'flycheck-posframe-monitor-post-command t))))
  (add-hook! flycheck-posframe-mode #'fix-flycheck-posframe-not-hide-immediately))

(add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    " > " "   ")))
    (funcall orig cand prefix suffix index _start)))

(advice-add #'vertico--format-candidate
            :around #'minibuffer-format-candidate)

(setq vertico-count-format nil)

(setq vertico-posframe-width 200)

(setq vertico-posframe-parameters
      '((left-fringe . 16)
        (right-fringe . 8)
        (border-width . 16)))

(setq read-process-output-max (* 4 1024 1024)) ;; 4mb

(fset #'jsonrpc--log-event #'ignore)

(use-package! elcord
  :config
  (setq elcord-editor-icon "emacs_icon"))
