(setq user-full-name "Tibor Pilz"
      user-mail-address "tibor@pilz.berlin")

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'light)
      doom-big-font (font-spec :family "FiraCode Nerd Font" :size 28 :weight 'normal)
      doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 16 :weight 'normal))

(setq display-line-numbers-type 'relative)

(setq tab-width 2)

(setq fancy-splash-image (concat doom-private-dir "splash-logos/emacs-logo-cutout.svg"))

(setq frame-title-format "%b - Emacs")

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

(add-hook 'org-mode-hook #'mixed-pitch-mode)

(defun set-line-spacing (size)
  "Set line spacing"
  (setq line-spacing size))

(add-hook 'org-mode-hook
          (lambda () (set-line-spacing 0.2)))

(setq org-hide-leading-stars t)

(setq org-startup-indented t)

(use-package! org-modern
  :hook (org-mode . global-org-modern-mode)
  :config
  (setq org-modern-label-border 0.4))

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
  "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

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

(use-package! ob-http
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

(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")

(use-package! ox-gfm :after ox :defer t)

(setq org-export-headline-levels 5)

(setq org-highlight-latex-and-related '(native script entities))

(setq org-roam-directory "~/org/roam")

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

(map! :map org-mode-map
      :localleader
      (:prefix-map ("B" . "babel")
       (:desc "Insert structure template" "c" #'org-insert-structure-template)))

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(evil-define-command evil-buffer-org-new (count file)
  "creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
      (:prefix "b"
       :desc "new empty ORG buffer" "o" #'evil-buffer-org-new))

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(defadvice! org-edit-latex-env-after-insert ()
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))

(add-hook! markdown-mode (auto-fill-mode -1))

(add-hook! 'emacs-startup-hook #'doom-init-ui-h)

(use-package! org-tempo)

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

(setq projectile-project-search-path '(("~/Code/" . 1)))

(setq +workspaces-on-switch-project-behavior nil)

(use-package! jest
  :after (typescript-mode js-mode typescript-tsx-mode)
  :hook (typescript-mode . jest-minor-mode))

;; (advice-add 'lsp
;;             :before (lambda (&rest _args)
;;                       (setf (lsp-session-server-id->folders (lsp-session)) (ht))))

(use-package! svelte-mode
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

(setq company-idle-delay 0.1 ;; How long to wait before popping up
      company-minimum-prefix-length 1 ;; Show the menu after one key press
      company-tooltip-limit 10 ;; Limit on how many options to display
      company-tooltip-align-annotations t ;; Align annotations to the right
      company-require-match nil           ;; Allow free typing
      company-selection-wrap-around t ;; Wrap around to beginning when you hit bottom of suggestions
      )

(after! lsp-mode
  (setq company-backends '(company-capf)))

(setq company-ispell-available nil)

(setq company-format-margin-function #'company-vscode-dark-icons-margin)

(use-package! copilot
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

(setq lsp-ui-doc-max-height 40
      lsp-ui-doc-max-width 80)

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

(use-package! gptel-extensions :after gptel)

(use-package quarto-mode
  :mode (("\\.Rmd" . poly-quarto-mode)))

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

(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'frappe)

(setq doom-themes-treemacs-theme "doom-colors")

(with-eval-after-load 'doom-themes
  (doom-themes-treemacs-config))

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p nil
              ewal-built-in-palette "sexy-material"))

(setq doom-modeline-vcs-max-length 50)

(setq doom-modeline-hud t)

(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-modal nil)
(setq doom-modeline-column-format "")
(setq size-indication-mode nil)

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

;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
;; (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)

(use-package! all-the-icons-ivy-rich
  :defer t
  :after counsel-projectile
  :init (all-the-icons-ivy-rich-mode +1)
  :config
  (setq all-the-icons-ivy-rich-icon-size 0.8))

(setq ivy-posframe-width 80)

(use-package! treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

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

(use-package! xwwp-full
  :after xwidget-webkit
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("f" . xwwp-ace-toggle)
              ("v" . xwwp-follow-link)))

(setq read-process-output-max (* 4 1024 1024)) ;; 4mb

(fset #'jsonrpc--log-event #'ignore)
