(setq user-full-name "Tibor Pilz"
      user-mail-address "tibor@pilz.berlin")

(defun is-mac ()
  (string-equal system-type "darwin"))

(defun is-linux ()
  (string-equal system-type "gnu/linux"))

(defun is-workstation ()
  (string-equal (system-name) "workyMcWorkstation"))

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14))

(setq display-line-numbers-type 'relative)

(setq tab-width 2)

(use-package! exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "NVM_DIR" "NVM_BIN"))
  (exec-path-from-shell-initialize))

(setq org-directory "~/org/")
(setq org-agenda-files (list org-directory))

(setq org-use-property-inheritance t)
(setq org-log-done 'time) ; Log time when task completes
(setq org-list-allow-alphabetical t)       ; a, A, a) A) list bullets)
(setq org-catch-invisible-edits 'smart) ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{})

(setq org-return-follows-link 1)
(setq calendar-week-start-day 1) ;; start on monday
(setq org-agenda-include-diary t)

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

;(require 'ox-extra)
;(ox-extras-activate '(ignore-headlines))

(setq org-highlight-latex-and-related '(native script entities))

;; (use-package! org-re-reveal)

(setq org-roam-directory "~/org")

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

;; (use-package! org-roam-ui
;;   :after org-roam
;;   :commands org-roam-ui-open
;;   :hook (org-roam . 'org-roam-ui-mode)
;;   :config
;;   (require 'org-roam) ; in case autoloaded
;;   (defun org-roam-ui-open ()
;;     "Ensure the server is active, then open the roam graph."
;;     (interactive    )
;;     (unless org-roam-ui-mode (org-roam-ui-mode 1))
;;     (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

;; (setq org-roam-ui-open-on-start nil)

;; (use-package! org-gcal
;;   :config
;;   (setq org-gcal-client-id "CLIENT_ID"
;;         org-gcal-client-secret "CLIENT_SECRET"
;;         org-gcal-fetch-file-alit '(("tbrpilz@googlemail.com" . "~/org/schedule.org"))))

(use-package! org-present
  :hook (org-present-mode . (lambda ()
                              (org-present-big)
                              (org-display-inline-images)
                              (org-present-hide-cursor)
                              (org-present-read-only)))
  :hook (org-present-mode-quit . (lambda ()
                                   (org-present-small)
                                   (org-remove-inline-images)
                                   (org-present-show-cursor)
                                   (org-present-read-write))))

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

(use-package! jest
  :after (typescript-mode js-mode typescript-tsx-mode)
  :hook (typescript-mode . jest-minor-mode))

(use-package! svelte-mode
    :mode "\\.svelte\\'")

(with-eval-after-load 'web-mode
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package! lsp-tailwindcss
  :defer t
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(setq typescript-indent-level 2)

(use-package! nix-mode
  :mode "\\.nix\\'")

;; (setq flycheck-command-wrapper-function
;;         (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
;;       flycheck-executable-find
;;         (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

(add-hook! python-mode
  (advice-add 'python-pytest-file :before
              (lambda (&rest args)
                (setq-local python-pytest-executable
                            (executable-find "pytest")))))

;; (use-package! polymode
;; (use-package! poly-markdown)

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(setq lsp-terraform-ls-enable-show-reference t)
(setq lsp-semantic-tokens-enable t)
(setq lsp-semantic-tokens-honor-refresh-requests t)

(setq company-idle-delay 0.1 ;; How long to wait before popping up
      company-minimum-prefix-length 0 ;; Show the menu after one key press
      company-tooltip-limit 10 ;; Limit on how many options to display
      company-tooltip-align-annotations t ;; Align annotations to the right
      company-require-match nil           ;; Allow free typing
      company-selection-wrap-around t ;; Wrap around to beginning when you hit bottom of suggestions
      )

(after! lsp-mode
  (setq company-backends '(company-capf)))

(setq company-ispell-available nil)

(setq company-format-margin-function #'company-vscode-dark-icons-margin)

(defun call-nvm (args &optional as-string)
  (let ((nvm-command "source $XDG_CONFIG_HOME/zsh/.zshrc && nvm"))
    (if as-string
        (shell-command-to-string (concat nvm-command " " args))
      (shell-command (concat nvm-command " " args)))))

(defun install-node-if-missing ()
  (if (not (eq 0 (call-nvm "ls 16")))
      (call-nvm "install 16")))

(defun load-copilot ()
  (use-package! copilot
    :hook ((prog-mode text-mode) . copilot-mode)
    :bind (:map copilot-completion-map
           ("C-SPC" . 'copilot-accept-completion)
           ("C-<spc>" . 'copilot-accept-completion)
           ("C-S-p" . 'copilot-previous-completion)
           ("C-S-n" . 'copilot-next-completion))))

(if (and (boundp 'copilot-node-executable) (file-exists-p copilot-node-executable))
    (load-copilot)
    (nvm-use "16" (lambda ()
                   (setq copilot-node-executable
                         (concat
                          (nth 1 (nvm--find-exact-version-for "16"))
                          "/bin/node"))
                   (load-copilot))))

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

(setq lsp-use-lists 't)

(setq lsp-completion-provider :capf)

;; (setq lsp-completion-show-detail t)

(setq lsp-completion-show-kind t)

;; (setq lsp-auto-guess-root t)
;; (add-hook 'prog-mode-hook #'lsp-deferred)

(map! :leader
      (:prefix ("c" . "code")
       :desc "Glance at documentation" "g" #'lsp-ui-doc-glance))

(setq lsp-lens-enable t)

(setq lsp-headerline-breadcrub-enable t)

(setq lsp-eldock-enable-hover nil)

(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)

(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(map! :leader
      (:prefix ("D" . "devdocs")
       :desc "Open devdocs" "o" #'devdocs-peruse
       :desc "Search devdocs" "l" #'devdocs-lookup
       :desc "Install devdocs set" "i" #'devdocs-install))

;; (setq dash-docs-docsets-path "$HOME/.local/share/docsets")

(setq gpt-openai-key (password-store-get "bitwarden/openai-gpt-key"))
(setq gpt-openai-engine "code-davinci-002")
(use-package! gpt)

(use-package quarto-mode
  :mode (("\\.Rmd" . poly-quarto-mode)))

(setq doom-theme 'doom-nord-aurora)

;; (add-to-list 'load-path "~/Code/doom-nano-testing") (require 'load-nano)
;; (setq doom-themes-treemacs-theme "doom-atom")

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p nil
              ewal-built-in-palette "sexy-material"))

(setq doom-modeline-vcs-max-length 50)

(setq doom-modeline-hud t)

(setq spacious-padding-width '(:internal-border-width 10 :right-divider-width 30 :scroll-bar-width 5))

(setq fancy-splash-image (concat doom-private-dir "splash-logos/emacs-logo-cutout.svg"))

;; (defun wjb/posframe-arghandler (buffer-or-name arg-name value)
;;   (let ((info '(:internal-border-width 2 :width 500 :height 48)))
;;     (or (plist-get info arg-name) value)))
;; (setq posframe-arghandler #'wjb/posframe-arghandler)

;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
;; (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)

(use-package! all-the-icons-ivy-rich
  :defer t
  :after counsel-projectile
  :init (all-the-icons-ivy-rich-mode +1)
  :config
  (setq all-the-icons-ivy-rich-icon-size 0.8))

(setq ivy-posframe-width 80)

(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    "  " "   ")))
    (funcall orig cand prefix suffix index _start)))

(advice-add #'vertico--format-candidate
            :around #'minibuffer-format-candidate)

(setq vertico-count-format nil)

(setq vertico-posframe-width 200)

(use-package! xwwp-full
  :after xwidget-webkit
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("f" . xwwp-ace-toggle)
              ("v" . xwwp-follow-link)))

(setq read-process-output-max (* 4 1024 1024)) ;; 4mb
