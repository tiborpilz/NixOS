;; [[file:config.org::*Personal data][Personal data:1]]
(setq user-full-name "Tibor Pilz"
      user-mail-address "tibor@pilz.berlin")
;; Personal data:1 ends here

;; [[file:config.org::*Add checks for different machines][Add checks for different machines:1]]
(defun is-mac ()
  (string-equal system-type "darwin"))

(defun is-linux ()
  (string-equal system-type "gnu/linux"))

(defun is-workstation ()
  (string-equal (system-name) "archyMcArchstation"))
;; Add checks for different machines:1 ends here

;; [[file:config.org::*Font selection][Font selection:1]]
(setq font-scale-factor (if (is-workstation) 1.1 1.0))

(defun scale-font (size)
  (round (* size font-scale-factor)))
;; Font selection:1 ends here

;; [[file:config.org::*Font selection][Font selection:2]]
(when (is-mac) (setq doom-font (font-spec :family "FiraCode Nerd Font" :size (scale-font 14))) nil)
;; (setq doom-font (font-spec :family "FiraCode Nerd Font" :size (scale-font 16) :weight 'light)
;;       doom-big-font (font-spec :family "FiraCode Nerd Font" :size (scale-font 24))
;;       doom-variable-pitch-font (font-spec :family "Open Sans" :size (scale-font 16))
;;       doom-serif-font (font-spec :family "FreeSerif" :weight 'light))
;; Font selection:2 ends here

;; [[file:config.org::*Line numbers][Line numbers:1]]
(setq display-line-numbers-type 'relative)
;; Line numbers:1 ends here

;; [[file:config.org::*Tab width][Tab width:1]]
(setq tab-width 2)
;; Tab width:1 ends here

;; [[file:config.org::*Base Settings][Base Settings:1]]
(setq org-directory "~/org/")
(setq org-agenda-files (list org-directory))

(setq org-use-property-inheritance t)
(setq org-log-done 'time) ; Log time when task completes
(setq org-list-allow-alphabetical t)       ; a, A, a) A) list bullets)
(setq org-catch-invisible-edits 'smart) ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{})


(setq org-return-follows-link 1)
(setq calendar-week-start-day 1) ;; start on monday
(setq org-agenda-include-diary t)
;; Base Settings:1 ends here

;; [[file:config.org::*mixed pitch & org-pretty-mode][mixed pitch & org-pretty-mode:1]]
(add-hook 'org-mode-hook #'+org-pretty-mode)
;; mixed pitch & org-pretty-mode:1 ends here

;; [[file:config.org::*Show passed deadlines as error][Show passed deadlines as error:1]]
(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))
;; Show passed deadlines as error:1 ends here

;; [[file:config.org::*Show quote blocks in italic][Show quote blocks in italic:1]]
(setq org-fontify-quote-and-verse-blocks t)
;; Show quote blocks in italic:1 ends here

;; [[file:config.org::*Defer font-lock][Defer font-lock:1]]
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))
;; Defer font-lock:1 ends here

;; [[file:config.org::*Symbols][Symbols:1]]
(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t ))

(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
         (?E . 'all-the-icons-blue)))
;; Symbols:1 ends here

;; [[file:config.org::*HTTP requests via babel][HTTP requests via babel:1]]
(use-package! ob-http
  :commands org-babel-execute:http)
;; HTTP requests via babel:1 ends here

;; [[file:config.org::*Babel header args][Babel header args:1]]
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noeweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))
;; Babel header args:1 ends here

;; [[file:config.org::*LSP in org-babel src blocks][LSP in org-babel src blocks:1]]
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name fie)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("go" "python" "ipython" "bash" "sh" "ditaa"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))
;; LSP in org-babel src blocks:1 ends here

;; [[file:config.org::*Auto-Tanglins][Auto-Tanglins:1]]
(defun org-babel-tangle-config ()
  (when (string-equal (file-name-nondirectory (buffer-file-name))
                      "config.org")
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle-config)))
;; Auto-Tanglins:1 ends here

;; [[file:config.org::*View exported file][View exported file:1]]
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
;; View exported file:1 ends here

;; [[file:config.org::*use github markdown][use github markdown:1]]
(use-package! ox-gfm :after ox)
;; use github markdown:1 ends here

;; [[file:config.org::*Export headings up to five levels deep][Export headings up to five levels deep:1]]
(setq org-export-headline-levels 5)
;; Export headings up to five levels deep:1 ends here

;; [[file:config.org::*Ignore tag][Ignore tag:1]]
;(require 'ox-extra)
;(ox-extras-activate '(ignore-headlines))
;; Ignore tag:1 ends here

;; [[file:config.org::*automatic latex rendering][automatic latex rendering:1]]
(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))
;; automatic latex rendering:1 ends here

;; [[file:config.org::*Latex fragments][Latex fragments:1]]
(setq org-highlight-latex-and-related '(native script entities))
;; Latex fragments:1 ends here

;; [[file:config.org::*Export to Reveal.js][Export to Reveal.js:1]]
;(use-package! org-re-reveal)
;; Export to Reveal.js:1 ends here

;; [[file:config.org::*Use the same directory as org][Use the same directory as org:1]]
(setq org-roam-directory "~/org")
;; Use the same directory as org:1 ends here

;; [[file:config.org::*Add Org-Roam UI][Add Org-Roam UI:2]]
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive    )
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

(setq org-roam-ui-open-on-start nil)
;; Add Org-Roam UI:2 ends here

;; [[file:config.org::*Google Calendar integration][Google Calendar integration:2]]
;; (use-package! org-gcal
;;   :config
;;   (setq org-gcal-client-id "CLIENT_ID"
;;         org-gcal-client-secret "CLIENT_SECRET"
;;         org-gcal-fetch-file-alit '(("tbrpilz@googlemail.com" . "~/org/schedule.org"))))
;; Google Calendar integration:2 ends here

;; [[file:config.org::*Visual-line-mode messes with with plaintext (markdow, latex)][Visual-line-mode messes with with plaintext (markdow, latex):1]]
(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
;; Visual-line-mode messes with with plaintext (markdow, latex):1 ends here

;; [[file:config.org::*Prevent org-block face for latex fragments, since they look weird][Prevent org-block face for latex fragments, since they look weird:1]]
(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
;; Prevent org-block face for latex fragments, since they look weird:1 ends here

;; [[file:config.org::*Function to create an org buffer][Function to create an org buffer:1]]
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
;; Function to create an org buffer:1 ends here

;; [[file:config.org::*Insert cdlatex enviornments and edit immediately][Insert cdlatex enviornments and edit immediately:1]]
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(defadvice! org-edit-latex-env-after-insert ()
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))
;; Insert cdlatex enviornments and edit immediately:1 ends here

;; [[file:config.org::*Disable auto-fill-mode][Disable auto-fill-mode:1]]
(add-hook! markdown-mode (auto-fill-mode -1))
;; Disable auto-fill-mode:1 ends here

;; [[file:config.org::*Disable auto-fill-mode][Disable auto-fill-mode:2]]
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))
;; Disable auto-fill-mode:2 ends here

;; [[file:config.org::*Nix-Doom-Emacs messes with dashboard][Nix-Doom-Emacs messes with dashboard:1]]
(add-hook! 'emacs-startup-hook #'doom-init-ui-h)
;; Nix-Doom-Emacs messes with dashboard:1 ends here

;; [[file:config.org::*Project Search Path][Project Search Path:1]]
;; (setq projectile-project-search-path '(("~/Code/" . 2)))
;; Project Search Path:1 ends here

;; [[file:config.org::*Jest Test Mode][Jest Test Mode:2]]
(use-package! jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))
;; Jest Test Mode:2 ends here

;; [[file:config.org::*Svelte][Svelte:2]]
(use-package! svelte-mode
    :mode "\\.svelte\\'")
;; Svelte:2 ends here

;; [[file:config.org::*Formatting][Formatting:1]]
(with-eval-after-load 'web-mode
  (setq web-mode-script-padding 0))
;; Formatting:1 ends here

;; [[file:config.org::*Tailwind][Tailwind:2]]
(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))
;; Tailwind:2 ends here

;; [[file:config.org::*Code formatting][Code formatting:1]]
(setq typescript-indent-level 2)
;; Code formatting:1 ends here

;; [[file:config.org::*nix-mode.el][nix-mode.el:1]]
(use-package! nix-mode
  :mode "\\.nix\\'")
;; nix-mode.el:1 ends here

;; [[file:config.org::*Company-nixos-options][Company-nixos-options:2]]
(add-to-list 'company-backends 'company-nixos-options)
;; Company-nixos-options:2 ends here

;; [[file:config.org::*Nix-sandbox][Nix-sandbox:2]]
(setq flycheck-command-wrapper-function
        (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
      flycheck-executable-find
        (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))
;; Nix-sandbox:2 ends here

;; [[file:config.org::*Run pytest in virtualenv][Run pytest in virtualenv:1]]
(add-hook! python-mode
  (advice-add 'python-pytest-file :before
              (lambda (&rest args)
                (setq-local python-pytest-executable
                            (executable-find "pytest")))))
;; Run pytest in virtualenv:1 ends here

;; [[file:config.org::*Code blocks][Code blocks:1]]
(use-package! polymode)
(use-package! poly-markdown)
;; Code blocks:1 ends here

;; [[file:config.org::*Live Preview][Live Preview:2]]
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
      (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))
;; Live Preview:2 ends here

;; [[file:config.org::*Handling][Handling:1]]
(setq company-idle-delay
      (lambda () (if (company-in-string-or-comment) nil 0.01)))
;; Handling:1 ends here

;; [[file:config.org::*Handling][Handling:2]]
(setq company-minimum-prefix-length 1)
;; Handling:2 ends here

;; [[file:config.org::*Handling][Handling:3]]
(setq company-selection-wrap-around t)
;; Handling:3 ends here

;; [[file:config.org::*Backends][Backends:1]]
(setq company-backends '((company-capf company-dabbrev-code)))
;; Backends:1 ends here

;; [[file:config.org::*Looks][Looks:1]]
(setq company-format-margin-function #'company-vscode-dark-icons-margin)
;; Looks:1 ends here

;; [[file:config.org::*Copilot][Copilot:3]]
(nvm-use "16" (lambda ()
                (setq copilot-node-executable
                      (concat
                       (nth 1 (nvm--find-exact-version-for "16"))
                       "/bin/node"))
                (use-package! copilot
                  :bind ("<backtab>" . 'copilot-accept-completion))))
;; Copilot:3 ends here

;; [[file:config.org::*Python][Python:1]]
(setq dap-python-debugger 'debugpy)
;; Python:1 ends here

;; [[file:config.org::*Fix Doom "+debugger/start"][Fix Doom "+debugger/start":1]]
;;;###autoload
(defun +debugger/clear ()
  "Clear the debugger configuration from the doom-store."
  (interactive)
  (doom-store-rem (doom-project-root) "+debugger"))
;; Fix Doom "+debugger/start":1 ends here

;; [[file:config.org::*Fix Doom "+debugger/start"][Fix Doom "+debugger/start":2]]
(setq debugger-start-copy (symbol-function '+debugger/start))

;;;###autoload
(defun +debugger/repeat (arg)
  "Start the debugger."
  (interactive)
  (funcall debugger-start-copy arg))
;; Fix Doom "+debugger/start":2 ends here

;; [[file:config.org::*Fix Doom "+debugger/start"][Fix Doom "+debugger/start":3]]
;;;###autoload
(defun +debugger/start (arg)
  "Launch a debugger session.
Launches the last used debugger, if one exists. Otherwise, you will be prompted
for what debugger to use. If the prefix ARG is set, prompt anyway."
  (interactive "P")
  (message arg)
  (+debugger--set-config (+debugger-completing-read))
  (+debugger/start-last))
;; Fix Doom "+debugger/start":3 ends here

;; [[file:config.org::*Get the window containing a file buffer][Get the window containing a file buffer:1]]
(defun get-window-with-file-buffer ()
  "Get the window with a file buffer."
  (seq-find (lambda (window)
              (buffer-file-name (window-buffer window)))
            (window-list)))
;; Get the window containing a file buffer:1 ends here

;; [[file:config.org::*Reset file buffer window][Reset file buffer window:1]]
(defun reset-file-window-buffer ()
  "Reset the file window's buffer."
  (let ((window (get-window-with-file-buffer)))
    (when window
      (set-window-buffer window (window-buffer window)))))
;; Reset file buffer window:1 ends here

;; [[file:config.org::*Add reset to window configuration change hook][Add reset to window configuration change hook:1]]
(defun add-reset-file-window-buffer-hook (&rest args)
  "Add the reset-file-window-buffer function to the window-configuration-change-hook."
  (add-hook 'window-configuration-change-hook 'reset-file-window-buffer))

(defun remove-reset-file-window-buffer-hook (&rest args)
    "Remove the reset-file-window-buffer function from the window-configuration-change-hook."
    (remove-hook 'window-configuration-change-hook 'reset-file-window-buffer))

(add-hook 'dap-mode-hook 'add-reset-file-window-buffer-hook)
;; Add reset to window configuration change hook:1 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
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
;; Keybindings:1 ends here

;; [[file:config.org::*Syntax Checking][Syntax Checking:1]]
(setq flycheck-syntax-automatically '(save-mode-enable))
;; Syntax Checking:1 ends here

;; [[file:config.org::*Performance][Performance:1]]
(setq lsp-use-lists 't)
;; Performance:1 ends here

;; [[file:config.org::*Handling][Handling:1]]
(setq lsp-completion-provider :capf)
;; Handling:1 ends here

;; [[file:config.org::*Handling][Handling:2]]
(setq lsp-completion-show-detail nil)
;; Handling:2 ends here

;; [[file:config.org::*Handling][Handling:3]]
(setq lsp-completion-show-kind t)
;; Handling:3 ends here

;; [[file:config.org::*UI][UI:1]]
(map! :leader
      (:prefix ("c" . "code")
       :desc "Glance at documentation" "g" #'lsp-ui-doc-glance))
;; UI:1 ends here

;; [[file:config.org::*UI][UI:2]]
(setq lsp-lens-enable t)
;; UI:2 ends here

;; [[file:config.org::*UI][UI:3]]
(setq lsp-headerline-breadcrub-enable t)
;; UI:3 ends here

;; [[file:config.org::*UI][UI:4]]
(setq lsp-eldock-enable-hover nil)
;; UI:4 ends here

;; [[file:config.org::*UI][UI:5]]
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
;; UI:5 ends here

;; [[file:config.org::*Disable Evil-Mode in timemachine mode][Disable Evil-Mode in timemachine mode:1]]
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))
;; Disable Evil-Mode in timemachine mode:1 ends here

;; [[file:config.org::*Doom Themes][Doom Themes:2]]
(setq doom-theme 'doom-nord-aurora)
;; Doom Themes:2 ends here

;; [[file:config.org::*Nano][Nano:1]]
;; (add-to-list 'load-path "~/Code/doom-nano-testing")
;; (require 'load-nano)
;; (setq doom-themes-treemacs-theme "doom-atom")
;; Nano:1 ends here

;; [[file:config.org::*Nano Modeline][Nano Modeline:2]]
(use-package! nano-modeline
  :config
  (nano-modeline-mode 1))
;; Nano Modeline:2 ends here

;; [[file:config.org::*Doom Modeline][Doom Modeline:1]]
(setq doom-modeline-vcs-max-length 50)
;; Doom Modeline:1 ends here

;; [[file:config.org::*Doom Modeline][Doom Modeline:2]]
(setq doom-modeline-hud t)
;; Doom Modeline:2 ends here

;; [[file:config.org::*Which-Key][Which-Key:2]]
(defun wjb/posframe-arghandler (buffer-or-name arg-name value)
  (let ((info '(:internal-border-width 2 :width 500 :height 48)))
    (or (plist-get info arg-name) value)))
(setq posframe-arghandler #'wjb/posframe-arghandler)
;; Which-Key:2 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
;; (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
;; Keybindings:1 ends here

;; [[file:config.org::*All-The-Icons Ivy Rich][All-The-Icons Ivy Rich:2]]
(use-package! all-the-icons-ivy-rich
  :after counsel-projectile
  :init (all-the-icons-ivy-rich-mode +1)
  :config
  (setq all-the-icons-ivy-rich-icon-size 0.8))
;; All-The-Icons Ivy Rich:2 ends here

;; [[file:config.org::*Ivy-Postframe][Ivy-Postframe:1]]
(setq ivy-posframe-width 80)
;; Ivy-Postframe:1 ends here

;; [[file:config.org::*Vertico][Vertico:1]]
(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    "  " "   ")))
    (funcall orig cand prefix suffix index _start)))

(advice-add #'vertico--format-candidate
            :around #'minibuffer-format-candidate)
;; Vertico:1 ends here

;; [[file:config.org::*Vertico][Vertico:2]]
(setq vertico-count-format nil)
;; Vertico:2 ends here

;; [[file:config.org::*Vertico][Vertico:3]]
(setq vertico-posframe-width 200)
;; Vertico:3 ends here

;; [[file:config.org::*Increase the amount of data which Emacs reads from the process][Increase the amount of data which Emacs reads from the process:1]]
;; (setq read-process-output-max (* 4 1024 1024)) ;; 4mb
;; Increase the amount of data which Emacs reads from the process:1 ends here
