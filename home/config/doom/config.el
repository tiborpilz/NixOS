;; [[file:config.org::*Personal data][Personal data:1]]
(setq user-full-name "Tibor Pilz"
      user-mail-address "tibor@pilz.berlin")
;; Personal data:1 ends here

;; [[file:config.org::*Font settings][Font settings:1]]
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'normal)
      doom-big-font (font-spec :family "FiraCode Nerd Font" :size 28 :weight 'light)
      doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 16 :weight 'light))
;; Font settings:1 ends here

;; [[file:config.org::*Line numbers][Line numbers:1]]
(setq display-line-numbers-type 'visual)
;; Line numbers:1 ends here

;; [[file:config.org::*Tab width][Tab width:1]]
(setq tab-width 2)
;; Tab width:1 ends here

;; [[file:config.org::*Splash Image][Splash Image:1]]
(setq fancy-splash-image (concat doom-private-dir "splash-logos/emacs-logo-cutout.svg"))
;; Splash Image:1 ends here

;; [[file:config.org::*Title][Title:1]]
(setq frame-title-format "%b - Emacs")
;; Title:1 ends here

;; [[file:config.org::*Password Store location][Password Store location:1]]
(setq auth-source-pass-filename
      (concat (getenv "HOME") "/.local/share/password-store"))
;; Password Store location:1 ends here

;; [[file:config.org::*Add-Hooks][Add-Hooks:1]]
(defun add-hooks (hook-list function)
  "Add FUNCTION to all hooks in HOOK-LIST."
  (dolist (hook hook-list)
    (add-hook hook function)))
;; Add-Hooks:1 ends here

;; [[file:config.org::*Base Settings][Base Settings:1]]
(setq org-directory "~/org/")

;; Function to rebuild agenda files including all roam files
(defun my/rebuild-org-agenda-files ()
  "Rebuild org-agenda-files to include all org files in roam directory."
  (interactive)
  (setq org-agenda-files 
        (append 
         (list org-directory)
         (directory-files-recursively (concat org-directory "roam") "\\.org$")))
  (message "Rebuilt org-agenda-files with %d files" (length org-agenda-files)))

;; Include ALL org files in roam directory recursively
(my/rebuild-org-agenda-files)

;; Performance optimization: larger cache for many files
(setq org-agenda-files-cache-time 600)  ; Cache for 10 minutes

(setq org-use-property-inheritance t)
(setq org-log-done 'time) ; Log time when task completes
(setq org-list-allow-alphabetical t)       ; a, A, a) A) list bullets)
(setq org-catch-invisible-edits 'smart) ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{})

(setq org-return-follows-link 1)
(setq calendar-week-start-day 1) ;; start on monday
(setq org-agenda-include-diary t)
;; Base Settings:1 ends here

;; [[file:config.org::*Headlines][Headlines:1]]
(defun set-org-headline-color ()
  "Set the org headline colors to darker variants of the foreground color."
  (dotimes (i 8)
    (set-face-foreground (intern (format "org-level-%d" (1+ i))) (doom-color 'fg)))
  (set-face-foreground 'org-document-title (doom-color 'fg)))

(add-hook 'org-mode-hook 'set-org-headline-color)
;; Headlines:1 ends here

;; [[file:config.org::*Headlines][Headlines:2]]
(setq org-hide-leading-stars nil)
;; Headlines:2 ends here

;; [[file:config.org::*Headlines][Headlines:3]]
(setq org-startup-indented nil)
;; Headlines:3 ends here

;; [[file:config.org::*Fonts][Fonts:1]]
(add-hook 'org-mode-hook #'mixed-pitch-mode)
;; Fonts:1 ends here

;; [[file:config.org::*Fonts][Fonts:2]]
(add-hook 'org-mode-hook
          (lambda () (setq line-spacing 0.2)))
;; Fonts:2 ends here

;; [[file:config.org::*Org-Modern][Org-Modern:2]]
(use-package! org-modern
  :defer t
  :config
  (setq org-modern-label-border 0.1
        org-modern-star 'replace))
;; Org-Modern:2 ends here

;; [[file:config.org::*Org-Modern][Org-Modern:3]]
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
  '(("src" . "")
    ("src" . "󰌠")
    ("result" . " result ")
    ("quote" . " quote")
    ("verse" . " verse")
    ("example" . "󰇥 example")
    ("comment" . " comment")
    ("center" . " center"))

  org-modern-keyword
  '(("results" . " Results")
    ("logbook" . " Logbook")
    ("clock" . " Clock")
    ("name" . " Name")
    ("title" . "󰗴")
    ("property" . "󱌣"))

  org-modern-star 'nil) ;; Use old org-modern star icons

(custom-set-faces!
  '(org-modern-label :height 1.1))

(global-org-modern-mode)
;; Org-Modern:3 ends here

;; [[file:config.org::*Org-Modern][Org-Modern:4]]
(setq prettify-symbols-alist
      '(("CLOCK:" . ?)
        (":LOGBOOK:" . ?)
        (":END:" . ?-)))
;; Org-Modern:4 ends here

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

;; [[file:config.org::*Hide ~:PROPERTY:~ Drawers][Hide ~:PROPERTY:~ Drawers:2]]
;; (use-package! org-tidy
;;   :defer t
;;   :hook (org-mode . org-tidy-mode)
;;   :config (map! :map org-mode-map
;;                 :localleader
;;                 :desc "Toggle org-tidy" "z" #'org-tidy-mode))
;; Hide ~:PROPERTY:~ Drawers:2 ends here

;; [[file:config.org::*Export][Export:2]]
(use-package! ox-hugo
  :after org
  :defer t
  :config
  (setq org-hugo-default-language "en"
        org-hugo-auto-export-mode t
        org-hugo-base-dir (concat org-directory "blog/")))
;; Export:2 ends here

;; [[file:config.org::*HTTP requests via babel][HTTP requests via babel:2]]
(use-package! ob-http
  :defer t
  :commands org-babel-execute:http)
;; HTTP requests via babel:2 ends here

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

;; [[file:config.org::*Auto-Tangling][Auto-Tangling:1]]
(defun org-babel-tangle-config ()
  (when (string-equal (file-name-nondirectory (buffer-file-name))
                      "config.org")
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle-config)))
;; Auto-Tangling:1 ends here

;; [[file:config.org::*Export headings up to five levels deep][Export headings up to five levels deep:1]]
(setq org-export-headline-levels 5)
;; Export headings up to five levels deep:1 ends here

;; [[file:config.org::*Latex fragments][Latex fragments:1]]
(setq org-highlight-latex-and-related '(native script entities))
;; Latex fragments:1 ends here

;; [[file:config.org::*Mermaid Diagrams][Mermaid Diagrams:2]]
(setq ob-mermaid-cli-path (shell-command-to-string "printf %s \"$(readlink -f $(which mmdc))\""))
;; Mermaid Diagrams:2 ends here

;; [[file:config.org::*Mermaid Diagrams][Mermaid Diagrams:3]]
(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)))
;; Mermaid Diagrams:3 ends here

;; [[file:config.org::*Use the same directory as org][Use the same directory as org:1]]
(setq org-roam-directory (concat org-directory "roam"))
;; Use the same directory as org:1 ends here

;; [[file:config.org::*make good Capture Templates & Shortcuts][make good Capture Templates & Shortcuts:1]]
(defun my/org-roam-capture-inbox()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-todo()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("t" "todo" plain "* TODO %?"
                                  :if-new (file+head "todo.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-journal()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("j" "journal" entry "* %T %?"
                                  :if-new (file+datetree "journal.org" day)))))

(map! :leader
      :desc "inbox" "n r c i" #'my/org-roam-capture-inbox)

(map! :leader
      :desc "todo" "n r c t" #'my/org-roam-capture-todo)

(map! :leader
      :desc "journal" "n r c j" #'my/org-roam-capture-journal)
;; make good Capture Templates & Shortcuts:1 ends here

;; [[file:config.org::*Add Org-Roam UI][Add Org-Roam UI:2]]
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
;; Add Org-Roam UI:2 ends here

;; [[file:config.org::*Automatic Blog Creation][Automatic Blog Creation:1]]
(after! org
  (setq time-stamp-active t
        time-stamp-start "#\\+hugo_lastmod:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%Y-%m-%d\]")
  (add-hook 'before-save-hook 'time-stamp))
;; Automatic Blog Creation:1 ends here

;; [[file:config.org::*Turn Logseq Nodes into Org-Roam Nodes][Turn Logseq Nodes into Org-Roam Nodes:1]]
;; (load (expand-file-name "org-roam-logseq.el" doom-user-dir))
;; Turn Logseq Nodes into Org-Roam Nodes:1 ends here

;; [[file:config.org::*Prevent org-block face for latex fragments, since they look weird][Prevent org-block face for latex fragments, since they look weird:1]]
(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
;; Prevent org-block face for latex fragments, since they look weird:1 ends here

;; [[file:config.org::*Nix-Doom-Emacs messes with dashboard][Nix-Doom-Emacs messes with dashboard:1]]
(add-hook! 'emacs-startup-hook #'doom-init-ui-h)
;; Nix-Doom-Emacs messes with dashboard:1 ends here

;; [[file:config.org::*Faster insertion of org structures (i.e. source blocks)][Faster insertion of org structures (i.e. source blocks):1]]
(use-package! org-tempo)
;; Faster insertion of org structures (i.e. source blocks):1 ends here

;; [[file:config.org::*Automatic list item insertion][Automatic list item insertion:2]]
(use-package! org-autolist
  :config
  (add-hook 'org-mode-hook #'org-autolist-mode))
;; Automatic list item insertion:2 ends here

;; [[file:config.org::*Add / change capture templates][Add / change capture templates:1]]
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
;; Add / change capture templates:1 ends here

;; [[file:config.org::*Improve org-capture dialog][Improve org-capture dialog:1]]
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
                `(("q" ,(concat (nerd-icons-octicon "nf-oct-stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
    (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier))
;; Improve org-capture dialog:1 ends here

;; [[file:config.org::*Improve org-capture dialog][Improve org-capture dialog:2]]
(setf (alist-get 'height +org-capture-frame-parameters) 15)
      ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))
;; Improve org-capture dialog:2 ends here

;; [[file:config.org::*Khal / Khalel][Khal / Khalel:2]]
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
;; Khal / Khalel:2 ends here

;; [[file:config.org::*Querying & Combined Views][Querying & Combined Views:3]]
(use-package! org-ql)

(use-package! org-roam-ql
  :after (org-roam)
  :bind ((:map org-roam-mode-map
          ;; Have org-roam-ql's transient available in org-roam-mode buffers
          ("v" . org-roam-ql-buffer-dispatch)
          :map minibuffer-mode-map
          ;; Be able to add titles in queries while in minibuffer.
          ;; This is similar to `org-roam-node-insert', but adds
          ;; only title as a string.
          ("C-c n i" . org-roam-ql-insert-node-title))))
;; Querying & Combined Views:3 ends here

;; [[file:config.org::*Database Refresh Strategy][Database Refresh Strategy:1]]
(defun my/refresh-org-databases ()
  "Refresh both org-roam and org-ql databases to pick up new files."
  (interactive)
  (my/rebuild-org-agenda-files)  ; Rebuild agenda files to include new roam files
  (when (featurep 'org-roam)
    (org-roam-db-sync))
  (when (featurep 'org-ql)
    (org-ql-clear-cache))
  (message "Refreshed org databases and agenda files"))

;; Auto-refresh after saving any org file
(add-hook 'after-save-hook
          (lambda ()
            (when (derived-mode-p 'org-mode)
              (run-with-idle-timer 1 nil #'my/refresh-org-databases))))

;; Keybinding for manual refresh
(map! :leader
      :desc "Refresh org databases" "n r r" #'my/refresh-org-databases
      :desc "Rebuild agenda files" "n r a" #'my/rebuild-org-agenda-files)
;; Database Refresh Strategy:1 ends here

;; [[file:config.org::*Unified Task Views][Unified Task Views:1]]
(defun my/org-ql-task-dashboard ()
  "Open a comprehensive task dashboard using org-ql."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo "TODO" "NEXT" "DOING" "PROG" "WAIT")
          (not (todo "DONE" "CANCELLED" "CANCELED")))
    :title "Task Dashboard - Active Tasks Only"
    :sort '(deadline scheduled priority todo)
    :super-groups '((:name "Overdue"
                     :deadline past
                     :order 1)
                    (:name "Due Today"
                     :deadline today
                     :order 2)
                    (:name "Due This Week"
                     :deadline (between today +7)
                     :order 3)
                    (:name "Scheduled Today"
                     :scheduled today
                     :order 4)
                    (:name "High Priority"
                     :priority "A"
                     :order 5)
                    (:name "In Progress"
                     :todo ("DOING" "NEXT" "PROG")
                     :order 6)
                    (:name "Waiting"
                     :todo "WAIT"
                     :order 7)
                    (:name "Other TODOs"
                     :todo "TODO"
                     :order 8))))

(defun my/org-ql-stale-tasks ()
  "Find potentially stale tasks - active TODOs without recent activity."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo "TODO" "NEXT" "DOING" "PROG" "WAIT")
          (not (todo "DONE" "CANCELLED" "CANCELED"))
          (not (ts :from -30)))
    :title "Potentially Stale Active Tasks (>30 days old)"
    :sort '(todo)
    :super-groups '((:name "Very Old (>90 days)"
                     :pred (lambda (item)
                             (when-let ((ts (org-element-property :raw-value 
                                            (org-element-property :timestamp 
                                             (org-element-at-point)))))
                               (time-less-p (date-to-time ts)
                                          (time-subtract (current-time) 
                                                       (days-to-time 90)))))
                     :order 1)
                    (:name "Old (30-90 days)"
                     :order 2))))

(defun my/org-ql-completed-tasks-review ()
  "Review completed tasks for potential cleanup."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(todo "DONE" "CANCELLED" "CANCELED")
    :title "Completed Tasks Review"
    :sort '(todo)
    :super-groups '((:name "Done Tasks"
                     :todo "DONE"
                     :order 1)
                    (:name "Cancelled Tasks"
                     :todo ("CANCELLED" "CANCELED")
                     :order 2))))

(defun my/org-ql-week-review ()
  "Review tasks for the coming week."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo "TODO" "NEXT" "DOING" "PROG" "WAIT")
          (not (todo "DONE" "CANCELLED" "CANCELED"))
          (or (deadline (between today +7))
              (scheduled (between today +7))
              (priority "A")))
    :title "Week Review - Focus Areas (Active Tasks Only)"
    :sort '(deadline scheduled priority)
    :super-groups '((:name "Critical This Week"
                     :and (:deadline (between today +7)
                           :priority "A")
                     :order 1)
                    (:name "Due This Week"
                     :deadline (between today +7)
                     :order 2)
                    (:name "Scheduled This Week"
                     :scheduled (between today +7)
                     :order 3)
                    (:name "High Priority (No Deadline)"
                     :priority "A"
                     :order 4))))
;; Unified Task Views:1 ends here

;; [[file:config.org::*Context-Aware Task Queries][Context-Aware Task Queries:1]]
(defun my/org-ql-tasks-by-context ()
  "Group tasks by their containing file/context."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo "TODO" "NEXT" "DOING" "PROG" "WAIT")
          (not (todo "DONE" "CANCELLED" "CANCELED")))
    :title "Active Tasks by Context"
    :sort '(todo)
    :super-groups '((:auto-parent t))))

(defun my/org-roam-ql-project-tasks ()
  "Find all tasks in org-roam notes tagged with projects."
  (interactive)
  (if (and (featurep 'org-roam) (bound-and-true-p org-roam-directory))
      (org-ql-search (list org-roam-directory)
        '(and (todo "TODO" "NEXT" "DOING" "PROG" "WAIT")
              (not (todo "DONE" "CANCELLED" "CANCELED"))
              (tags "project"))
        :title "Active Project Tasks (from Roam)")
    (message "org-roam not available or not configured")))

(defun my/org-ql-task-clusters ()
  "Find where tasks are clustering to identify overcommitment."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo "TODO" "NEXT" "DOING" "PROG" "WAIT")
          (not (todo "DONE" "CANCELLED" "CANCELED"))
          (deadline (between today +14)))
    :title "Active Task Clustering Analysis (Next 2 Weeks)"
    :sort '(deadline scheduled)
    :super-groups '((:auto-outline-path t))))
;; Context-Aware Task Queries:1 ends here

;; [[file:config.org::*Quick Task Operations][Quick Task Operations:1]]
(defun my/org-ql-toggle-task-done ()
  "Quick toggle task state in org-ql buffers."
  (interactive)
  (org-todo 'done))

(defun my/org-ql-reschedule-task ()
  "Reschedule task in org-ql buffer."
  (interactive)
  (org-schedule nil))

(defun my/org-ql-set-task-priority ()
  "Set task priority in org-ql buffer."
  (interactive)
  (org-priority))

;; Keybindings for org-ql buffers
(map! :map org-ql-view-map
      :n "t" #'my/org-ql-toggle-task-done
      :n "s" #'my/org-ql-reschedule-task
      :n "p" #'my/org-ql-set-task-priority
      :n "gr" #'org-ql-view-refresh)
;; Quick Task Operations:1 ends here

;; [[file:config.org::*Integration with Existing Workflow][Integration with Existing Workflow:1]]
;; Add keybindings to your existing leader key setup
(map! :leader
      :desc "Task dashboard" "n q d" #'my/org-ql-task-dashboard
      :desc "Stale tasks" "n q s" #'my/org-ql-stale-tasks
      :desc "Week review" "n q w" #'my/org-ql-week-review
      :desc "Tasks by context" "n q c" #'my/org-ql-tasks-by-context
      :desc "Task clusters" "n q l" #'my/org-ql-task-clusters
      :desc "Project tasks" "n q p" #'my/org-roam-ql-project-tasks
      :desc "Review completed tasks" "n q r" #'my/org-ql-completed-tasks-review)

;; Replace default agenda with task dashboard
(map! :leader
      :desc "Task dashboard" "o A" #'my/org-ql-task-dashboard)
;; Integration with Existing Workflow:1 ends here

;; [[file:config.org::*Performance Optimizations][Performance Optimizations:1]]
;; Optimize org-ql for large numbers of files
(setq org-ql-cache-persist t
      org-ql-cache-size 1000)

;; Only scan necessary directories for org-roam
(setq org-roam-file-extensions '("org")
      org-roam-scan-timeout 30)

;; Async refreshing to avoid blocking
(defun my/async-refresh-org-databases ()
  "Asynchronously refresh org databases."
  (run-with-timer 0.5 nil #'my/refresh-org-databases))

;; Hook into org-roam capture to refresh database
(add-hook 'org-roam-capture-new-node-hook #'my/async-refresh-org-databases)
;; Performance Optimizations:1 ends here

;; [[file:config.org::*Projectile Project Search Path][Projectile Project Search Path:1]]
(setq projectile-project-search-path '(("~/Code/" . 1)))
;; Projectile Project Search Path:1 ends here

;; [[file:config.org::*Disable Automatic Workspace Creation][Disable Automatic Workspace Creation:1]]
(setq +workspaces-on-switch-project-behavior nil)
;; Disable Automatic Workspace Creation:1 ends here

;; [[file:config.org::*Testing][Testing:2]]
(use-package! jest
  :after (typescript-mode js-mode typescript-tsx-mode)
  :config
  (add-hook 'typescript-mode-hook #'jest-minor-mode))
;; Testing:2 ends here

;; [[file:config.org::*Testing][Testing:3]]
(setq find-sibling-rules
      '(("src/\\(.*/\\)?\\([^/]+\\)\\.\\(ts\\|vue\\)\\'"
         "test/.*\\2.test.ts")
        ("test/\\(.*/\\)?\\([^/]+\\)\\.test.ts\\'"
         "src/.*\\2.\\(ts\\|vue\\)")))
;; Testing:3 ends here

;; [[file:config.org::*Eslint][Eslint:1]]
;; (advice-add 'lsp
;;             :before (lambda (&rest _args)
;;                       (setf (lsp-session-server-id->folders (lsp-session)) (ht))))
;; Eslint:1 ends here

;; [[file:config.org::*Svelte][Svelte:2]]
(use-package! svelte-mode
  :defer t
  :mode "\\.svelte\\'")
;; Svelte:2 ends here

;; [[file:config.org::*Vue][Vue:1]]
(with-eval-after-load 'web-mode
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))
;; Vue:1 ends here

;; [[file:config.org::*Astro][Astro:2]]
(setq treesit-language-source-alist
      '((astro "https://github.com/virchau13/tree-sitter-astro")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
;; Astro:2 ends here

;; [[file:config.org::*Astro][Astro:3]]
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '(("\\.astro\\'" . astro-mode))
              auto-mode-alist))
;; Astro:3 ends here

;; [[file:config.org::*Astro][Astro:4]]
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(astro-mode . "astro"))
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls"))
                    :activation-fn (lsp-activate-on "astro")
                    :server-id 'astro-ls)))
;; Astro:4 ends here

;; [[file:config.org::*Tailwind][Tailwind:2]]
(use-package! lsp-tailwindcss
  :defer t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode typescript-mode typescript-tsx-mode rust-mode rustic-mode))
  )
;; Tailwind:2 ends here

;; [[file:config.org::*Code formatting][Code formatting:1]]
(setq typescript-indent-level 2)
(setq js-indent-level 2)
;; Code formatting:1 ends here

;; [[file:config.org::*Nix][Nix:1]]
(use-package! nix-mode
  :mode "\\.nix\\'")
;; Nix:1 ends here

;; [[file:config.org::*Run pytest in virtualenv][Run pytest in virtualenv:1]]
(add-hook! python-mode
  (advice-add 'python-pytest-file :before
              (lambda (&rest args)
                (setq-local python-pytest-executable
                            (executable-find "pytest")))))
;; Run pytest in virtualenv:1 ends here

;; [[file:config.org::*Terraform][Terraform:1]]
(setq lsp-terraform-ls-enable-show-reference t)
(setq lsp-semantic-tokens-enable t)
(setq lsp-semantic-tokens-honor-refresh-requests t)
;; Terraform:1 ends here

;; [[file:config.org::*Haskell][Haskell:1]]
(setq flymake-allowed-file-name-masks nil)
;; Haskell:1 ends here

;; [[file:config.org::*Jsonnet][Jsonnet:2]]
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
;; Jsonnet:2 ends here

;; [[file:config.org::*Rust][Rust:1]]
(setq lsp-rust-features "all")
;; Rust:1 ends here

;; [[file:config.org::*MDX][MDX:1]]
(setq auto-mode-alist
      (append '(("\\.mdx\\'" . markdown-mode))
              auto-mode-alist))
;; MDX:1 ends here

;; [[file:config.org::*Gleam][Gleam:2]]
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
;; Gleam:2 ends here

;; [[file:config.org::*Lean][Lean:2]]
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(lean4-mode . "lean"))
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("lean --server"))
                    :activation-fn (lsp-activate-on "lean")
                    :server-id 'lean-ls)))
;; Lean:2 ends here

;; [[file:config.org::*Lean][Lean:4]]
(use-package! ob-lean4
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(lean4 . t)))
;; Lean:4 ends here

;; [[file:config.org::*Install and Package Set-Up][Install and Package Set-Up:2]]
;; Ob-sagemath supports only evaluating with a session.
(setq org-babel-default-header-args:sage '((:session . t)
                                           (:results . "output")))

;; C-c c for asynchronous evaluating (only for SageMath code blocks).
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; Do not evaluate code blocks when exporting.
(setq org-export-babel-evaluate nil)

;; Show images when opening a file.
(setq org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;; Install and Package Set-Up:2 ends here

;; [[file:config.org::*Some Org-Babel Settings][Some Org-Babel Settings:1]]
;; (setq org-babel-default-header-args:sage
;;       '((:session . "none")
;;         (:cache . "no")
;;         (:noeval . "no")
;;         (:hlines . "no")
;;         (:tangle . "no")
;;         (:comments . "link")))
;; Some Org-Babel Settings:1 ends here

;; [[file:config.org::*Handling][Handling:1]]
(setq  corfu-auto-delay 0.1
       corfu-auto-prefix 2
       corfu-left-margin-width 2
       corfu-right-margin-width 2
       corfu-bar-width 1)
;; Handling:1 ends here

;; [[file:config.org::*Handling][Handling:2]]
(setq global-corfu-minibuffer nil)
;; Handling:2 ends here

;; [[file:config.org::*UI][UI:1]]
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
;; UI:1 ends here

;; [[file:config.org::*UI][UI:2]]
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
;; UI:2 ends here

;; [[file:config.org::*UI][UI:3]]
(setq corfu-popupinfo-delay '(0.1 . 0.05)
      corfu-popupinfo-hide nil
      corfu-popupinfo-max-width 160
      corfu-popupinfo-min-width 160
      corfu-popupinfo-max-height 30
      corfu-popupinfo--buffer-parameters '((truncate-lines . nil)
                                           (left-margin-width . 2)
                                           (right-margin-width . 2)
                                           (word-wrap . t)))
;; UI:3 ends here

;; [[file:config.org::*Copilot][Copilot:2]]
(use-package! copilot
  :defer t
  :config
  (add-hook 'prog-mode-hook #'copilot-mode)
  (add-hook 'copilot-mode-hook (lambda ()
                                  (setq-local copilot--indent-warning-printed-p t)))
  :bind (:map copilot-completion-map
              ("C-<space>" . 'copilot-accept-completion)
              ("C-SPC" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
;; Copilot:2 ends here

;; [[file:config.org::*Copilot][Copilot:3]]
(map! :leader
      :desc "Show Copilot Completion" "i g s" #'copilot-complete
      :desc "Insert Copilot Completion" "i g c" #'copilot-accept-completion
      :desc "Toggle Copilot" "t p" #'copilot-mode)
;; Copilot:3 ends here

;; [[file:config.org::*Aider][Aider:2]]
(use-package! aidermacs
  :defer t
  :hook (aidermacs-minor-mode . (lambda () (setenv "OPENAI_API_KEY" (password-store-get "bitwarden/openai-gpt-key"))))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "4o"))
;; Aider:2 ends here

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
      :desc "Debug" "d d" #'dap-debug
      :desc "Next" "d n" #'dap-next
      :desc "Step in" "d i" #'dap-step-in
      :desc "Step out" "d o" #'dap-step-out
      :desc "Continue" "d c" #'dap-continue
      :desc "Restart" "d r" #'dap-restart-frame
      :desc "Disconnect" "d D" #'dap-disconnect
      :desc "Evaluate" "d e" #'dap-eval
      :desc "Add Expression" "d a" #'dap-ui-expressions-add)
       ;; Future breakpoints keybindings:
       ;; :desc "Toggle breakpoint" "d b t" #'dap-breakpoint-toggle
       ;; :desc "Add breakpoint" "d b a" #'dap-breakpoint-add
       ;; :desc "Delete breakpoint" "d b d" #'dap-breakpoint-delete
       ;; :desc "Set condition" "d b c" #'dap-breakpoint-condition
       ;; :desc "Set log message" "d b m" #'dap-breakpoint-log-message
       ;;  :desc "Set hit condition" "h" #'dap-breakpoint-hit-condition)))
;; Keybindings:1 ends here

;; [[file:config.org::*Syntax Checking][Syntax Checking:1]]
(setq flycheck-syntax-automatically '(save-mode-enable))
;; Syntax Checking:1 ends here

;; [[file:config.org::*Performance][Performance:1]]
(setq lsp-use-plists 't)
;; Performance:1 ends here

;; [[file:config.org::*Handling][Handling:1]]
(setq lsp-completion-provider :capf)
;; Handling:1 ends here

;; [[file:config.org::*Handling][Handling:2]]
(setq lsp-completion-show-detail t)
;; Handling:2 ends here

;; [[file:config.org::*Handling][Handling:3]]
(setq lsp-completion-show-kind t)
;; Handling:3 ends here

;; [[file:config.org::*Handling][Handling:4]]
(setq lsp-auto-guess-root t)
(add-hook 'prog-mode-hook #'lsp-deferred)
;; Handling:4 ends here

;; [[file:config.org::*UI][UI:1]]
(map! :leader
      :desc "Glance at documentation" "c g" #'lsp-ui-doc-glance)
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

;; [[file:config.org::*UI][UI:6]]
(setq lsp-ui-doc-max-height 400
      lsp-ui-doc-max-width 250)
;; UI:6 ends here

;; [[file:config.org::*Disable Evil-Mode in timemachine mode][Disable Evil-Mode in timemachine mode:1]]
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))
;; Disable Evil-Mode in timemachine mode:1 ends here

;; [[file:config.org::*Gitlab Integration][Gitlab Integration:2]]
(use-package! lab
  :config
  (setq lab-host "https://gitlab.com")
  (setq lab-token (password-store-get "bitwarden/gitlab-token")))
;; Gitlab Integration:2 ends here

;; [[file:config.org::*Gitlab Integration][Gitlab Integration:3]]
(map! :leader
      :desc "List Pipelines" "g l p" #'lab-list-project-pipelines
      :desc "List Merge Requests" "g l m" #'lab-list-project-merge-requests
      :desc "List all owned projects" "g l o" #'lab-list-all-owned-projects)
;; Gitlab Integration:3 ends here

;; [[file:config.org::*Configuration][Configuration:1]]
(map! :leader
      :desc "Open devdocs" "D o" #'devdocs-peruse
      :desc "Search devdocs" "D l" #'devdocs-lookup
      :desc "Install devdocs set" "D i" #'devdocs-install)
;; Configuration:1 ends here

;; [[file:config.org::*GPTel][GPTel:2]]
(use-package! gptel
  :config
  (setq! gptel-api-key (lambda () (password-store-get "bitwarden/openai-gpt-key")))
  (setq! gptel-model "gpt-4"))

(map! :leader
      :desc "Open GPTel" "o g" #'gptel)
;; GPTel:2 ends here

;; [[file:config.org::*Task Runners][Task Runners:2]]
(use-package! justl
  :config

  (map! :leader
        :desc "Make" "c m" #'justl)
  (map! :n "e" 'justl-exec-recipe))
;; Task Runners:2 ends here

;; [[file:config.org::*Mermaid][Mermaid:2]]
(setq org-babel-default-header-args:mermaid
      '((:background-color. "transparent")
        (:theme . "dark")
        (:results . "file")
        (:file . (lambda () (make-temp-file "mermaid" nil ".svg")))))
;; Mermaid:2 ends here

;; [[file:config.org::*Markdown / Org Preview][Markdown / Org Preview:1]]
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc -f markdown -t html5"))
;; Markdown / Org Preview:1 ends here

;; [[file:config.org::*Markdown / Org Preview][Markdown / Org Preview:3]]
(use-package simple-httpd
  :config
  (setq httpd-port 7070))

(use-package impatient-mode
  :commands impatient-mode)
;; Markdown / Org Preview:3 ends here

;; [[file:config.org::*Markdown / Org Preview][Markdown / Org Preview:5]]
(defun markdown-html-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
        (set-buffer buffer)
        (set-buffer (markdown tmp))
        (format "<!DOCTYPE html><html><title>Markdown Preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/><body><article class=\"markdown-body\">%s</article></body></html>" (buffer-string))))
    (current-buffer)))
;; Markdown / Org Preview:5 ends here

;; [[file:config.org::*Markdown / Org Preview][Markdown / Org Preview:6]]
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
      :desc "Preview" "m p" #'markdown-html-preview
      :desc "Stop Preview" "m s" #'markdown-html-preview-stop)
;; Markdown / Org Preview:6 ends here

;; [[file:config.org::*Catppuccin][Catppuccin:2]]
(setq catppuccin-flavor 'frappe)
;; Catppuccin:2 ends here

;; [[file:config.org::*Current Theme][Current Theme:1]]
(setq doom-theme 'doom-nord-aurora)
;; Current Theme:1 ends here

;; [[file:config.org::*Ewal][Ewal:2]]
(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p nil
              ewal-built-in-palette "sexy-material"))
;; Ewal:2 ends here

;; [[file:config.org::*Doom Modeline][Doom Modeline:1]]
(setq doom-modeline-vcs-max-length 50)
;; Doom Modeline:1 ends here

;; [[file:config.org::*Doom Modeline][Doom Modeline:2]]
(setq doom-modeline-hud t)
;; Doom Modeline:2 ends here

;; [[file:config.org::*Doom Modeline][Doom Modeline:3]]
(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-modal nil)
  (setq doom-modeline-column-format "")
  (setq size-indication-mode nil)
  (setq doom-modeline-bar-width 0))
;; Doom Modeline:3 ends here

;; [[file:config.org::*Doom Modeline][Doom Modeline:4]]
(after! doom-modeline
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
  (line-number-mode -1))
;; Doom Modeline:4 ends here

;; [[file:config.org::*Doom Modeline][Doom Modeline:5]]
(add-hook 'treemacs-mode-hook (lambda () (hide-mode-line-mode)))
;; Doom Modeline:5 ends here

;; [[file:config.org::*Incorporate Nano Modeline][Incorporate Nano Modeline:2]]
;; (use-package! nano-modeline
;;   :config
;;   (nano-modeline-text-mode t))
;; Incorporate Nano Modeline:2 ends here

;; [[file:config.org::*General Padding][General Padding:2]]
(use-package! spacious-padding
  :config
  (setq spacious-padding-width '(
    :internal-border-width 20
    :header-line-width 8
    :mode-line-width 12
    :tab-width 16
    :right-divider-width 60
    :scroll-bar-width 12)))
;; General Padding:2 ends here

;; [[file:config.org::*General Padding][General Padding:3]]
(setq spacious-padding-subtle-mode-line t)
;; General Padding:3 ends here

;; [[file:config.org::*General Padding][General Padding:4]]
(spacious-padding-mode 1)
;; General Padding:4 ends here

;; [[file:config.org::*Better Error Display][Better Error Display:1]]
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
;; Better Error Display:1 ends here

;; [[file:config.org::*Treemacs Modeline][Treemacs Modeline:1]]
(add-hook 'treemacs-mode-hook #'hide-mode-line-mode)
;; Treemacs Modeline:1 ends here

;; [[file:config.org::*Vertico][Vertico:1]]
(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    " > " "   ")))
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

;; [[file:config.org::*Vertico][Vertico:4]]
(setq vertico-posframe-parameters
      '((left-fringe . 16)
        (right-fringe . 8)
        (border-width . 16)))
;; Vertico:4 ends here

;; [[file:config.org::*Performance][Performance:1]]
(setq read-process-output-max (* 4 1024 1024)) ;; 4mb
;; Performance:1 ends here

;; [[file:config.org::*Performance][Performance:2]]
(fset #'jsonrpc--log-event #'ignore)
;; Performance:2 ends here

;; [[file:config.org::*Discord Presence][Discord Presence:2]]
(use-package! elcord
  :config
  (setq elcord-editor-icon "emacs_icon"))
;; Discord Presence:2 ends here
