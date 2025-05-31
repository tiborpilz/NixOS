
# Table of Contents

1.  [Base Settings](#org58a9396)
    1.  [Personal data](#org6452685)
    2.  [Font settings](#orgc02debf)
    3.  [Line numbers](#orgee9fedc)
    4.  [Tab width](#org0911891)
    5.  [Splash Screen](#orgcbab1e4)
        1.  [Splash Image](#org68c3bc2)
    6.  [Title](#orgcafab61)
    7.  [Password Store location](#org01cb474)
2.  [Utils](#org7edcf3a)
    1.  [Add-Hooks](#orgfa94113)
3.  [Org Mode](#org93c8d16)
    1.  [Base Settings](#orge9c34a2)
    2.  [Visual improvements](#orga44dcbd)
        1.  [Headlines](#org0034510)
        2.  [Fonts](#org79cec3f)
        3.  [Org-Modern](#org0800c2f)
        4.  [Miscellanious improvements](#org5923b4b)
            1.  [Show passed deadlines as error](#orgc9f483f)
            2.  [Show quote blocks in italic](#orge382066)
            3.  [Defer font-lock](#org1dd5080)
        5.  [Hide `:PROPERTY:` Drawers](#org0641cd4)
    3.  [Babel](#orgbfdbfaa)
        1.  [HTTP requests via babel](#org7dc5682)
        2.  [Babel header args](#org1016297)
        3.  [Auto-Tangling](#orgee8c9e3)
        4.  [Typescript](#orgb4f705b)
        5.  [Export headings up to five levels deep](#org3874599)
        6.  [Latex fragments](#org4ed5991)
        7.  [Mermaid Diagrams](#orgb9b3c60)
            1.  [All mermaid diagrams have a white background](#org698afa8)
    4.  [Roam](#org39cecb7)
        1.  [Use the same directory as org](#orgc94fec5)
        2.  [Capture Templates & Shortcuts](#org47c42e8)
        3.  [Add Org-Roam UI](#orgbedb714)
        4.  [Capture to Org Roam Dailies](#orgfee94d5)
        5.  [Logseq compatibility](#org157f3c4)
            1.  [Turn Logseq Nodes into Org-Roam Nodes](#org1a49d79)
    5.  [Org-Node](#org0dabd75)
    6.  [Fixes and miscellanious improvements](#orgf9edde5)
        1.  [Prevent org-block face for latex fragments, since they look weird](#org4fa885f)
        2.  [Nix-Doom-Emacs messes with dashboard](#org72c2112)
        3.  [Faster insertion of org structures (i.e. source blocks)](#org29214aa)
        4.  [Automatic list item insertion](#org78a5b5b)
    7.  [Capture](#org3015141)
        1.  [Add / change capture templates](#orgb27c65f)
        2.  [Improve org-capture dialog](#orge120834)
    8.  [Agenda and Time Management](#org16f3c87)
        1.  [Khal / Khalel](#org56a6935)
4.  [Workspaces & Projects](#org598e01e)
    1.  [Projectile Project Search Path](#org6050a7f)
    2.  [Disable Automatic Workspace Creation](#org992635c)
5.  [Development](#org558b491)
    1.  [Language-Specific Settings](#org0e6c379)
        1.  [Web Dev (JS/TS/CSS)](#org47cc353)
            1.  [Testing](#org9a6d8f4)
                1.  [This seems to only work for direct descendants of `src` and `test`.](#org2c75935)
            2.  [Eslint](#org236914c)
            3.  [Svelte](#org493e910)
            4.  [Vue](#org7b86fe9)
            5.  [Astro](#org6e72086)
            6.  [Tailwind](#orgc0ff9ca)
            7.  [Code formatting](#org3178a42)
            8.  [Typescript REPL](#orgb5449ec)
        2.  [Nix](#org04886ae)
            1.  [nix.el](#org7b6a32a)
            2.  [nix-flake.el](#org36c1497)
            3.  [nix-repls.el](#org750ff3b)
            4.  [nix-store.el](#orgc971e78)
            5.  [nix-prettify-mode.el](#org4b329a1)
        3.  [Python](#org3ab36f5)
            1.  [Poetry](#orgaf975b0)
            2.  [Run pytest in virtualenv](#org69e2fb8)
        4.  [Terraform](#org9223840)
        5.  [Haskell](#orge5e0e92)
        6.  [Jsonnet](#orga6c60c0)
        7.  [Rust](#org3d5b582)
        8.  [MDX](#org41b9015)
        9.  [Gleam](#org7cbc481)
    2.  [Tools](#org8b61766)
        1.  [Code Completion](#org4203ed3)
            1.  [Handling](#orge8d072a)
            2.  [UI](#org7410cbe)
        2.  [Copilot](#org09e36c0)
        3.  [Aider](#org314f851)
        4.  [Debugging](#orge92c547)
            1.  [Language-Specific Debugger settings](#orgfd092b2)
                1.  [Python](#org480828c)
            2.  [Fixes](#orgeca90a4)
                1.  [Fix Doom &ldquo;+debugger/start&rdquo;](#org05b7e19)
                2.  [Missing fringes in dap-mode](#orge56b36e)
            3.  [Keybindings](#orgc1fc579)
        5.  [Syntax Checking](#orgd7eee66)
        6.  [LSP](#orgfd44372)
            1.  [Performance](#org622d392)
            2.  [Handling](#orgb0be217)
            3.  [UI](#org52dd01e)
            4.  [Emacs-LSP-Booster compatibility](#org1dd4b9f)
        7.  [Git](#org888b94e)
            1.  [Disable Evil-Mode in timemachine mode](#org73b9a97)
            2.  [Gitlab Integration](#orgcd6f450)
            3.  [Delta as Git Diff](#org20b24f3)
        8.  [Documentation](#org68a014e)
            1.  [Devdocs](#org1436470)
                1.  [Install](#org0611e91)
                2.  [Configuration](#org4013543)
        9.  [AI Assistance](#orgd55c5a3)
            1.  [GPTel](#org2d2f5f8)
        10. [Task Runners](#org4b47beb)
        11. [Diagrams](#org9f12192)
            1.  [Mermaid](#orgfe0ad17)
        12. [Markdown / Org Preview](#org0c3ece4)
    3.  [Theming](#org09b66ce)
        1.  [Doom Themes](#org8abed05)
        2.  [Catppuccin](#orgdb72f01)
        3.  [Current Theme](#orgaa7ba0c)
        4.  [Misc Themes](#org59ddde2)
            1.  [Grayscale](#org684d367)
            2.  [Tao Themes](#org1fb2034)
            3.  [Ewal](#org6a1b6f0)
        5.  [Theme Magic](#org3614e0b)
        6.  [Autothemer](#orgd50d716)
        7.  [Base 16 Themes](#orgc250bb5)
            1.  [TODO: Implement more complex color schemes based on base16 colors](#org40501f6)
    4.  [UI](#org8300ccf)
        1.  [Doom Modeline](#org3eb5036)
        2.  [General Padding](#org106ec87)
        3.  [Better Error Display](#org4f43e73)
        4.  [Treemacs Modeline](#orgc5c46f5)
        5.  [Solaire-Mode](#orgf5e7e68)
    5.  [Vertico](#org3f28de0)
    6.  [Kubernetes](#org181b384)
6.  [Performance](#org40eb296)
7.  [Unsorted Packages](#org89e5348)
    1.  [pcre2el](#org38b2ab7)
    2.  [Discord Presence](#org0491a1c)



<a id="org58a9396"></a>

# Base Settings


<a id="org6452685"></a>

## Personal data

Set Name and mail adress

    (setq user-full-name "Tibor Pilz"
          user-mail-address "tibor@pilz.berlin")


<a id="orgc02debf"></a>

## Font settings

    (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'normal)
          doom-big-font (font-spec :family "FiraCode Nerd Font" :size 28 :weight 'light)
          doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'light)
          doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 16 :weight 'light))


<a id="orgee9fedc"></a>

## Line numbers

Since I&rsquo;m using Evil/vim-keybindings, I prefer relative line numbers. `'visual` is
a different way of relative line numbers, where folded lines are not counted.

    (setq display-line-numbers-type 'visual)


<a id="org0911891"></a>

## Tab width

    (setq tab-width 2)


<a id="orgcbab1e4"></a>

## Splash Screen


<a id="org68c3bc2"></a>

### Splash Image

I&rsquo;ve created a custom SVG with transparency and shadow effects that should go
well with a variety of color schemes.

    (setq fancy-splash-image (concat doom-private-dir "splash-logos/emacs-logo-cutout.svg"))


<a id="orgcafab61"></a>

## Title

I&rsquo;m setting the title to be just &ldquo;Emacs&rdquo;

    (setq frame-title-format "%b - Emacs")


<a id="org01cb474"></a>

## Password Store location

I&rsquo;m using `pass` (synced to my Bitwarden via a custom utility) to store/retrieve
secrets. Instead of the default location, my password storage lives at
`$HOME/.local/share/password-store`.

For `password-store` to work correctly, this needs to be set up.

    (setq auth-source-pass-filename
          (concat (getenv "HOME") "/.local/share/password-store"))


<a id="org7edcf3a"></a>

# Utils

For efficiently writing my config, I need some utils.


<a id="orgfa94113"></a>

## Add-Hooks

Small utility to add a function to multiple hooks at once.

    (defun add-hooks (hook-list function)
      "Add FUNCTION to all hooks in HOOK-LIST."
      (dolist (hook hook-list)
        (add-hook hook function)))


<a id="org93c8d16"></a>

# Org Mode


<a id="orge9c34a2"></a>

## Base Settings

I&rsquo;m using `~/org` as my base directory for org files. (This is then synced across devices).

    (setq org-directory "~/org/")
    (setq org-agenda-files (append (list org-directory) '("~/org/roam/journal.org" "~/org/roam/inbox.org" "~/org/roam/todo.org")))
    
    (setq org-use-property-inheritance t)
    (setq org-log-done 'time) ; Log time when task completes
    (setq org-list-allow-alphabetical t)       ; a, A, a) A) list bullets)
    (setq org-catch-invisible-edits 'smart) ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{})
    
    (setq org-return-follows-link 1)
    (setq calendar-week-start-day 1) ;; start on monday
    (setq org-agenda-include-diary t)


<a id="orga44dcbd"></a>

## Visual improvements


<a id="org0034510"></a>

### Headlines

Set headlines to the foreground color, successively darkened:

    (defun set-org-headline-color ()
      "Set the org headline colors to darker variants of the foreground color."
      (dotimes (i 8)
        (set-face-foreground (intern (format "org-level-%d" (1+ i))) (doom-color 'fg)))
      (set-face-foreground 'org-document-title (doom-color 'fg)))
    
    (add-hook 'org-mode-hook 'set-org-headline-color)

Hide leading stars:

    (setq org-hide-leading-stars nil)

Indent content based on headline level:

    (setq org-startup-indented nil)


<a id="org79cec3f"></a>

### Fonts

Org mode should use a variable pitch font for better readability.

    (add-hook 'org-mode-hook #'mixed-pitch-mode)

Line spacing should be higher in org-mode than in &ldquo;code&rdquo; modes.

    (add-hook 'org-mode-hook
              (lambda () (setq line-spacing 0.2)))


<a id="org0800c2f"></a>

### Org-Modern

Org-modern (<https://github.com/minad/org-modern>) contains a lot of nice visual
improvements for org-mode.

    (package! org-modern)

    (use-package! org-modern
      :defer t
      :hook (org-mode . global-org-modern-mode)
      :config
      (setq org-modern-label-border 0.1
            org-modern-star 'replace))

Set up org-modern

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

(It&rsquo;s pretty bare-bones for now, though the author has more examples for theming in their README.)

Use it globally and tweak it some more.

Org-modern comes with prettify-symbols mode, so I&rsquo;m defining some symbols and
enabling it.

    (setq prettify-symbols-alist
          '(("CLOCK:" . ?)
            (":LOGBOOK:" . ?)
            (":END:" . ?-)))


<a id="org5923b4b"></a>

### Miscellanious improvements


<a id="orgc9f483f"></a>

#### Show passed deadlines as error

    (setq org-agenda-deadline-faces
          '((1.001 . error)
            (1.0 . org-warning)
            (0.5 . org-upcoming-deadline)
            (0.0 . org-upcoming-distant-deadline)))


<a id="orge382066"></a>

#### Show quote blocks in italic

    (setq org-fontify-quote-and-verse-blocks t)


<a id="org1dd5080"></a>

#### Defer font-lock

For a more responsive editing experience

    (defun locally-defer-font-lock ()
      "Set jit-lock defer and stealth, when buffer is over a certain size."
      (when (> (buffer-size) 50000)
        (setq-local jit-lock-defer-time 0.05
                    jit-lock-stealth-time 1)))


<a id="org0641cd4"></a>

### Hide `:PROPERTY:` Drawers

org-tidy is a package for hiding all `:PROPERTY:` drawers.

    (package! org-tidy)

I want to enable it per default in org-mode, but be able to toggle it with
`<localleader>-z`

    (use-package! org-tidy
      :defer t
      :hook (org-mode . org-tidy-mode)
      :config (map! :map org-mode-map
                    :localleader
                    :desc "Toggle org-tidy" "z" #'org-tidy-mode))


<a id="orgbfdbfaa"></a>

## Babel

Org-Babel provides two things related to code blocks:

1.  a way to execute code blocks inside of org files. In a way, it&rsquo;s similar to Jupyter Notebooks,

though it supports a lot more languages on the one hand, but does not (easily) pass around context between cells on
the other.

1.  &ldquo;Tangling&rdquo;, meaning it will strip the prose surrounding the code blocks and generate a program from the individual code
    blocks in the document. In fact, that&rsquo;s how this config is generated.


<a id="org7dc5682"></a>

### HTTP requests via babel

ob-http is a package that allows for making HTTP requests from within org-mode
using babel.

    (package! ob-http)

    (use-package! ob-http
      :defer t
      :commands org-babel-execute:http)

Example usage:

    #+begin_src http :pretty
    GET https://jsonplaceholder.typicode.com/posts/1
    #+end_src
    
    #+RESULTS:
    : {
    :   "userId": 1,
    :   "id": 1,
    :   "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
    :   "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
    : }


<a id="org1016297"></a>

### Babel header args

Sensible defaults for the various header arguments of code blocks.

See <https://org-babel.readthedocs.io/en/latest/header-args/>

    (setq org-babel-default-header-args
          '((:session . "none")
            (:results . "replace")
            (:exports . "code")
            (:cache . "no")
            (:noeweb . "no")
            (:hlines . "no")
            (:tangle . "no")
            (:comments . "link")))


<a id="orgee8c9e3"></a>

### Auto-Tangling

I want org to tangle my config.org on file save, regardless whether it&rsquo;s the one
loaded or in a different repo.

    (defun org-babel-tangle-config ()
      (when (string-equal (file-name-nondirectory (buffer-file-name))
                          "config.org")
        (let ((org-config-babel-evaluate nil))
          (org-babel-tangle))))
    
    (add-hook 'org-mode-hook
              (lambda ()
                (add-hook 'after-save-hook #'org-babel-tangle-config)))


<a id="orgb4f705b"></a>

### Typescript

To execute typescript code blocks, I&rsquo;m using the `ob-typescript` package.

    (package! ob-typescript)


<a id="org3874599"></a>

### Export headings up to five levels deep

    (setq org-export-headline-levels 5)


<a id="org4ed5991"></a>

### Latex fragments

    (setq org-highlight-latex-and-related '(native script entities))


<a id="orgb9b3c60"></a>

### Mermaid Diagrams

Using ob-mermaid, I can generate diagrams using the mermaid syntax.

    (package! ob-mermaid)

Since mmdc is handled via nix, I&rsquo;ll need to get the binary&rsquo;s path during runtime via a shell call.

    (setq ob-mermaid-cli-path (shell-command-to-string "printf %s \"$(readlink -f $(which mmdc))\""))

Then, I&rsquo;ll need to add mermaid to the list of babel languages.

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((mermaid . t)))


<a id="org698afa8"></a>

#### TODO All mermaid diagrams have a white background


<a id="org39cecb7"></a>

## Roam

Org-Roam is a Zettelkasten implementation. In short, it allows for writing down multiple small notes and interlinking them,
creating pretty graphs. See also Obsidian or Logseq


<a id="orgc94fec5"></a>

### Use the same directory as org

All roam files should be under a subdirectory of my org directory - so I can sync them with the same mechanism.

    (setq org-roam-directory (concat org-directory "roam"))


<a id="org47c42e8"></a>

### Capture Templates & Shortcuts

I&rsquo;m adding some capture tempates for todos and journal entries, as well as some helper functions to directly call them.

    (setq org-roam-capture-templates
          '(("o" "moc" plain
             "\n*Link*:  %?\n\n"
             :if-new (file+head "1-main/${slug}.org" "#+title: ${title}\n#+filetags: :moc:\n#+hugo_section: braindump\n#+date: %u\n#+hugo_lastmod: %u\n#+hugo_tags: noexport\n")
             :immediate-finish t
             :unnarrowed t
             :empty-lines-after 1)))
    ;; (setq org-roam-capture-templates
    ;; (add-to-list 'org-roam-capture-templates
    ;;              '("j" "Journal" entry "* %T %?" :target (file+datetree "journal.org" day))
    ;;              '("t" "Todo" entry "* TODO %?" :target (file+head "todo.org" "Inbox")))

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


<a id="orgbedb714"></a>

### Add Org-Roam UI

Org-Roam UI is a web-based interface for Org-roam. It is a separate package -
and it also needs the websocket package as dependency.

The closest comparison to org-roam-ui is Obsidian.

    (package! org-roam-ui)
    (package! websocket)

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


<a id="orgfee94d5"></a>

### TODO Capture to Org Roam Dailies

I want to be able to use `org-capture` to capture stuff in the current date&rsquo;s roam daily file.


<a id="org157f3c4"></a>

### TODO Logseq compatibility

Org-Roam is so similar to Logseq that they can *almost* be used together. I&rsquo;m mostly following [Core Dumped&rsquo;s Guide on how to integrate both](https://coredumped.dev/2021/05/26/taking-org-roam-everywhere-with-logseq/)


<a id="org1a49d79"></a>

#### Turn Logseq Nodes into Org-Roam Nodes

One of the incompatibilities between Logseq and Org-Roam is the way links between nodes are handled. Logseq inserts a file link, Org-Roam an Id link.
But nodes inserted via Logseq aren&rsquo;t properly recognized in Org-Roam.

There&rsquo;s an Elisp Snippet from Bill Burdick that turns Logseq notes into Org-Roam nodes (<https://gist.github.com/zot/ddf1a89a567fea73bc3c8a209d48f527>)

    ;; (load (expand-file-name "org-roam-logseq.el" doom-user-dir))


<a id="org0dabd75"></a>

## Org-Node

Org-Node is similar to Org-Roam - it allows for creating small notes and linking them, creating a knowledge graph.

(In fact, it&rsquo;s so similar that an existing Org-Roam knowledge base should just work in org-node)

    (package! org-node)

    (use-package! org-node
      :after org
      :config (org-node-cache-mode))

Additionally, org-node-fakeroam can speed up org-roam itself, as well as allow for the usage of org-node and org-roam in parallel.

    (package! org-node-fakeroam)

    (use-package! org-node-fakeroam
      :defer)


<a id="orgf9edde5"></a>

## Fixes and miscellanious improvements


<a id="org4fa885f"></a>

### Prevent org-block face for latex fragments, since they look weird

    (require 'org-src)
    (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))


<a id="org72c2112"></a>

### Nix-Doom-Emacs messes with dashboard

I&rsquo;m using the nix-doom-emacs package to install emacs & dependencies, and for
some reason, using that binary, the Dashboard is disabled. [This comment](https://github.com/nix-community/nix-doom-emacs/issues/88#issuecomment-1115500602) in a
corresponding GH issue has a fix.

    (add-hook! 'emacs-startup-hook #'doom-init-ui-h)


<a id="org29214aa"></a>

### Faster insertion of org structures (i.e. source blocks)

Org-Tempo provides shortcuts like `"<s" - <TAB>` for generating code blocks.
For some reason, it does not start at launch, so I&rsquo;m  loading it here.

    (use-package! org-tempo)


<a id="org78a5b5b"></a>

### Automatic list item insertion

The package `org-autolist` makes org lists behave more like traditional text
editors, meaning `<Return>` will insert a list item first, `<Return><Return>` will insert a
newline, etc.

    (package! org-autolist)

    (use-package! org-autolist
      :hook (org-mode . org-autolist-mode))


<a id="org3015141"></a>

## Capture


<a id="orgb27c65f"></a>

### Add / change capture templates

Some other tools (like orgzly) work better with `TODO` instead of `[ ]`, so I&rsquo;m adjusting my capture templates.

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


<a id="orge120834"></a>

### TODO Improve org-capture dialog

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

The [org-capture bin](file:///Users/tibor.pilz/.emacs.d/bin/org-capture) is rather nice, but It would be even nicer with a smaller frame, and
no modeline.

    (setf (alist-get 'height +org-capture-frame-parameters) 15)
          ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
    (setq +org-capture-fn
          (lambda ()
            (interactive)
            (set-window-parameter nil 'mode-line-format 'none)
            (org-capture)))


<a id="org16f3c87"></a>

## Agenda and Time Management


<a id="org56a6935"></a>

### Khal / Khalel

Khal is a  CLI tool for managing calendars and events. It can be synced with
various services, like Google Calendar, through vdirsyncer.

Khalel is an Emacs package that sits between the CLI and Org mode, allowing for
easy integration of Khal with Org.

    (package! khalel)

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


<a id="org598e01e"></a>

# Workspaces & Projects


<a id="org6050a7f"></a>

## Projectile Project Search Path

Search for projects in  `~/Code/`, but only one level deep.

    (setq projectile-project-search-path '(("~/Code/" . 1)))


<a id="org992635c"></a>

## Disable Automatic Workspace Creation

Per default, emacs creates a workspace for every project - prohibiting, for
instance, side-by-side editing. I&rsquo;m disabling this behavior.

    (setq +workspaces-on-switch-project-behavior nil)


<a id="org558b491"></a>

# Development


<a id="org0e6c379"></a>

## Language-Specific Settings


<a id="org47cc353"></a>

### Web Dev (JS/TS/CSS)


<a id="org9a6d8f4"></a>

#### Testing

Add a package for Jest testing

    (package! jest)

    (use-package! jest
      :after (typescript-mode js-mode typescript-tsx-mode)
      :hook (typescript-mode . jest-minor-mode))

Set up regexes for &ldquo;sibling files&rdquo; - so I&rsquo;m able to jump from `src/foo/bar.ts` to `test/foo/bar.test.ts.`

    (setq find-sibling-rules
          '(("src/\\(.*/\\)?\\([^/]+\\)\\.\\(ts\\|vue\\)\\'"
             "test/.*\\2.test.ts")
            ("test/\\(.*/\\)?\\([^/]+\\)\\.test.ts\\'"
             "src/.*\\2.\\(ts\\|vue\\)")))


<a id="org2c75935"></a>

##### TODO This seems to only work for direct descendants of `src` and `test`.


<a id="org236914c"></a>

#### TODO Eslint

Eslint keeps track of all projects it has been run in, and - even if only one workspace is open,
will start to run in all of them.

The following sets it to run only in the current session.

    ;; (advice-add 'lsp
    ;;             :before (lambda (&rest _args)
    ;;                       (setf (lsp-session-server-id->folders (lsp-session)) (ht))))

Now, this runs into the issue that switching workspaces will not automatically
switch the client, so I&rsquo;m adding a hook to restart all lsp clients when I&rsquo;m
switching workspaces.

First, I need to define a function to restart all clients.


<a id="org493e910"></a>

#### Svelte

    (package! svelte-mode)

    (use-package! svelte-mode
      :defer t
      :mode "\\.svelte\\'")


<a id="org7b86fe9"></a>

#### Vue

Vue SFC files are recognized and handled by `web-mode`, so I only need to set up
some tweaks regarding the lsp and indentation.

⚠️ To get lsp support working, there needs to be a `.volarrc` file in the project&rsquo;s
root directory.

Remove 1 space padding from `<script>` tags, set indent to 2.

    (with-eval-after-load 'web-mode
      (setq web-mode-script-padding 0)
      (setq web-mode-style-padding 0)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-markup-indent-offset 2))


<a id="org6e72086"></a>

#### Astro

Add a package for a dedicated `astro` mode:

    (package! astro-ts-mode)

Because `astro-ts-mode` uses treesitter, treesitter needs to be set up to handle
`.astro` files properly.

    (setq treesit-language-source-alist
          '((astro "https://github.com/virchau13/tree-sitter-astro")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

Define Astro as a derived mode for `.astro` files.

    (define-derived-mode astro-mode web-mode "astro")
    (setq auto-mode-alist
          (append '(("\\.astro\\'" . astro-mode))
                  auto-mode-alist))

Register the astro-ls binary for lsp support.

    (with-eval-after-load 'lsp-mode
      (add-to-list 'lsp-language-id-configuration '(astro-mode . "astro"))
      (lsp-register-client
        (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls"))
                        :activation-fn (lsp-activate-on "astro")
                        :server-id 'astro-ls)))


<a id="orgc0ff9ca"></a>

#### Tailwind

Add the tailwind lsp package

    (package! lsp-tailwindcss
      :pin "3e3cc80a448e9dd24663eaa41742cda686dac5ab"
      :recipe (:host github
               :repo "merrickluo/lsp-tailwindcss"))

&#x2026;and use it

    (use-package! lsp-tailwindcss
      :defer t
      :init
      (setq lsp-tailwindcss-add-on-mode t)
      (setq lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode typescript-mode typescript-tsx-mode rust-mode rustic-mode))
      )


<a id="org3178a42"></a>

#### Code formatting

Set typescript, javascript and json file indentation to be 2 levels by default.

    (setq typescript-indent-level 2)
    (setq js-indent-level 2)


<a id="orgb5449ec"></a>

#### TODO Typescript REPL


<a id="org04886ae"></a>

### Nix

[Nix-mode](https://github.com/NixOS/nix-mode) is a major mode for editing nix expressions.

    (use-package! nix-mode
      :mode "\\.nix\\'")

It comes with a variaty of
submodules:


<a id="org7b6a32a"></a>

#### nix.el

Nix.el contains some miscellanious tools. Interactive functions include:

-   nix-unpack - unpack source of a Nix attribute.
    Available via `M-x nix-unpack` followed by the nix path and attribute path.

-   nix-build - functions similar to `M-x compile`. Will build in the current
    directory if it contains a `default.nix`.

There are also basic functions for interacting with nix - some variables are
provided to point to Nix binaries that can be used in Lisp code.

-   `nix-executable`
-   `nix-build-executable`
-   `nixinstantiate-executable`
-   `nix-store-executable`
-   `nix-shell-executable`

Also, a function `nix-system` is provided to get the current system (the way Nix
detects it).


<a id="org36c1497"></a>

#### nix-flake.el

Uses transient.el to provide a magit-like interface for supporting flake
commands.
Using `M-x nix-flake` commands can be run on the current flake, whereas `M-x
~nix-flake-init` can initialize a flake from a atemplate.


<a id="org750ff3b"></a>

#### nix-repls.el

Provides an interface for completion, nused by nix-company.el. Secondly it
provides an interactive function to open a repl via `M-x nix-repl`


<a id="orgc971e78"></a>

#### nix-store.el

Displays information about the store path including logs associated with a
derivation.


<a id="org4b329a1"></a>

#### nix-prettify-mode.el

Improves display of store paths.


<a id="org3ab36f5"></a>

### Python


<a id="orgaf975b0"></a>

#### Poetry

After years of frustration, I&rsquo;m finally content with setting up and managing
projects in the Python ecosystem, thanks to Poetry. It&rsquo;s a great tool, and
luckily, there is excellent integration with Emacs.

    (package! poetry)


<a id="org69e2fb8"></a>

#### Run pytest in virtualenv

python-pytest does not use the virtualenv&rsquo;s binary by default. As a fix, I&rsquo;m
adding a hook to python-mode to set the correct executable - since python-mode
plays nicely with direnv.

    (add-hook! python-mode
      (advice-add 'python-pytest-file :before
                  (lambda (&rest args)
                    (setq-local python-pytest-executable
                                (executable-find "pytest")))))


<a id="org9223840"></a>

### Terraform

There are two competing lsp servers for Terraform with support in Emacs, but,
although `terraform-lsp` is the more featurerich, I&rsquo;m sticking with `terraform-ls`
for now, but, `terraform-lsp` is a good alternative, with some nice- to- haves.

    (setq lsp-terraform-ls-enable-show-reference t)
    (setq lsp-semantic-tokens-enable t)
    (setq lsp-semantic-tokens-honor-refresh-requests t)


<a id="orge5e0e92"></a>

### Haskell

There&rsquo;s a Doom-Emacs module for setting up haskell-mode together with Haskell&rsquo;s
lsp, but currently, a bug prevents `haskell-mode` from working properly:
<https://github.com/haskell/haskell-mode/issues/1825>

A workaround is to explicitly set `flymake-allowed-file-name-masks` to nil.

    (setq flymake-allowed-file-name-masks nil)


<a id="orga6c60c0"></a>

### Jsonnet

Jsonnet is a data templating language.

First, a package for a jsonnet mode:

    (package! jsonnet-mode)

Then, lsp support:

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


<a id="org3d5b582"></a>

### Rust

Rust projects have multiple &ldquo;features&rdquo;, for instance, in Leptos there are
backend and frontend features. Per default, I want all of them to be enabled for
the lsp:

    (setq lsp-rust-features "all")


<a id="org41b9015"></a>

### MDX

MDX is a format combining JSX and Markdown (More info: <https://v0.mdxjs.com/>).
Sadly, there is no real emacs support via major-mode or LSP, but I can approximate
something.

First, I&rsquo;m setting the auto-mode for `.mdx` files to be `markdown-mode`:

    (setq auto-mode-alist
          (append '(("\\.mdx\\'" . markdown-mode))
                  auto-mode-alist))


<a id="org7cbc481"></a>

### Gleam

Gleam is a strictly typed functional language that runs on the Erlang VM (and can also be transpiled to Javascript).

    (package! gleam-ts-mode)

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


<a id="org8b61766"></a>

## Tools


<a id="org4203ed3"></a>

### Code Completion

I recently migrated from company to corfu, as it&rsquo;s also integrated in Doom Emacs and much more lightweight.


<a id="orge8d072a"></a>

#### Handling

I want to have a very low delay for the completion popup, and I want completions to start after the first letter.

    (setq  corfu-auto-delay 0.1
           corfu-auto-prefix 2
           corfu-left-margin-width 2
           corfu-right-margin-width 2
           corfu-bar-width 1)

I don&rsquo;t want Corfu to show up in the minibuffer

    (setq global-corfu-minibuffer nil)


<a id="org7410cbe"></a>

#### UI

Since I rely on theme colors, I need to add a hook for when the theme changes so that everything stays in sync.

    (defvar after-load-theme-hook nil
      "Hook run after a color theme is loaded using `load-theme'.")
    (defadvice load-theme (after run-after-load-theme-hook activate)
      "Run `after-load-theme-hook'."
      (run-hooks 'after-load-theme-hook))

Per default, the Corfu minibuffer is a bit cramped, so I&rsquo;m adjusting its buffer parameters

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

Corfu has a popup info feature that shows documentation for the currently selected completion. The default delay values for this are way too high, so I&rsquo;m setting them to 0.3 (initial) and 0.1 (subsequent) seconds.
I&rsquo;m also tweaking some settings related to the hiding of the popup and its size.

    (setq corfu-popupinfo-delay '(0.1 . 0.05)
          corfu-popupinfo-hide nil
          corfu-popupinfo-max-width 160
          corfu-popupinfo-min-width 160
          corfu-popupinfo-max-height 30
          corfu-popupinfo--buffer-parameters '((truncate-lines . nil)
                                               (left-margin-width . 2)
                                               (right-margin-width . 2)
                                               (word-wrap . t)))


<a id="org09e36c0"></a>

### Copilot

This package adds Github Copilot functionality to Emacs:

    (package! copilot
      :recipe (:host github
               :repo "copilot-emacs/copilot.el"
               :files ("*.el" "dist")))

When enabling copilot, I&rsquo;m hooking into `prog-mode` to enable it for all programming modes.
Further, I&rsquo;m disabling the warning about indentation (see <https://github.com/zerolfx/copilot.el/issues/220>),
and binding some keys.

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

In insert mode, I&rsquo;m binding  `C-SPC` to accept the complete suggestion. I&rsquo;m also binding `C-S-p` and `C-S-n` to navigate through the suggestions.
Additionally, I&rsquo;m binding `i g s` to show the suggestions, and `i g c`
to insert the suggestion for use in normal mode, and `t p` to toggle Copilot.

    (map! :leader
          (:prefix-map ("i" . "insert")
           (:prefix ("g" . "github copilot")
            :desc "Show Copilot Completion" "s" #'copilot-complete
            :desc "Insert Copilot Completion" "c" #'copilot-accept-completion))
          (:prefix ("t" . "toggle")
           :desc "Toggle Copilot" "p" #'copilot-mode))


<a id="org314f851"></a>

### Aider

Aider is a CLI tool for modifying codebases using LLMs. Aidermacs is a package that integrates Aider with Emacs.

    (package! aidermacs)

    (use-package! aidermacs
      :defer t
      :hook (aidermacs-minor-mode . (lambda () (setenv "OPENAI_API_KEY" (password-store-get "bitwarden/openai-gpt-key"))))
      :custom
      (aidermacs-use-architect-mode t)
      (aidermacs-default-model "4o"))


<a id="orge92c547"></a>

### Debugging

Doom Emacs has a debugger module which uses `dap-mode` under the hood.


<a id="orgfd092b2"></a>

#### Language-Specific Debugger settings


<a id="org480828c"></a>

##### Python

I&rsquo;m using debugpy for python.

    (setq dap-python-debugger 'debugpy)


<a id="orgeca90a4"></a>

#### Fixes


<a id="org05b7e19"></a>

##### Fix Doom &ldquo;+debugger/start&rdquo;

By default, `+debugger/start` will look for the last configuration set in the
project&rsquo;s doom-store - which has to be cleared manually to reset. This function
will remove the debugger configuration from the doom-store.

    ;;;###autoload
    (defun +debugger/clear ()
      "Clear the debugger configuration from the doom-store."
      (interactive)
      (doom-store-rem (doom-project-root) "+debugger"))

The old function is renamed to `+debugger/repeat`.

    (setq debugger-start-copy (symbol-function '+debugger/start))
    
    ;;;###autoload
    (defun +debugger/repeat (arg)
      "Start the debugger."
      (interactive)
      (funcall debugger-start-copy arg))

And `+debugger/start`  is redefined to clear the configuration before starting.

    ;;;###autoload
    (defun +debugger/start (arg)
      "Launch a debugger session.
    Launches the last used debugger, if one exists. Otherwise, you will be prompted
    for what debugger to use. If the prefix ARG is set, prompt anyway."
      (interactive "P")
      (message arg)
      (+debugger--set-config (+debugger-completing-read))
      (+debugger/start-last))


<a id="orge56b36e"></a>

##### Missing fringes in dap-mode

When running the dap-mode debugger, for some reason, the code window&rsquo;s fringes
get set to 0 width. This can be fixed with a workaround by setting the window&rsquo;s
buffer again via `set-window-buffer`. Since this only should happen on windows
with file buffers, we need some helper functions to get the correct window.

1.  Get the window containing a file buffer

    Since there&rsquo;s only one window with a file buffer when running the debugger, this
    can be kept fairly simple.
    
        (defun get-window-with-file-buffer ()
          "Get the window with a file buffer."
          (seq-find (lambda (window)
                      (buffer-file-name (window-buffer window)))
                    (window-list)))

2.  Reset file buffer window

    Using the helper function, we can reset the file window&rsquo;s buffer.
    
        (defun reset-file-window-buffer ()
          "Reset the file window's buffer."
          (let ((window (get-window-with-file-buffer)))
            (when window
              (set-window-buffer window (window-buffer window)))))

3.  Add reset to window configuration change hook

    Having tried multiple dap hooks to no avail, I&rsquo;ve resigned to just resetting the
    file window&rsquo;s buffer on every window configuration change. This can be achieved
    with the `window-configuration-change-hook`. Here, I only want to have the hook
    active when in a dap session, so I&rsquo;m adding the reset function after the dap
    session has been created and removing it when the session is terminated.
    
        (defun add-reset-file-window-buffer-hook (&rest args)
          "Add the reset-file-window-buffer function to the window-configuration-change-hook."
          (add-hook 'window-configuration-change-hook 'reset-file-window-buffer))
        
        (defun remove-reset-file-window-buffer-hook (&rest args)
            "Remove the reset-file-window-buffer function from the window-configuration-change-hook."
            (remove-hook 'window-configuration-change-hook 'reset-file-window-buffer))
        
        (add-hook 'dap-mode-hook 'add-reset-file-window-buffer-hook)


<a id="orgc1fc579"></a>

#### Keybindings

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


<a id="orgd7eee66"></a>

### Syntax Checking

For some reason, flycheck - especially when checking web files - is really slow.
To alleviate, it should only check the syntax on file-save.

    (setq flycheck-syntax-automatically '(save-mode-enable))


<a id="orgfd44372"></a>

### LSP

For the LSP settings, I&rsquo;m using the doom lsp module, which defaults to lsp-mode,
and lsp-ui. The alternative, eglot, would mean I&rsquo;d have to set up the language
servers myself instead of relying on `M-x lsp-install`. Although I have started to
work on a nix-workflow to install node-packages for that purpose, the
the ease of use of `M-x lsp-install` and the possibility of using lsp-ui means
I&rsquo;ll stick to lsp-mode for now.

Most of the language-specific settings are already defined under [5.1](#org0e6c379).
Here, I&rsquo;ll define some general settings.


<a id="org622d392"></a>

#### Performance

Using plists should increase the LSP performance.

    (setq lsp-use-plists 't)


<a id="orgb0be217"></a>

#### Handling

Set `capf` as completion provider.

    (setq lsp-completion-provider :capf)

Don&rsquo;t show completion item detail

    (setq lsp-completion-show-detail t)

Show completion item kind

    (setq lsp-completion-show-kind t)

Automatically start LSP on file open, guess root.

    (setq lsp-auto-guess-root t)
    (add-hook 'prog-mode-hook #'lsp-deferred)


<a id="org52dd01e"></a>

#### UI

Although I like using `lsp-ui-doc`, I don&rsquo;t want it to appear every time I&rsquo;m
hovering. Having a keybinding to glance at the documentation is fine for me.

    (map! :leader
          (:prefix ("c" . "code")
           :desc "Glance at documentation" "g" #'lsp-ui-doc-glance))

Enable lenses

    (setq lsp-lens-enable t)

Enable headerline with breadcrumbs.

    (setq lsp-headerline-breadcrub-enable t)

Disable eldoc, as it does not look that good and mostly serves as a distraction.

    (setq lsp-eldock-enable-hover nil)

Same with signature help, as well as help documentation

    (setq lsp-signature-auto-activate nil)
    (setq lsp-signature-render-documentation nil)

Set lsp-ui-doc sizing

    (setq lsp-ui-doc-max-height 400
          lsp-ui-doc-max-width 250)


<a id="org1dd4b9f"></a>

#### Emacs-LSP-Booster compatibility

LSP-Booster is a binary wrapper for language server binaries that pre-converts their JSON output to elisp bytecode
to offload this computation from emacs.

To correctly use it, I need to adjust the lsp command called from within emacs:

    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)
    
    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                (setcar orig-result command-from-exec-path))
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


<a id="org888b94e"></a>

### Git

Doom Emacs comes with Magit.


<a id="org73b9a97"></a>

#### Disable Evil-Mode in timemachine mode

    (eval-after-load 'git-timemachine
      '(progn
         (evil-make-overriding-map git-timemachine-mode-map 'normal)
         ;; force update evil keymaps after git-timemachine-mode loaded
         (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))


<a id="orgcd6f450"></a>

#### Gitlab Integration

Although Doom comes with Forge, the intergration leaves some things to be
desired.
`lab.el` is a package that provides more features that integrate with Gitlab,
like pipeline status, and MR interaction.

    (package! lab)

Set up the package with the default instance and the token from my password manager.

    (use-package! lab
      :config
      (setq lab-host "https://gitlab.com")
      (setq lab-token (password-store-get "bitwarden/gitlab-token")))

Add some keybindings und `SPC g` (Git)

    (map! :leader
          :desc "List Pipelines" "g l p" #'lab-list-project-pipelines
          :desc "List Merge Requests" "g l m" #'lab-list-project-merge-requests
          :desc "List all owned projects" "g l o" #'lab-list-all-owned-projects)


<a id="org20b24f3"></a>

#### Delta as Git Diff

    (package! diff-ansi)


<a id="org68a014e"></a>

### Documentation


<a id="org1436470"></a>

#### Devdocs

Devdocs (<https://elpa.gnu.org/packages/devdocs.html>) is a package for viewing
documentations, similar to Dash (<https://kapeli.com/dash>).

The documentation is hosted on <https://devdocs.io/> and is open source. Sadly,
Devdocs can not read docsets from Dash.


<a id="org0611e91"></a>

##### Install

    (package! devdocs)


<a id="org4013543"></a>

##### Configuration

Add keybindings under `SPC o D` (&ldquo;o&rdquo; for &ldquo;open&rdquo;, &ldquo;D&rdquo; for &ldquo;Devdocs&rdquo;).

    (map! :leader
          (:prefix ("D" . "devdocs")
           :desc "Open devdocs" "o" #'devdocs-peruse
           :desc "Search devdocs" "l" #'devdocs-lookup
           :desc "Install devdocs set" "i" #'devdocs-install))


<a id="orgd55c5a3"></a>

### AI Assistance


<a id="org2d2f5f8"></a>

#### GPTel

GPTel (<https://github.com/karthink/gptel>) is a package for interacting with
various LLMs.

Install

    (package! gptel)

Configure

    (use-package! gptel
      :config
      (setq! gptel-api-key (lambda () (password-store-get "bitwarden/openai-gpt-key")))
      (setq! gptel-model "gpt-4"))
    
    (map! :leader
          (:prefix ("o" . "open")
           :desc "Open GPTel" "g" #'gptel))


<a id="org4b47beb"></a>

### Task Runners

I&rsquo;m switching from Makefiles to Justfiles, using `just`.
There&rsquo;s `just.el` which allows you to call the recipe from within Emacs.

    (package! justl :recipe (:host github :repo "psibi/justl.el"))

I want to bind `just-exec-recipe` to `e` in normal mode.

    (use-package! justl
      :config
    
      (map! :leader
            (:prefix ("c" . "Code")
             :desc "Make" "m" #'justl))
      (map! :n "e" 'justl-exec-recipe))

(I also want to have a major mode for editing Justfiles)

    (package! just-mode)


<a id="org9f12192"></a>

### Diagrams


<a id="orgfe0ad17"></a>

#### Mermaid

`mermaid-mode` is a package for live previewing mermaid diagrams.

    (package! mermaid-mode)


<a id="org0c3ece4"></a>

### Markdown / Org Preview

There are multiple packages out there for previewing markdown, some of them not
maintained, some of them relying on the github API (via grip). Ideally, I&rsquo;d like
to have a web browser open that auto reloads either based on me saving the file
or on a certain idle time.

`impatient-mode` is a package for previewing HTML as you write it (including live-reload), so in theory,
using pandoc to convert the current buffer&rsquo;s content to HTML should make it
possible to preview anything that pandoc can convert.

Inspiration: <https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/>

First, I&rsquo;ll set up markdown-mode to use pandoc as `markdown-command`.

    (use-package markdown-mode
      :mode ("\\.md\\'" . gfm-mode)
      :commands (markdown-mode gfm-mode)
      :config
      (setq markdown-command "pandoc -f markdown -t html5"))

Now, `impatient-mode` as well as `simple-httpd`.

    (package! simple-httpd)
    (package! impatient-mode)

    (use-package simple-httpd
      :config
      (setq httpd-port 7070))
    
    (use-package impatient-mode
      :commands impatient-mode)

Now, I&rsquo;m defining a filter to process the markdown buffer.

TODO improve markdown filter

To make my life easier when defining the HTML, I&rsquo;ll use a library to convert
Lisp to XML/HTML

    (package! esxml)

    (defun markdown-html-filter (buffer)
      (princ
       (with-temp-buffer
         (let ((tmp (buffer-name)))
            (set-buffer buffer)
            (set-buffer (markdown tmp))
            (format "<!DOCTYPE html><html><title>Markdown Preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/><body><article class=\"markdown-body\">%s</article></body></html>" (buffer-string))))
        (current-buffer)))

And finally, a function to start previewing the markdown buffer.

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


<a id="org09b66ce"></a>

## Theming


<a id="org8abed05"></a>

### Doom Themes

    (package! doom-themes)

Doom Themes require `all-the-icons` to be installed (even though Doom itself moved on to nerd-icons).
To keep compatibility, I&rsquo;m installing the package manually

    (package! all-the-icons)


<a id="orgdb72f01"></a>

### Catppuccin

Catppuccin is a color scheme using pastel colors. It&rsquo;s available for a variety
of tools.

    (package! catppuccin-theme)

Catppuccin has different &ldquo;flavors&rdquo;, which can be set via the `catppuccin-flavor`
variable.

-   `latte`: Light theme
-   `frappe`: Dark theme, muted colors
-   `macciato`: Dark theme, semi-muted colors
-   `mocha`: (default) Dark theme, vibrant colors

Since the default is a bit too vibrant for my taste, I&rsquo;m setting the flavor to `frappe`.

    (setq catppuccin-flavor 'frappe)


<a id="orgaa7ba0c"></a>

### Current Theme

The theme I&rsquo;m using for now is Doom-Nord-Aurora. It&rsquo;s included in `doom-themes`, looks pretty similar to Doom Nord, but has a bit more muted colors.

    (setq doom-theme 'doom-nord-aurora)


<a id="org59ddde2"></a>

### Misc Themes


<a id="org684d367"></a>

#### Grayscale

    (package! grayscale-theme)

It leetle much on the warm side for my tastes&#x2026;


<a id="org1fb2034"></a>

#### Tao Themes

Very appealing, minimalistic themes.

    (package! tao-theme)


<a id="org6a1b6f0"></a>

#### Ewal

Ewal (<https://github.com/cyruseuros/ewal>) is similar to (and builds upon)
pywal, but for Emacs. It allows you to set the theme of Emacs based on the
colors of your wallpaper. (Or other pictures).

    (package! ewal)
    (package! ewal-doom-themes)

    (use-package ewal
      :init (setq ewal-use-built-in-always-p nil
                  ewal-use-built-in-on-failure-p nil
                  ewal-built-in-palette "sexy-material"))


<a id="org3614e0b"></a>

### Theme Magic

In a stark difference to the other solutions, which wants to adjust Emacs to the buty of the
rest of the world, Theme Magic (<https://github.com/jcaw/theme-magic>), which uses
PyWal (again!) to adjust every color it can to match your glorious editor.

    (package! theme-magic)


<a id="orgd50d716"></a>

### Autothemer

More than auto&ldquo;magically&rdquo; generating hew themes, Autothemer
(<https://github.com/jasonm23/autothemer>) is more of a tool for those proficient
in themeing or those who want to be. a package for
generating color schemes, although it is more flexible than ewal or pywal.

    (package! autothemer)


<a id="orgc250bb5"></a>

### Base 16 Themes

Since they rely on only the 16 base terminal colors, base 16 themes are very
popular and make it easy to have a harmonized look across all your programs.
Fortunately, there is a base 16 theme for Doom Emacs, which stems from the
&ldquo;Tinted Themeing project&rdquo; (<https://github.com/tinted-theming/home>).

    (package! base16-theme)

Some of the themes have a bit too less contrast for my taste. I think  the issue
is that all 16 colors are taken &rsquo;as-is&rsquo;, whereas it should be possible to
create a color scheme with more nuance via color correcting the applied colors.

`kurecolor` seems to be a library aimed exactly at such a purpose.

    (package! kurecolor)


<a id="org40501f6"></a>

#### TODO: Implement more complex color schemes based on base16 colors


<a id="org8300ccf"></a>

## UI


<a id="org3eb5036"></a>

### Doom Modeline

Allow for more characters in the branch name

    (setq doom-modeline-vcs-max-length 50)

    (setq doom-modeline-hud t)

Disable additional unnecessary information

    (after! doom-modeline
      (setq doom-modeline-buffer-encoding nil)
      (setq doom-modeline-modal nil)
      (setq doom-modeline-column-format "")
      (setq size-indication-mode nil)
      (setq doom-modeline-bar-width 0))

Remove size indicator and column number mode

    (after! doom-modeline
      (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
      (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
      (line-number-mode -1))

Hide Modeline in Treemacs

    (add-hook 'treemacs-mode-hook (lambda () (hide-mode-line-mode)))


<a id="org106ec87"></a>

### General Padding

The way windows have been close together always kind of botheres me. Luckily,
there&rsquo;s a package that seems to halp with simple padding, without any
contortions.

    (package! spacious-padding)

To actually use it, I need to find a better color scheme, where the edges aren&rsquo;t
as jarring, but it&rsquo;s very promising. The individual spacings can be adjusted
with the following:

    (use-package! spacious-padding
      :config
      (setq spacious-padding-width '(
        :internal-border-width 15
        :header-line-width 4
        :mode-line-width 6
        :tab-width 8
        :right-divider-width 30
        :scroll-bar-width 8)))

Furthermore, there&rsquo;s a &ldquo;subtle mode line setting&rdquo;, Which subdues the modelines a
bit. There&rsquo;s the possibility to target individual faces of the modelines and
change them based on whether they&rsquo;re active or not  - but I&rsquo;m content with just
setting the modeline to have the window&rsquo;s background color.

    (setq spacious-padding-subtle-mode-line t)

Now that everything&rsquo;s set up, I&rsquo;m enabling the mode.

    (spacious-padding-mode 1)

(The actual values obviously need to be tweaked though)


<a id="org4f43e73"></a>

### Better Error Display

The posframe that flycheck-posframe is using for error display seems to linger
for a while, and does not disappear until you stop moving the cursor.

This snippet makes it disappear instantly:

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


<a id="orgc5c46f5"></a>

### Treemacs Modeline

I don&rsquo;t really need the modeline in the treemacs buffer, so I&rsquo;m disabling it.

    (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)


<a id="orgf5e7e68"></a>

### Solaire-Mode

Solaire-Mode provides a way to change the background color of buffers that are
not the current buffer, making the current buffer stand out more.

This is enabled per default in Doom Emacs, but it results in issues with Treemacs,
so I&rsquo;m disabling it.

    (package! solaire-mode :disable t)


<a id="org3f28de0"></a>

## Vertico

Prefix the current candidate with an arrow

    (defun minibuffer-format-candidate (orig cand prefix suffix index _start)
      (let ((prefix (if (= vertico--index index)
                        " > " "   ")))
        (funcall orig cand prefix suffix index _start)))
    
    (advice-add #'vertico--format-candidate
                :around #'minibuffer-format-candidate)

Don&rsquo;t show results count

    (setq vertico-count-format nil)

Make vertico-posframe a little wider

    (setq vertico-posframe-width 200)

Add fringe to vertico-posframea

    (setq vertico-posframe-parameters
          '((left-fringe . 16)
            (right-fringe . 8)
            (border-width . 16)))


<a id="org181b384"></a>

## Kubernetes

`kubernetes-mode` brings a lot of snippets.

    (package! k8s-mode)
    (package! k8s-mode)


<a id="org40eb296"></a>

# Performance

Various tweaks to improve the overall performance.

Raise the GC-Cons threshold

    (setq gc-cons-threshold (* 1024 1024 1024)) ;; 1G

Increase the amount of data which Emacs reads from the process

    (setq read-process-output-max (* 4 1024 1024)) ;; 4mb

Ignore JSONRPC logs

    (fset #'jsonrpc--log-event #'ignore)


<a id="org89e5348"></a>

# Unsorted Packages


<a id="org38b2ab7"></a>

## pcre2el

pcre2el is included with doom emacs but pinned to a version that is currently throwing warnings because of an
obsolete variable.

    (unpin! pcre2el)


<a id="org0491a1c"></a>

## Discord Presence

    (package! elcord)

    (use-package! elcord
      :config
      (setq elcord-editor-icon "emacs_icon"))

