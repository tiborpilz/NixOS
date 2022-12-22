;; [[file:config.org::*Fix straight.el][Fix straight.el:1]]
(package! straight :pin "3eca39d")
;; Fix straight.el:1 ends here

;; [[file:config.org::*Editorconfig][Editorconfig:1]]
(package! editorconfig)
;; Editorconfig:1 ends here

;; [[file:config.org::*org-present][org-present:1]]
(package! org-present)
;; org-present:1 ends here

;; [[file:config.org::*Add Org-Roam UI][Add Org-Roam UI:1]]
(unpin! org-roam)
(package! org-roam-ui)
(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980") ; dependency of `org-roam-ui'
;; Add Org-Roam UI:1 ends here

;; [[file:config.org::*Google Calendar integration][Google Calendar integration:1]]
(package! org-gcal)
;; Google Calendar integration:1 ends here

;; [[file:config.org::*Automatically commit via git][Automatically commit via git:1]]
(package! git-auto-commit-mode)
;; Automatically commit via git:1 ends here

;; [[file:config.org::*Jest Test Mode][Jest Test Mode:1]]
(package! jest-test-mode)
;; Jest Test Mode:1 ends here

;; [[file:config.org::*Svelte][Svelte:1]]
(package! svelte-mode)
;; Svelte:1 ends here

;; [[file:config.org::*LSP][LSP:1]]
(unpin! lsp-mode)
;; LSP:1 ends here

;; [[file:config.org::*Tailwind][Tailwind:1]]
(package! lsp-tailwindcss
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))
;; Tailwind:1 ends here

;; [[file:config.org::*Nix-sandbox][Nix-sandbox:1]]
(package! nix-sandbox)
;; Nix-sandbox:1 ends here

;; [[file:config.org::*Poetry][Poetry:1]]
(package! poetry)
;; Poetry:1 ends here

;; [[file:config.org::*Live Preview][Live Preview:1]]
(package! impatient-mode)
;; Live Preview:1 ends here

;; [[file:config.org::*Getting a Node 16 binary][Getting a Node 16 binary:1]]
(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
;; Getting a Node 16 binary:1 ends here

;; [[file:config.org::*Getting a Node 16 binary][Getting a Node 16 binary:2]]
(package! nvm)
;; Getting a Node 16 binary:2 ends here

;; [[file:config.org::*Install][Install:1]]
(package! devdocs)
;; Install:1 ends here

;; [[file:config.org::*Doom Themes][Doom Themes:1]]
(package! doom-themes)
;; Doom Themes:1 ends here

;; [[file:config.org::*Grayscale][Grayscale:1]]
(package! grayscale-theme)
;; Grayscale:1 ends here

;; [[file:config.org::*Tao Themes][Tao Themes:1]]
(package! tao-theme)
;; Tao Themes:1 ends here

;; [[file:config.org::*Nano Modeline][Nano Modeline:1]]
;; (package! nano-modeline)
;; Nano Modeline:1 ends here

;; [[file:config.org::*Which-Key][Which-Key:1]]
;; (package! which-key-posframe)
;; Which-Key:1 ends here

;; [[file:config.org::*All-The-Icons Ivy Rich][All-The-Icons Ivy Rich:1]]
(package! all-the-icons-ivy-rich)
;; All-The-Icons Ivy Rich:1 ends here

;; [[file:config.org::*Treemacs-All-The-Icons][Treemacs-All-The-Icons:1]]
(package! treemacs-all-the-icons)
;; Treemacs-All-The-Icons:1 ends here

;; [[file:config.org::*Unsorted Packages][Unsorted Packages:1]]
(package! dap-mode)

;; Orgmode


;; Only show emphasis markers when editing them
(package! org-appear)

;; Citations
(package! org-ref :pin "3ca9beb744621f007d932deb8a4197467012c23a")

;; HTTP requests via babel
(package! ob-http :pin "b1428ea2a63bcb510e7382a1bf5fe82b19c104a7")

;; OrgRoam visualization / webapp

;; automatic latex rendering
(package! org-fragtog :pin "479e0a1c3610dfe918d89a5f5a92c8aec37f131d")

;; export github markdown
(package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")

;; K8s
(package! k8s-mode)

;; Copilot
;; (package! copilot
;;   :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; Multiple major modes in one buffer
(package! polymode)
(package! poly-markdown)
;; Unsorted Packages:1 ends here
