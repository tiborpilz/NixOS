(package! straight :pin "3eca39d")

(package! editorconfig)

(package! exec-path-from-shell)

(package! org-modern)

(package! org-present)

(unpin! org-roam)
(package! org-roam-ui)
(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980") ; dependency of `org-roam-ui'

(package! org-gcal)

(package! jest)

(package! svelte-mode)

(unpin! lsp-mode)

(package! lsp-tailwindcss
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))

(package! nix-sandbox)

(package! poetry)

(package! impatient-mode)

(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! nvm)

(package! devdocs)

;; (package! dash-docs)

(package! gpt)

(package! doom-themes)

(package! grayscale-theme)

(package! tao-theme)

(package! ewal)
(package! ewal-doom-themes)

(package! theme-magic)

(package! autothemer)

;; (package! which-key-posframe)

(package! all-the-icons-ivy-rich)

(package! treemacs-all-the-icons)

(package! xwwp :recipe (:host github :repo "BlueFlo0d/xwwp"))
(package! xwwp-follow-link-ivy)
(package! ctable)

(package! dap-mode)

;; Citations
(package! org-ref :pin "3ca9beb744621f007d932deb8a4197467012c23a")

;; HTTP requests via babel
(package! ob-http :pin "b1428ea2a63bcb510e7382a1bf5fe82b19c104a7")

;; OrgRoam visualization / webapp

;; automatic latex rendering

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
