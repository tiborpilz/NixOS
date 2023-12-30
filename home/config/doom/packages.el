(package! editorconfig)

(package! olivetti)

(package! org-modern)

(package! org-fragtog)

(unpin! org-roam)
(package! org-roam-ui)
(package! websocket) ; dependency of `org-roam-ui'

(package! jest)

(package! svelte-mode)

(unpin! lsp-mode)

(package! astro-ts-mode)

(package! lsp-tailwindcss
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))

(package! poetry)

(package! jsonnet-mode)

(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! lab)

(package! devdocs)

(package! gptel)

(package! gptel-extensions
  :recipe (:host github
  :repo "kamushadenes/gptel-extensions.el"))

(package! quarto-mode)

(package! mermaid-mode)

(package! simple-httpd)
(package! impatient-mode)

(package! esxml)

(package! doom-themes)

(package! catppuccin-theme)

(package! grayscale-theme)

(package! tao-theme)

(package! ewal)
(package! ewal-doom-themes)

(package! theme-magic)

(package! autothemer)

(package! base16-theme)

(package! kurecolor)

(package! spacious-padding)

(package! all-the-icons-ivy-rich)

(package! treemacs-nerd-icons :pin "9876cb478145a0ec4e36f64ff6583f3de7126216")

(package! xwwp :recipe (:host github :repo "BlueFlo0d/xwwp"))
(package! xwwp-follow-link-ivy)
(package! ctable)

(package! k8s-mode)

(package! dap-mode)

;; HTTP requests via babel
(package! ob-http :pin "b1428ea2a63bcb510e7382a1bf5fe82b19c104a7")

;; OrgRoam visualization / webapp

;; automatic latex rendering

;; export github markdown
(package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")
