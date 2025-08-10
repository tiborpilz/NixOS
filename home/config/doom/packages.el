(package! org-modern)

(package! org-tidy)

(package! ob-http)

(package! ob-typescript)

(package! ob-mermaid)

(package! org-roam-ui)
(package! websocket)

(package! org-node)

(package! org-node-fakeroam)

(package! org-autolist)

(package! khalel)

(package! org-ql)

(package! jest)

(package! svelte-mode)

(package! astro-ts-mode)

(package! lsp-tailwindcss
  :pin "3e3cc80a448e9dd24663eaa41742cda686dac5ab"
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))

(package! poetry)

(package! jsonnet-mode)

(package! gleam-ts-mode)

(package! lean4-mode
  :recipe (:host github
           :repo "leanprover-community/lean4-mode"
           :files ("*.el" "data")))

(package! ob-lean4
  :recipe (:host github
           :repo "Maverobot/ob-lean4"
           :files ("*.el")))

(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el" "dist")))

(package! aidermacs)

(package! lab)

(package! diff-ansi)

(package! devdocs)

(package! gptel)

(package! justl :recipe (:host github :repo "psibi/justl.el"))

(package! just-mode)

(package! mermaid-mode)

(package! simple-httpd)
(package! impatient-mode)

(package! esxml)

(package! doom-themes)

(package! all-the-icons)

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

(package! solaire-mode :disable t)

(package! k8s-mode)
(package! k8s-mode)

(unpin! pcre2el)

(package! elcord)
