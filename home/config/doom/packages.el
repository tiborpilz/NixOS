(package! org-modern)

(package! org-tidy)

(package! ox-hugo)

(package! ob-http)

(package! ob-typescript)

(package! ob-mermaid)

(package! org-roam-ui)
(package! websocket)

; (package! org-node)

; (package! org-node-fakeroam)

(package! org-autolist)

(package! khalel)

(package! org-ql)

(package! org-roam-ql)

;; (package! org-similarity)

(package! prisma-mode
  :pin "f7744a995e84b8cf51265930ce18f6a6b26dade7"
  :recipe (:host github
           :repo "pimeys/emacs-prisma-mode"
           :branch "main"))

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
  :pin "1388f9d1429e38a39ab913c6daae55f6ce799479"
  :recipe (:host github
           :repo "leanprover-community/lean4-mode"
           :files ("*.el" "data")))

(package! ob-lean4
  :pin "e2216aa61fd54b2abe3092247a5b08225db9b807"
  :recipe (:host github
           :repo "Maverobot/ob-lean4"
           :files ("*.el")))

(package! sage-shell-mode)
(package! ob-sagemath)

(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el" "dist")))

(package! lab)

(package! diff-ansi)

(package! devdocs)

; (package! ollama-buddy)

(package! justl
  :pin "edfc117e9643e44692651337274a8dc0e5163284"
  :recipe (:host github :repo "psibi/justl.el"))

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

;; (package! nano-modeline)

(package! spacious-padding)

(package! solaire-mode :disable t)

(package! k8s-mode)
(package! k8s-mode)

(package! ob-gptel
  :pin "cbed018a7d81de9ba8dc3220e1c4d10b7bb29b11"
  :recipe (:host github
           :repo "jwiegley/ob-gptel"
           :files ("*.el")))

(package! mcp)

(package! gptel-mcp
  :pin "aa78f4c74e706f985b2613e0ea86752a571dfa1a"
  :recipe (:host github
           :repo "lizqwerscott/gptel-mcp"))

(unpin! pcre2el)

(package! elcord)
