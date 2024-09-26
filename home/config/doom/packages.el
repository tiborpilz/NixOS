(package! org-modern)

(package! org-tidy)

(package! ob-http)

(package! ob-typescript)

(package! org-roam-ui)
(package! websocket)

(package! org-autolist)

(package! jest)

(package! svelte-mode)

;; (unpin! lsp-mode)

(package! astro-ts-mode)

(package! lsp-tailwindcss
  :pin "3e3cc80a448e9dd24663eaa41742cda686dac5ab"
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))

(package! poetry)

(package! jsonnet-mode)

(package! copilot
  :pin "c6b31f640eca89bb68d3c5005f22c27d033cc92e"
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el" "dist")))

(package! lab)

(package! diff-ansi)

(package! devdocs)

(package! gptel)

(package! gptel-extensions
  :pin "671e5186153fc9bf105c91effb1ed7db06508677"
  :recipe (:host github
           :repo "kamushadenes/gptel-extensions.el"))

(package! quarto-mode)

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
