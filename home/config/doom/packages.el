(package! editorconfig)

(package! org-modern)

(package! org-outer-indent
  :recipe (:host github :repo "rougier/org-outer-indent")
  :pin "9c5aef47f259a13baae73e50f9f897f85767718f")

(package! org-tidy)

(package! ob-http)

(package! ob-typescript)

;; (unpin! org-roam)
(package! org-roam-ui)
(package! websocket) ; dependency of `org-roam-ui'

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

(package! copilot)
  ;; :pin
  ;; :recipe (:host github
  ;;          :repo "copilot-emacs/copilot.el"
  ;;          :files ("*.el" "dist")))

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

(package! treemacs-nerd-icons :pin "9876cb478145a0ec4e36f64ff6583f3de7126216")

(package! k8s-mode)

(package! elcord)
