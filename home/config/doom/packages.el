;; [[file:config.org::*Org-Modern][Org-Modern:1]]
(package! org-modern)
;; Org-Modern:1 ends here

;; [[file:config.org::*Hide ~:PROPERTY:~ Drawers][Hide ~:PROPERTY:~ Drawers:1]]
(package! org-tidy)
;; Hide ~:PROPERTY:~ Drawers:1 ends here

;; [[file:config.org::*HTTP requests via babel][HTTP requests via babel:1]]
(package! ob-http)
;; HTTP requests via babel:1 ends here

;; [[file:config.org::*Typescript][Typescript:1]]
(package! ob-typescript)
;; Typescript:1 ends here

;; [[file:config.org::*Add Org-Roam UI][Add Org-Roam UI:1]]
(package! org-roam-ui)
(package! websocket)
;; Add Org-Roam UI:1 ends here

;; [[file:config.org::*Automatic list item insertion][Automatic list item insertion:1]]
(package! org-autolist)
;; Automatic list item insertion:1 ends here

;; [[file:config.org::*Testing][Testing:1]]
(package! jest)
;; Testing:1 ends here

;; [[file:config.org::*Svelte][Svelte:1]]
(package! svelte-mode)
;; Svelte:1 ends here

;; [[file:config.org::*Vue][Vue:1]]
;; (unpin! lsp-mode)
;; Vue:1 ends here

;; [[file:config.org::*Astro][Astro:1]]
(package! astro-ts-mode)
;; Astro:1 ends here

;; [[file:config.org::*Tailwind][Tailwind:1]]
(package! lsp-tailwindcss
  :pin "3e3cc80a448e9dd24663eaa41742cda686dac5ab"
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))
;; Tailwind:1 ends here

;; [[file:config.org::*Poetry][Poetry:1]]
(package! poetry)
;; Poetry:1 ends here

;; [[file:config.org::*Jsonnet][Jsonnet:1]]
(package! jsonnet-mode)
;; Jsonnet:1 ends here

;; [[file:config.org::*Copilot][Copilot:1]]
(package! copilot
  :pin "c6b31f640eca89bb68d3c5005f22c27d033cc92e"
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el" "dist")))
;; Copilot:1 ends here

;; [[file:config.org::*Gitlab Integration][Gitlab Integration:1]]
(package! lab)
;; Gitlab Integration:1 ends here

;; [[file:config.org::*Delta as Git Diff][Delta as Git Diff:1]]
(package! diff-ansi)
;; Delta as Git Diff:1 ends here

;; [[file:config.org::*Install][Install:1]]
(package! devdocs)
;; Install:1 ends here

;; [[file:config.org::*GPTel][GPTel:1]]
(package! gptel)
;; GPTel:1 ends here

;; [[file:config.org::*GPTel-extensions][GPTel-extensions:1]]
(package! gptel-extensions
  :pin "671e5186153fc9bf105c91effb1ed7db06508677"
  :recipe (:host github
           :repo "kamushadenes/gptel-extensions.el"))
;; GPTel-extensions:1 ends here

;; [[file:config.org::*Quarto][Quarto:1]]
(package! quarto-mode)
;; Quarto:1 ends here

;; [[file:config.org::*Mermaid][Mermaid:1]]
(package! mermaid-mode)
;; Mermaid:1 ends here

;; [[file:config.org::*Markdown / Org Preview][Markdown / Org Preview:2]]
(package! simple-httpd)
(package! impatient-mode)
;; Markdown / Org Preview:2 ends here

;; [[file:config.org::*Markdown / Org Preview][Markdown / Org Preview:4]]
(package! esxml)
;; Markdown / Org Preview:4 ends here

;; [[file:config.org::*Doom Themes][Doom Themes:1]]
(package! doom-themes)
;; Doom Themes:1 ends here

;; [[file:config.org::*Doom Themes][Doom Themes:2]]
(package! all-the-icons)
;; Doom Themes:2 ends here

;; [[file:config.org::*Catppuccin][Catppuccin:1]]
(package! catppuccin-theme)
;; Catppuccin:1 ends here

;; [[file:config.org::*Grayscale][Grayscale:1]]
(package! grayscale-theme)
;; Grayscale:1 ends here

;; [[file:config.org::*Tao Themes][Tao Themes:1]]
(package! tao-theme)
;; Tao Themes:1 ends here

;; [[file:config.org::*Ewal][Ewal:1]]
(package! ewal)
(package! ewal-doom-themes)
;; Ewal:1 ends here

;; [[file:config.org::*Theme Magic][Theme Magic:1]]
(package! theme-magic)
;; Theme Magic:1 ends here

;; [[file:config.org::*Autothemer][Autothemer:1]]
(package! autothemer)
;; Autothemer:1 ends here

;; [[file:config.org::*Base 16 Themes][Base 16 Themes:1]]
(package! base16-theme)
;; Base 16 Themes:1 ends here

;; [[file:config.org::*Base 16 Themes][Base 16 Themes:2]]
(package! kurecolor)
;; Base 16 Themes:2 ends here

;; [[file:config.org::*General Padding][General Padding:1]]
(package! spacious-padding)
;; General Padding:1 ends here

;; [[file:config.org::*Icons][Icons:1]]
(package! treemacs-nerd-icons :pin "9876cb478145a0ec4e36f64ff6583f3de7126216")
;; Icons:1 ends here

;; [[file:config.org::*Kubernetes][Kubernetes:1]]
(package! k8s-mode)
;; Kubernetes:1 ends here

;; [[file:config.org::*pcre2el][pcre2el:1]]
(unpin! pcre2el)
;; pcre2el:1 ends here

;; [[file:config.org::*Discord Presence][Discord Presence:1]]
(package! elcord)
;; Discord Presence:1 ends here
