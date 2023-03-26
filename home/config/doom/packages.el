(package! org-padding :recipe (:host github :repo "toncherami/org-padding"))

(package! org-modern)

(package! ob-http)

;; (package! org-present)

(package! org-roam-ui)
(package! websocket) ; dependency of `org-roam-ui'

(package! transient
      :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
      :recipe (:host github :repo "magit/transient"))

(package! with-editor
          :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
          :recipe (:host github :repo "magit/with-editor"))

(package! org-gcal)

(package! jest-test-mode)

(package! svelte-mode)

(unpin! lsp-mode)

(package! lsp-tailwindcss
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))

(package! poetry)

(package! impatient-mode)

(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! nvm)

(package! devdocs)

;; (package! dash-docs)

(package! gpt)

(package! gptel)

(package! doom-themes)

(package! grayscale-theme)

(package! tao-theme)

(package! ewal)
(package! ewal-doom-themes)

(package! theme-magic)

(package! autothemer)

;; (package! which-key-posframe)

;; (package! all-the-icons-ivy-rich)

(package! treemacs-all-the-icons)

(package! xwwp :recipe (:host github :repo "BlueFlo0d/xwwp"))
(package! xwwp-follow-link-ivy)
(package! ctable)
