vim.g.markdown_fenced_languages = { "bash=sh", "json", "python", "ts=typescript", "vim", "vue", "yaml", "nix" }
vim.treesitter.language.register("markdown", { "vimwiki" })

return {
  ---- Preview
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    build = "cd app && yarn install",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
    ft = { "markdown" },
  },
  ---- Folding/Concealing
  {
    "plasticboy/vim-markdown",
    dependencies = {
      {"godlygeek/tabular"},
    },
  },
}
