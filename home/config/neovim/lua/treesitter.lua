require'nvim-treesitter.configs'.setup {

  ensure_installed = {
    "typescript",
    "javascript",
    "vue",
    "tsx",
    "rust",
    "lua",
    "vim"
  },

  -- Install parsers asynchronously...
  sync_install = false,
  -- ...but make sure they'll get installed when they're missing (in the current buffer)
  auto_install = true,

  highlight = { enable = true },
  incremental_selection = { enable = true },
  textobjects = { enable = true },
}
