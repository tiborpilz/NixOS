vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.lsp.foldexpr()"
vim.opt.foldlevel = 99
vim.opt.foldtext = ""

return {
  -- TODO: plugin or something for better fold fill text
}
