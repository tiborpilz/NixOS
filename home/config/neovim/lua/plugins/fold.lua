vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.lsp.foldexpr()"
vim.opt.foldlevel = 99
vim.opt.foldtext = "x"
vim.o.foldcolumn = '1'

return {
  -- 'kevinhwang91/nvim-ufo',
  -- dependencies = {
  --   'kevinhwang91/promise-async',
  -- },
  -- setup = function()
  --   vim.o.foldcolumn = '1' -- '0' is not bad
  --   vim.o.foldlevel = 99   -- Using ufo provider need a large value, feel free to decrease the value
  --   vim.o.foldlevelstart = 99
  --   vim.o.foldenable = true
  --
  --   -- Using ufo provider need remap `zR` and `zM`. If Neovim is 0.6.1, remap yourself
  --   vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
  --   vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)
  --
  --   -- Option 1: coc.nvim as LSP client
  --   require('ufo').setup()
  -- end
}
