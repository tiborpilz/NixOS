-- vim.diagnostic.config({
--   virtual_text = false,
-- })
-- vim.o.updatetime = 250
-- vim.cmd [[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]]

-- Bind diagnostic to <Leader> c d
vim.api.nvim_set_keymap('n', '<Leader>cd', '<cmd>lua vim.diagnostic.open_float(nil, {focus=false})<CR>', { noremap = true, silent = true })
