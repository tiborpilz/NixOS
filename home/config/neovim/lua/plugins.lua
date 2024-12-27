-- Settings
--- Various
-- vim.g.vim_markdown_conceal = 0
-- vim.g.vim_markdown_conceal_code_blocks = 0


--- Airline
-- Copilot

-- Colorscheme
-- vim.cmd("colorscheme nord")
-- vim.opt.termguicolors = true

-- LSP / Mason setup
-- require("lsp-config")
-- require("mason-setup")

-- -- Neotest
-- require("neotest-setup")

-- Formatting
-- Completion setup
-- require('cmp-config')

-- Treesitter settings
-- require("treesitter")
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

-- Diagostics settings
-- require('diagnostics')

return {}
