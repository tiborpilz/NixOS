local function script_dir()
  local source = debug.getinfo(1, "S").source
  if source:sub(1, 1) == "@" then
    source = source:sub(2)
  end
  return vim.fn.fnamemodify(source, ":h")
end

local tests_dir = script_dir()
-- tests_dir is .../lua/neotree-lsp-symbols/tests; walk up 3 to the neovim config dir
local config_dir = vim.fn.fnamemodify(tests_dir, ":h:h:h")
local lazy_dir = vim.fn.stdpath("data") .. "/lazy"

vim.opt.rtp:prepend(lazy_dir .. "/plenary.nvim")
vim.opt.rtp:prepend(lazy_dir .. "/nui.nvim")
vim.opt.rtp:prepend(config_dir)

package.path = package.path
  .. ";" .. config_dir .. "/lua/?.lua"
  .. ";" .. config_dir .. "/lua/?/init.lua"

vim.cmd("runtime! plugin/plenary.vim")
