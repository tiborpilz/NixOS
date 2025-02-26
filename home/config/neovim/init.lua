-- Basic Settings
--_ Set encoding
vim.o.encoding = "utf-8"
vim.scriptencoding = "utf-8"

---- Basic Settings
vim.o.compatible = false
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.expandtab = true
vim.o.wrap = true
vim.o.mouse = "a"
vim.o.directory = vim.fn.expand("$HOME/.vim/tmp")
vim.o.backupdir = vim.fn.expand("$HOME/.vim/tmp")
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.shiftround = true
vim.o.smartindent = true
vim.o.errorbells = false
vim.o.autoread = true
vim.o.modeline = true
vim.o.modelines = 5
vim.o.foldenable = false

-- Use system clipboard
vim.o.clipboard = "unnamedplus"

--- Persistent undo
vim.o.undofile = true

--- Set Hybrid Line Numbers
vim.wo.relativenumber = true
vim.wo.number = true

--- Enable syntax highlighting
vim.cmd("syntax enable")

--- Smoother update
vim.o.updatetime = 1000

--- Space as Leader
vim.g.mapleader = " "

--- Sign column settings
vim.wo.signcolumn = "yes:1"
vim.cmd("highlight clear SignColumn")

--- Fill characters
vim.o.fillchars = "eob: "

--- Load plugins
require('plugins')

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({ { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." }, }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup("plugins")
