-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  -- Copilot
  {"github/copilot.vim"},
  {"glepnir/dashboard-nvim"},

  -- Telescope (Fuzzy Finder)
  {"nvim-lua/plenary.nvim"},
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {"nvim-lua/plenary.nvim"},
    keys = {
      {
        "<leader>pf",
        function() require("telescope.builtin").find_files() end,
        desc = "Find Files",
      },
      {
        "<leader>sp",
        function() require("telescope.builtin").live_grep() end,
        desc = "Search Project",
      },
      {
        "<leader>bb",
        function() require("telescope.builtin").buffers() end,
        desc = "List Buffers",
      },
      {
        "<leader>fh",
        function() require("telescope.builtin").help_tags() end,
        desc = "Find Help",
      },
    },
  },

  -- Icons
  {"ryanoasis/vim-devicons"},
  {"kyazdani42/nvim-web-devicons"},

  -- NERDtree
  {
    "preservim/nerdtree",
  },

  -- Which Key
  {"folke/which-key.nvim"},
  {"AckslD/nvim-whichkey-setup.lua"},

  -- Git
  {"tpope/vim-fugitive"},

  -- Statusline
  {
    "vim-airline/vim-airline",
    dependencies = {"vim-airline/vim-airline-themes"},
  },

  -- Floating Window Borders
  {"mikesmithgh/borderline.nvim"},

  -- Comments
  {"tpope/vim-commentary"},

  -- Treesitter
  {"nvim-treesitter/nvim-treesitter"},

  -- LSP
  {"williamboman/mason.nvim"},
  {"williamboman/mason-lspconfig.nvim"},
  {"neovim/nvim-lspconfig"},

  -- LSP Lightbulb (Code Actions)
  {"kosayoda/nvim-lightbulb"},

  -- LSP Signatures
  {"ray-x/lsp_signature.nvim"},

  -- Formatting
  {"stevearc/conform.nvim"},
  
  -- Autocompletion
  {"hrsh7th/nvim-cmp"},
  {"hrsh7th/cmp-nvim-lsp"},

  -- Tests
  {"vim-test/vim-test"},

  -- Run Snippets
  -- TODO: set up correctly
  {
    "michaelb/sniprun",
    build = "sh install.sh",
    config = function()
      require("sniprun").setup({})
    end,
  },

  -- TODO
  -- hkupty/iron.nvim REPL
  -- Colorschemes
  {"eddyekofo94/gruvbox-flat.nvim"},
  {"marko-cerovac/material.nvim"},
  {"kdheepak/monochrome.nvim"},
  {"EdenEast/nightfox.nvim"},
  {"RRethy/nvim-base16"},
  {"mcchrish/zenbones.nvim"},
  {"rktjmp/lush.nvim"},

  -- Organization
  {"vimwiki/vimwiki"},

  -- Languages
  --- Terraform
  {"hashivim/vim-terraform"},

  --- Nix
  {"LnL7/vim-nix"},
})

-- Settings
--- Airline
vim.g.airlene_left_sep = ""
vim.g.airlene_right_sep = ""
vim.g.airline_theme = "base16"

-- Keybindings
-- Nerdtree
vim.keymap.set("n", "<leader>op", ":NERDTreeToggle<CR>", { desc = "Toggle NERDTree" })

-- Vim Test
vim.keymap.set("n", "<leader>tt", ":TestNearest<CR>")
vim.keymap.set("n", "<leader>tT", ":TestFile<CR>")
vim.keymap.set("n", "<leader>ta", ":TestSuite<CR>")
vim.keymap.set("n", "<leader>tl", ":TestLast<CR>")
vim.keymap.set("n", "<leader>tg", ":TestVisit<CR>")

-- Copilot
vim.api.nvim_set_keymap('i', '<C-Space>', 'copilot#Accept("\\<CR>")', { silent = true, expr = true, script = true })
vim.g.copilot_no_tab_map = 1

-- Colorscheme
vim.g.nightfox_style = "nordfox"
vim.cmd("colorscheme nightfox")

-- LSP / Mason setup
require("lsp-config")
require("mason-setup")

-- Completion setup
require('cmp-config')

-- Treesitter settings
require("treesitter")
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

-- Diagostics settings
require('diagnostics')
