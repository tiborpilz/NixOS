-- Diagnostic Settings
vim.fn.sign_define("DiagnosticSignError", { text = "", texthl = "DiagnosticSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = "", texthl = "DiagnosticSignWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "", texthl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = "", texthl = "DiagnosticSignHint" })

-- vim.diagnostic.config { float = { border = "" } }

vim.g.tpipeline_autoembed = 0

return {
  -- Icons
  {
    'nvim-tree/nvim-web-devicons',
    config = function()
      require('nvim-web-devicons').setup({
        color_icons = true,
      })
    end,
  },
  -- Status Column
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup({
        signs = {
          add          = { text = '┃' },
          change       = { text = '┃' },
          delete       = { text = '_' },
          topdelete    = { text = '‾' },
          changedelete = { text = '~' },
          untracked    = { text = '┆' },
        },
        preview_config = {
          border = { " ", " ", " ", " ", " ", " ", " ", " " },
        },
      })

      vim.keymap.set("n", "<leader>gB", "<cmd>Gitsigns blame_line<cr>", { desc = "Blame Line" })
    end,
  },

  -- Zen Mode
  {
    "folke/zen-mode.nvim",
    cmd = "ZenMode",
    keys = {
      { "<leader>zz", "<cmd>ZenMode<cr>", desc = "Toggle Zen Mode" },
    },
    opts = {
      window = {
        width = 0.85, -- width will be 85% of the editor width
        options = {
          number = false,
          relativenumber = false,
          signcolumn = "no",
          cursorline = false,
          cursorcolumn = false,
          foldcolumn = "0",
          list = false,
        },
      },
      plugins = {
        gitsigns = { enabled = false },
        tmux = { enabled = false },
        diagnostics = { enabled = false }, -- disable diagnostics
        kitty = {
          enabled = false,
          font = "+2", -- font size increment
        },
      },
      on_open = function()
        -- Disable status line in zen mode
        vim.opt.laststatus = 0
      end,
      on_close = function()
        -- Re-enable status line when exiting zen mode
        vim.opt.laststatus = 2
      end,
    },
  },

  -- Colorschemes / Themes
  { "eddyekofo94/gruvbox-flat.nvim" },
  { "marko-cerovac/material.nvim" },
  { "kdheepak/monochrome.nvim" },
  { "EdenEast/nightfox.nvim" },
  { "RRethy/nvim-base16" },
  { "mcchrish/zenbones.nvim" },
  { "rktjmp/lush.nvim" },
  { "yorickpeterse/nvim-grey" },
  {
    "shaunsingh/nord.nvim",
    config = function()
      vim.g.nord_contrast = true

      require("nord").set()
    end,
  },
  {
    'AlexvZyl/nordic.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('nordic').load()
    end
  },
  --- Icons
  {
    'echasnovski/mini.icons',
    config = function()
      require('mini.icons').setup()
    end,
  },
  -- Screenkey
  {
    "NStefan002/screenkey.nvim",
    lazy = false,
    version = "*",
    config = function()
      require("screenkey").setup({
        -- Center
        win_opts = {
          width = 20,
          col = (vim.o.columns / 2) + 10,
          row = 1,
          border = "none",
          height = 1,
        },
      })
    end,
  },

}
