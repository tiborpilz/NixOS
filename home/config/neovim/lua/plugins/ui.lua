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
