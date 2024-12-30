vim.g.airline_left_sep = ""
vim.g.airline_right_sep = ""
vim.g.airline_theme = "base16"

-- Diagnostic Settings
vim.fn.sign_define("DiagnosticSignError", { text = "", texthl = "DiagnosticSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = "", texthl = "DiagnosticSignWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "", texthl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = "", texthl = "DiagnosticSignHint" })

vim.diagnostic.config { float = { border = "rounded" } }

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
  -- Status line
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      -- TODO: This doesn't work, setting the background via nord theme override
      vim.cmd [[ hi lualine_c_normal guibg=NONE ]]

      local nord_theme_custom = require('lualine.themes.nord')
      nord_theme_custom.normal.c.bg = 'none'
      nord_theme_custom.inactive.c.bg = 'none'

      local mode_map = {
        ['NORMAL'] = 'N',
        ['O-PENDING'] = 'N?',
        ['INSERT'] = 'I',
        ['VISUAL'] = 'V',
        ['V-BLOCK'] = 'VB',
        ['V-LINE'] = 'VL',
        ['V-REPLACE'] = 'VR',
        ['REPLACE'] = 'R',
        ['COMMAND'] = '!',
        ['SHELL'] = 'SH',
        ['TERMINAL'] = 'T',
        ['EX'] = 'X',
        ['S-BLOCK'] = 'SB',
        ['S-LINE'] = 'SL',
        ['SELECT'] = 'S',
        ['CONFIRM'] = 'Y?',
        ['MORE'] = 'M',
      }

      require('lualine').setup({
        options = {
          theme = nord_theme_custom,
          section_separators = { left = ' ', right = ' ' },
          component_separators = { left = ' ', right = ' ' },
          globalstatus = false,
        },
        sections = {
          lualine_a = { },
          lualine_b = { },
          lualine_c = { },
          lualine_x = {
            'overseer',
            {
              'filename',
              path = 4,
              symbols = {
                modified = '',
                readonly = '',
                unnamed = '',
                newfile = '',
              },
            },
          },
          lualine_y = {
            'branch',
            {
              'diagnostics',

              sources = { 'nvim_lsp', 'nvim_diagnostic' },
              sections = { 'error', 'warn', 'info', 'hint' },

              diagnostics_color = {
                error = 'DiagnosticError', -- Changes diagnostics' error color.
                info  = 'DiagnosticInfo',  -- Changes diagnostics' info color.
                hint  = 'DiagnosticHint',  -- Changes diagnostics' hint color.
              },
              symbols = { error = '', warn = '', info = '', hint = '' },
              colored = true,          -- Displays diagnostics status in color if set to true.
              update_in_insert = true, -- Update diagnostics in insert mode.
              always_visible = false,   -- Show diagnostics even if there are none.
            },
          },
          lualine_z = {
            {
              'mode',
              fmt = function(mode)
                return mode_map[mode] or mode
              end,
            },
          },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = { 'filename' },
          lualine_z = {},
        },
        extensions = {
          'nvim-tree',
          'mason',
          'lazy',
        },
      })
    end,
  },

  -- Status line in Tmux
  {
    "vimpostor/vim-tpipeline",
    event = "VeryLazy",
  },

  -- Status Column
  --
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
      })
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
      vim.cmd("colorscheme nord")
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

  -- Messages, Cmdline & Popupmenu replacement
  { "folke/noice.nvim" },

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
